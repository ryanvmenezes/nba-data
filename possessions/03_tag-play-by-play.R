library(glue)
library(furrr)
library(tidyverse)

plan(multisession)

source('utils.R')

tag.game = function(year, gameid, game) {
  game = read.pbp.v2(year, gameid)

  tagged.game = game %>%
    mutate(
      homedescription = replace_na(homedescription, ''),
      neutraldescription = replace_na(neutraldescription, ''),
      visitordescription = replace_na(visitordescription, ''),
      time.elapsed = map2_dbl(
        period, pctimestring,
        function(qtr, clock) {
          clock.split = str_split(as.character(clock), pattern = ':')[[1]]
          minutes = as.integer(clock.split[1])
          seconds = minutes * 60 + as.integer(clock.split[2])
          elapsed.this.qtr = 720 - seconds
          elapsed.prev.qtrs = case_when(
            as.integer(qtr) <= 4 ~ (qtr - 1) * 720,
            TRUE ~ 720 * 4  + (qtr - 5) * 60 * 5
          )
          elapsed = elapsed.prev.qtrs + elapsed.this.qtr
          return (elapsed)
        }
      ),
      is.made.shot = eventmsgtype == 1,
      is.missed.shot = eventmsgtype == 2,
      is.free.throw = eventmsgtype == 3,
      is.rebound = eventmsgtype == 4,
      is.turnover = eventmsgtype == 5,
      is.foul = eventmsgtype == 6,
      is.violation = eventmsgtype == 7,
      is.substitution = eventmsgtype == 8,
      is.timeout = eventmsgtype == 9,
      is.jump.ball = eventmsgtype == 10,
      is.ejection = eventmsgtype == 11,
      is.start.of.period = eventmsgtype == 12,
      is.end.of.period = eventmsgtype == 13,
      is.miss = str_detect(str_to_lower(homedescription), 'miss') |
        str_detect(str_to_lower(visitordescription), 'miss'),
      is.shooting.foul = is.foul & eventmsgactiontype == 2,
      is.away.from.play.foul = is.foul & eventmsgactiontype == 6,
      is.inbound.foul = is.foul & eventmsgactiontype == 5,
      is.loose.ball.foul = is.foul & eventmsgactiontype == 3,
      is.team.rebound = case_when(
        is.rebound & eventmsgactiontype == 1 ~ TRUE,
        is.rebound &
          str_detect(str_to_lower(homedescription), 'rebound') &
          !str_detect(str_to_lower(homedescription), '\\(off:\\d+ def:\\d+\\)') ~ TRUE,
        is.rebound &
          str_detect(str_to_lower(visitordescription), 'rebound') &
          !str_detect(str_to_lower(visitordescription), '\\(off:\\d+ def:\\d+\\)') ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    mutate(
      is.defensive.rebound = case_when(
        !is.rebound ~ FALSE,
        TRUE ~ pmap_lgl(
          list(
            is.team.rebound, player1.id, player1.team.id,
            row_number(), list(.)
          ),
          function(
            is.team.rebound, player1.id, player1.team.id,
            rn, df
          ) {
            
            # search backwards for last miss
            last.miss = df %>%
              filter(row_number() < rn) %>% 
              arrange(-row_number()) %>% 
              filter(is.miss) %>% 
              head(1)
            
            if (is.team.rebound) {
              return (last.miss$player1.team.id[1] != player1.id)
            }
            
            return (last.miss$player1.team.id[1] != player1.team.id)
          }
        )
      )
    ) %>% 
    mutate(
      is.missed.free.throw = is.free.throw & is.miss,
      is.1.of.1 = is.free.throw & eventmsgactiontype == 10,
      is.2.of.2 = is.free.throw & eventmsgactiontype == 12,
      is.3.of.3 = is.free.throw & eventmsgactiontype == 15,
      is.technical = is.free.throw & eventmsgactiontype == 13,
      is.last.multi.free.throw = is.2.of.2 | is.3.of.3,
      is.last.free.throw = is.1.of.1 | is.last.multi.free.throw,
    ) %>% 
    mutate(
      is.last.free.throw.made = case_when(
        !is.free.throw ~ FALSE,
        TRUE ~ pmap_lgl(
          list(
            is.last.multi.free.throw, is.1.of.1, is.miss,
            row_number(), list(.)
          ),
          function(
            is.last.multi.free.throw, is.1.of.1, is.miss,
            rn, df
          ) {
            
            last.foul = df %>%
              filter(row_number() < rn) %>% 
              arrange(-row_number()) %>% 
              filter(is.foul) %>% 
              head(1)
            
            return (
              (is.last.multi.free.throw |
                 (is.1.of.1 &
                    !last.foul$is.away.from.play.foul[1] &
                    !last.foul$is.loose.ball.foul[1] &
                    !last.foul$is.inbound.foul[1])) &
                !is.miss
            )
          }
        )
      )
    ) %>% 
    mutate(
      is.and.1 = case_when(
        !is.made.shot ~ FALSE,
        TRUE ~ pmap_lgl(
          list(
            time.elapsed, player1.id,
            row_number(), list(.)
          ),
          function(
            te, shooter.player1.id,
            rn, df
          ) {
            
            next.3.seconds = df %>% 
              filter(time.elapsed >= te & time.elapsed < te + 3)
            
            has.shooting.foul = next.3.seconds %>% 
              filter(is.shooting.foul) %>% 
              nrow()
            
            has.1.of.1.with.same.shooter = next.3.seconds %>% 
              filter(is.1.of.1 & (player1.id == shooter.player1.id)) %>% 
              nrow()
            
            return (has.shooting.foul & has.1.of.1.with.same.shooter)
          }
        )
      )
    ) %>% 
    mutate(
      is.make.and.not.and.1 = is.made.shot & !is.and.1,
      is.three = str_detect(str_to_lower(homedescription), '3pt') |
        str_detect(str_to_lower(visitordescription), '3pt'),
      is.team.turnover = is.turnover & eventmsgactiontype %in% c(9, 10, 11, 44),
    ) %>% 
    mutate(
      is.end.of.possession = (
        is.turnover |
          is.last.free.throw.made |
          is.defensive.rebound |
          is.make.and.not.and.1 |
          is.end.of.period
      ),
      possession.end = case_when(
        is.turnover ~ 'turnover',
        is.last.free.throw.made ~ 'last.ft.made', 
        is.defensive.rebound ~ 'missed.shot',
        is.make.and.not.and.1 ~ 'made.shot',
        is.end.of.period ~ 'end.of.period',
        TRUE ~ NA_character_
      ),
      possession.id = cumsum(replace_na(lag(is.end.of.possession), FALSE))
    ) %>% 
    rename(player.1.id = player1.id) %>%
    select(-game.id, -wctimestring, -starts_with('person'), -starts_with('player1'), -starts_with('player2'), -starts_with('player3'), -starts_with('video')) %>% 
    select(1:8, is.end.of.possession, possession.end, possession.id, everything()) %>% 
    mutate_if(is.logical, ~case_when(. == FALSE ~ '', TRUE ~ 'TRUE'))
  
  return(tagged.game)
}

write.tagged.game = function(year, gameid, overwrite = FALSE) {
  outfolder = glue('tagged-pbp/{year}/')
  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }

  outpath = glue('{outfolder}{gameid}.csv')
  
  if (!file.exists(outpath) | overwrite) {
    data = tag.game(year, gameid)
    write_csv(data, outpath, na = '')
  }
  
  return(NULL)
}

end.year = 2020

game.ids = tibble(year = start.year:end.year) %>%
  mutate(
    game.ids = map(
      year,
      ~read.game.log(.x) %>%
        distinct(game.id) %>%
        arrange(game.id)
    )
  ) %>%
  unnest(c(game.ids)) %>%
  arrange(year, game.id)

game.ids

null.output = future_map2(
  game.ids$year,
  game.ids$game.id,
  write.tagged.game,
  .progress = TRUE
)

beepr::beep()