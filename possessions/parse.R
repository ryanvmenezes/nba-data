library(tidyverse)

GAME = read_csv('../play-by-play/playbyplayv2/2020/0021900002.csv')

GAME

game = GAME %>% 
  rename_all(str_to_lower) %>% 
  rename_all(~str_replace_all(., '_', '.')) %>% 
  mutate(
    homedescription = replace_na(homedescription, ''),
    neutraldescription = replace_na(neutraldescription, ''),
    visitordescription = replace_na(visitordescription, ''),
  )

game

names(game)

ncol(game)

game %>% 
  mutate(
    homedescription = replace_na(homedescription, ''),
    neutraldescription = replace_na(neutraldescription, ''),
    visitordescription = replace_na(visitordescription, ''),
    time.elapsed = map2_chr(
      period, pctimestring,
      function(qtr, clock) {
        clock.split = str_split(clock, pattern = ':')[[1]]
        minutes = as.integer(clock.split[1])
        seconds = minutes * 60 + as.integer(clock.split[2])
        elapsed.this.qtr = 720 - seconds
        elapsed = (as.integer(qtr) - 1) * 720 + elapsed.this.qtr
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
            (
              is.last.multi.free.throw |
                (is.1.of.1 & !last.foul$is.away.from.play.foul[1] & !last.foul$is.loose.ball.foul[1] & !last.foul$is.inbound.foul[1])
            ) &
              !is.miss
          )
        }
      )
    )
  ) %>% 
  # mutate(
  #   is.and.1 = case_when(
  #     !is.made.shot ~ FALSE,
  #     TRUE ~ pmap_lgl(
  #       
  #     )
  #   )
  # )
  # select(starts_with('event'), ends_with('description'), ends_with('rebound'))
  # head() %>%
  # pull(is.defensive.rebound)
  write_csv('tmp.csv')
