library(glue)
library(furrr)
library(tidyverse)

plan(multiprocess)

source('utils.R')

year = 2020
gameid = '0021900001'

separate.possessions = function(year, gameid, awayteamid, hometeamid) {
  tagged = read.tagged(year, gameid)

  on.court = read.on.court(year, gameid)
  
  joined = tagged %>% 
    select(-neutraldescription) %>%
    mutate_if(is.logical, ~replace_na(., FALSE)) %>% 
    left_join(on.court, by = c('eventorder', 'period')) %>% 
    mutate(
      away.team.id = awayteamid,
      home.team.id = hometeamid,
    ) %>% 
    fill(starts_with('away.'), starts_with('home.'), starts_with('score'), .direction = 'down') %>% 
    mutate(
      team.at.possession.end = case_when(
        is.na(possession.end) ~ NA_character_,
        possession.end %in% c('last.ft.made', 'made.shot') ~ case_when(
          player.1.id %in% c(away.1, away.2, away.3, away.4, away.5) ~ 'away',
          player.1.id %in% c(home.1, home.2, home.3, home.4, home.5) ~ 'home',
        ),
        possession.end == 'missed.shot' ~ case_when(
          player.1.id %in% c(away.1, away.2, away.3, away.4, away.5, away.team.id) ~ 'home',
          player.1.id %in% c(home.1, home.2, home.3, home.4, home.5, home.team.id) ~ 'away',
        ),
        possession.end == 'turnover' ~ case_when(
          player.1.id %in% c(away.1, away.2, away.3, away.4, away.5, away.team.id) ~ 'away',
          player.1.id %in% c(home.1, home.2, home.3, home.4, home.5, home.team.id) ~ 'home',
        )
      ),
    )
  
  possession.summary = joined %>% 
    arrange(eventorder) %>% 
    group_by(possession.id) %>% 
    summarise(
      period = first(period),
      how.possession.ended = last(possession.end),
      team.with.ball = last(team.at.possession.end),
      score = last(score),
      scoremargin = last(scoremargin),
      end.time = max(time.elapsed),
      attempts.2p = sum((is.made.shot | is.missed.shot) & !is.three),
      makes.2p = sum(is.made.shot & !is.three),
      attempts.3p = sum((is.made.shot | is.missed.shot) & is.three),
      makes.3p = sum(is.made.shot & is.three),
      attempts.ft = sum(is.free.throw),
      makes.ft = sum(is.free.throw & !is.miss),
      possession.points = makes.ft + 2 * makes.2p + 3 * makes.3p,
      defensive.fouls = sum(is.foul & eventmsgtype != 4),
      offensive.rebounds = sum(is.rebound & !is.defensive.rebound),
      # add players on court
      has.substitution = any(eventmsgtype == 8),
      away.1 = first(away.1),
      away.2 = first(away.2),
      away.3 = first(away.3),
      away.4 = first(away.4),
      away.5 = first(away.5),
      home.1 = first(home.1),
      home.2 = first(home.2),
      home.3 = first(home.3),
      home.4 = first(home.4),
      home.5 = first(home.5),
    ) %>% 
    ungroup() %>% 
    mutate(
      team.with.ball = case_when(
        how.possession.ended == 'end.of.period' ~ if_else(lag(team.with.ball) == 'home', 'away', 'home'),
        TRUE ~ team.with.ball
      )
    ) %>% 
    arrange(possession.id) %>% 
    mutate(
      start.time = lag(end.time, default = 0),
      poss.time = end.time - start.time,
      team.with.ball.id = case_when(
        team.with.ball == 'home' ~ hometeamid,
        team.with.ball == 'away' ~ awayteamid,
      )
    ) %>% 
    select(
      possession.id, period, how.possession.ended, team.with.ball, team.with.ball.id,
      possession.points, poss.time, start.time, end.time,
      score, scoremargin,
      attempts.2p, makes.2p, attempts.3p, makes.3p, attempts.ft, makes.ft,
      defensive.fouls, offensive.rebounds,
      away.1, away.2, away.3, away.4, away.5, home.1, home.2, home.3, home.4, home.5
    )
    
  return(possession.summary)
}

write.separated.possessions = function(year, gameid, awayteamid, hometeamid, overwrite = FALSE) {
  outfolder = glue('possessions/{year}/')
  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  outpath = glue('{outfolder}{gameid}.csv')
  
  if (!file.exists(outpath) | overwrite) {
    data = separate.possessions(year, gameid, awayteamid, hometeamid)
    write_csv(data, outpath, na = '')
  }
  
  return(NULL)
}

game.ids = tibble(year = start.year:end.year) %>%
  mutate(
    game.ids = map(
      year,
      ~read.game.log(.x) %>%
        mutate(home.away = if_else(str_detect(matchup, '@'), 'away.team.id', 'home.team.id')) %>% 
        select(game.id, team.id, home.away) %>% 
        pivot_wider(names_from = 'home.away', values_from = 'team.id') %>% 
        arrange(game.id)
    )
  ) %>%
  unnest(c(game.ids)) %>%
  arrange(year, game.id) %>% 
  mutate(overwrite = overwrite)

game.ids

null.output = future_pmap(
  list(
    game.ids$year,
    game.ids$game.id,
    game.ids$away.team.id,
    game.ids$home.team.id,
    game.ids$overwrite
  ),
  write.separated.possessions,
  .progress = TRUE
)

beepr::beep()