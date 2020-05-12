library(glue)
library(tidyverse)

source('utils.R')

year = 2020
gameid = '0021900001'

pbp = read.pbp.v2(year, gameid)

tagged = read_csv(glue('tagged-pbp/{year}/{gameid}.csv'))

tagged

game.log = read.game.log(year)

game.team.ids = game.log %>% 
  mutate(home.away = if_else(str_detect(matchup, '@'), 'away.team.id', 'home.team.id')) %>% 
  select(game.id, team.id, home.away) %>% 
  pivot_wider(names_from = 'home.away', values_from = 'team.id')

game.team.ids

this.game.team.ids = game.team.ids %>% 
  filter(game.id == gameid)

this.game.team.ids

on.court = read_csv(glue('players-on-court/{year}/{gameid}.csv'))

on.court

joined = tagged %>% 
  select(-neutraldescription) %>%
  mutate_if(is.logical, ~replace_na(., FALSE)) %>% 
  left_join(on.court, by = c('eventnum', 'period')) %>% 
  mutate(
    away.team.id = this.game.team.ids$away.team.id,
    home.team.id = this.game.team.ids$home.team.id,
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

joined

joined %>% count(possession.end)

# joined %>% 
#   filter(!is.na(possession.end)) %>% 
#   select(period, possession.id, possession.end, team.at.possession.end) %>% 
#   view()

possession.summary = joined %>% 
  group_by(possession.id) %>% 
  summarise(
    period = first(period),
    how.possession.ended = last(possession.end),
    team.with.ball = last(team.at.possession.end),
    # score = last(score),
    # scoremargin = last(scoremargin),
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
      team.with.ball == 'home' ~ this.game.team.ids$home.team.id,
      team.with.ball == 'away' ~ this.game.team.ids$away.team.id,
    )
  ) %>% 
  select(
    possession.id, period, how.possession.ended, team.with.ball, team.with.ball.id,
    possession.points, poss.time, start.time, end.time,
    attempts.2p, makes.2p, attempts.3p, makes.3p, attempts.ft, makes.ft,
    defensive.fouls, offensive.rebounds,
  ) %>% 
  mutate(
    alternates = team.with.ball != lag(team.with.ball)
  )
  
  
possession.summary  

possession.summary %>% 
  group_by(team.with.ball) %>%
  summarise(sum(possession.points))

possession.summary %>% 
  write_csv('tmp.csv', na = '')
