library(glue)
library(tidyverse)

suppress.read.csv = function(...) {
  suppressMessages(
    suppressWarnings(
      read_csv(...)
    )
  )
}

read.quarter.box = function(year, gameid, q) {
  suppress.read.csv(glue('../raw-data/boxscoretraditionalv2/{year}/{gameid}_q{q}.csv')) %>%
    mutate(PERIOD = q)
}

identify.period.starters = function(year, gameid) {
  pbp = suppress.read.csv(glue('../raw-data/playbyplayv2/{year}/{gameid}.csv'))
  
  pbp
  
  quarter.box.scores = 1:max(pbp$PERIOD) %>% 
    map(~read.quarter.box(year, gameid, .x)) %>% 
    bind_rows()
  
  full.subs = pbp %>% 
    filter(EVENTMSGTYPE == 8) %>% 
    select(PERIOD, EVENTNUM, OUT = PLAYER1_ID, IN = PLAYER2_ID) %>% 
    pivot_longer(-PERIOD:-EVENTNUM, names_to = 'SUB', values_to = 'PLAYER_ID') %>% 
    select(PERIOD, PLAYER_ID, EVENTNUM, SUB)
  
  full.subs
  
  first.event.of.period = full.subs %>%
    group_by(PERIOD, PLAYER_ID) %>% 
    filter(EVENTNUM == min(EVENTNUM))
  
  first.event.of.period
  
  players.subbed.in = first.event.of.period %>% filter(SUB == 'IN')
  
  players.subbed.in
  
  starters.at.period = quarter.box.scores %>% 
    select(PLAYER_NAME, PLAYER_ID, TEAM_ABBREVIATION, PERIOD) %>% 
    anti_join(players.subbed.in, by = c("PLAYER_ID", "PERIOD"))
  
  starters.at.period
}

get.or.read.starters = function(year, gameid) {
  if (!dir.exists('starters-at-period/{year}')) {
    dir.create('starters-at-period/{year}')
  }
  
  outpath = glue('starters-at-period/{year}/{gameid}.csv')
  if (file.exists(outpath)) {
    data = suppress.read.csv(outpath)
  } else {
    data = identify.period.starters(year, gameid)
    write_csv(data, outpath)
  }
  return (data)
}

YEAR = 2018

game.log = read_csv(
  glue('../raw-data/leaguegamelog/log-{YEAR}.csv'),
  col_types = cols(
    .default = col_double(),
    TEAM_ABBREVIATION = col_character(),
    TEAM_NAME = col_character(),
    GAME_ID = col_character(),
    GAME_DATE = col_date(format = ""),
    MATCHUP = col_character(),
    WL = col_character()
  )
)

gameids = game.log %>% distinct(GAME_ID) %>% arrange(GAME_ID)

starters = gameids %>% 
  mutate(starters = map(GAME_ID, ~get.or.read.starters(YEAR, .x)))
  