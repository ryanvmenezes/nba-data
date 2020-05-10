library(glue)
library(tidyverse)

suppress.read.csv = function(...) {
  suppressMessages(
    suppressWarnings(
      read_csv(...)
    )
  )
}

create.lineups.by.event = function(year, gameid) {
  pbp = suppress.read.csv(glue('../raw-data/playbyplayv2/{YEAR}/{gameid}.csv'))
  pbp
  
  starters = suppress.read.csv(glue('starters-at-period/{YEAR}/{gameid}.csv'))
  starters
  
  lineup.table = starters %>%
    left_join(
      away.home %>% 
        filter(GAME_ID == gameid),
      by = 'TEAM_ABBREVIATION'
    ) %>% 
    group_by(PERIOD, SIDE) %>% 
    mutate(
      SIDE_ID = str_c(SIDE, row_number(), sep = '_')
    ) %>% 
    ungroup() %>% 
    select(PERIOD, SIDE_ID, PLAYER_ID) %>%
    pivot_wider(names_from = 'SIDE_ID', values_from = 'PLAYER_ID')
  
  lineup.table
  
  sub.list = pbp %>% 
    arrange(EVENTNUM) %>% 
    filter(EVENTMSGTYPE == 8) %>% 
    mutate(
      SUB_TEAM = case_when(
        !is.na(HOMEDESCRIPTION) & is.na(VISITORDESCRIPTION) ~ 'HOME',
        is.na(HOMEDESCRIPTION) & !is.na(VISITORDESCRIPTION) ~ 'AWAY',
      )
    ) %>% 
    select(GAME_ID, EVENTNUM, PERIOD, SUB_TEAM, SUB_OUT = PLAYER1_ID, SUB_IN = PLAYER2_ID) %>% 
    right_join(
      pbp %>% 
        select(GAME_ID, EVENTNUM, PERIOD),
      by = c("GAME_ID", "EVENTNUM", "PERIOD")
    ) %>% 
    mutate(
      lineups = pmap(
        list(PERIOD, SUB_TEAM, SUB_OUT, SUB_IN),
        function(period, sub.team, sub.out, sub.in) {
          if (!is.na(sub.team) & sub.team == 'HOME') {
            lineup.table <<- lineup.table %>% 
              mutate(
                HOME_1 = case_when(PERIOD == period & sub.out == HOME_1 ~ sub.in, TRUE ~ HOME_1),
                HOME_2 = case_when(PERIOD == period & sub.out == HOME_2 ~ sub.in, TRUE ~ HOME_2),
                HOME_3 = case_when(PERIOD == period & sub.out == HOME_3 ~ sub.in, TRUE ~ HOME_3),
                HOME_4 = case_when(PERIOD == period & sub.out == HOME_4 ~ sub.in, TRUE ~ HOME_4),
                HOME_5 = case_when(PERIOD == period & sub.out == HOME_5 ~ sub.in, TRUE ~ HOME_5),
              )
          }
          
          if (!is.na(sub.team) & sub.team == 'AWAY') {
            lineup.table <<- lineup.table %>% 
              mutate(
                AWAY_1 = case_when(PERIOD == period & sub.out == AWAY_1 ~ sub.in, TRUE ~ AWAY_1),
                AWAY_2 = case_when(PERIOD == period & sub.out == AWAY_2 ~ sub.in, TRUE ~ AWAY_2),
                AWAY_3 = case_when(PERIOD == period & sub.out == AWAY_3 ~ sub.in, TRUE ~ AWAY_3),
                AWAY_4 = case_when(PERIOD == period & sub.out == AWAY_4 ~ sub.in, TRUE ~ AWAY_4),
                AWAY_5 = case_when(PERIOD == period & sub.out == AWAY_5 ~ sub.in, TRUE ~ AWAY_5),
              )
          }
          
          return (lineup.table %>% filter(PERIOD == period) %>% select(-PERIOD))
        }
      )
    )
  
  sub.list
  
  lineups.by.event = sub.list %>% 
    select(GAME_ID, EVENTNUM, PERIOD, lineups) %>% 
    unnest(c(lineups))
  
  lineups.by.event
}

get.or.create.lineups = function(year, gameid) {
  outpath = glue('players-on-court/{year}/{gameid}.csv')
  if (file.exists(outpath)) {
    data = suppress.read.csv(outpath)
  } else {
    data = create.lineups.by.event(year, gameid)
    write_csv(data, outpath)
  }
  return (data)
}

YEAR = 2020

game.log = suppress.read.csv(glue('../raw-data/leaguegamelog/log-{YEAR}.csv'))
 
away.home = gamelog %>%
  filter(str_detect(MATCHUP, '@')) %>%
  select(GAME_ID, MATCHUP) %>%
  separate(MATCHUP, into = c('AWAY', 'HOME'), sep = ' @ ') %>%
  pivot_longer(-GAME_ID, names_to = 'SIDE', values_to = 'TEAM_ABBREVIATION')

away.home

gameids = away.home %>% distinct(GAME_ID) %>% arrange(GAME_ID)

all.lineups = gameids %>% 
  mutate(lineups = map(GAME_ID, ~get.or.create.lineups(YEAR, .x)))

all.lineups
