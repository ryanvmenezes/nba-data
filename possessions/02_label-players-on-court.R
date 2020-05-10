library(glue)
library(tidyverse)

suppress.read.csv = function(...) {
  suppressMessages(
    suppressWarnings(
      read_csv(...)
    )
  )
}

read.pbp = function(year, gameid) {
  suppress.read.csv(
    glue('../raw-data/playbyplayv2/{year}/{gameid}.csv'),
    col_types = cols(
      GAME_ID = col_character(),
      EVENTNUM = col_double(),
      EVENTMSGTYPE = col_double(),
      EVENTMSGACTIONTYPE = col_double(),
      PERIOD = col_double(),
      WCTIMESTRING = col_time(format = ""),
      PCTIMESTRING = col_time(format = ""),
      HOMEDESCRIPTION = col_character(),
      NEUTRALDESCRIPTION = col_logical(),
      VISITORDESCRIPTION = col_character(),
      SCORE = col_character(),
      SCOREMARGIN = col_character(),
      PERSON1TYPE = col_double(),
      PLAYER1_ID = col_double(),
      PLAYER1_NAME = col_character(),
      PLAYER1_TEAM_ID = col_double(),
      PLAYER1_TEAM_CITY = col_character(),
      PLAYER1_TEAM_NICKNAME = col_character(),
      PLAYER1_TEAM_ABBREVIATION = col_character(),
      PERSON2TYPE = col_double(),
      PLAYER2_ID = col_double(),
      PLAYER2_NAME = col_character(),
      PLAYER2_TEAM_ID = col_double(),
      PLAYER2_TEAM_CITY = col_character(),
      PLAYER2_TEAM_NICKNAME = col_character(),
      PLAYER2_TEAM_ABBREVIATION = col_character(),
      PERSON3TYPE = col_double(),
      PLAYER3_ID = col_double(),
      PLAYER3_NAME = col_character(),
      PLAYER3_TEAM_ID = col_double(),
      PLAYER3_TEAM_CITY = col_character(),
      PLAYER3_TEAM_NICKNAME = col_character(),
      PLAYER3_TEAM_ABBREVIATION = col_character(),
      VIDEO_AVAILABLE_FLAG = col_double()
    )
  ) 
}

read.starters = function(year, gameid) {
  suppress.read.csv(
    glue('starters-at-period/{year}/{gameid}.csv'),
    col_types = cols(
      PLAYER_NAME = col_character(),
      PLAYER_ID = col_double(),
      TEAM_ABBREVIATION = col_character(),
      PERIOD = col_double()
    )
  )
}

create.lineups.by.event = function(year, gameid, matchup) {
  pbp = read.pbp(year, gameid)
  
  starters = read.starters(year, gameid)
  
  lineup.table = starters %>%
    left_join(
      matchup,
      by = 'TEAM_ABBREVIATION'
    ) %>% 
    group_by(PERIOD, SIDE) %>% 
    mutate(
      SIDE_ID = str_c(SIDE, row_number(), sep = '_')
    ) %>% 
    ungroup() %>% 
    select(PERIOD, SIDE_ID, PLAYER_ID) %>%
    pivot_wider(names_from = 'SIDE_ID', values_from = 'PLAYER_ID')
  
  sub.list = pbp %>% 
    arrange(EVENTNUM) %>% 
    filter(EVENTMSGTYPE == 8) %>% 
    mutate(
      SUB_TEAM = case_when(
        !is.na(HOMEDESCRIPTION) & is.na(VISITORDESCRIPTION) ~ 'HOME',
        is.na(HOMEDESCRIPTION) & !is.na(VISITORDESCRIPTION) ~ 'AWAY',
      )
    ) %>% 
    select(EVENTNUM, PERIOD, SUB_TEAM, SUB_OUT = PLAYER1_ID, SUB_IN = PLAYER2_ID) %>% 
    right_join(
      pbp %>% select(EVENTNUM, PERIOD),
      by = c("EVENTNUM", "PERIOD")
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
  
  lineups.by.event = sub.list %>% 
    select(EVENTNUM, PERIOD, lineups) %>% 
    unnest(c(lineups)) %>% 
    distinct(PERIOD, AWAY_1, AWAY_2, AWAY_3, AWAY_4, AWAY_5, HOME_1, HOME_2, HOME_3, HOME_4, HOME_5, .keep_all = TRUE)
}

get.or.create.lineups = function(year, gameid, matchup) {
  outfolder = glue('players-on-court/{year}/')
  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }

  outpath = glue('{outfolder}{gameid}.csv')

  if (file.exists(outpath)) {
    data = suppress.read.csv(outpath)
  } else {
    data = create.lineups.by.event(year, gameid, matchup)
    write_csv(data, outpath)
  }
  
  # pb$tick()$print()
  
  return (data)
}

start.year = 2020
end.year = 2016

games = tibble(year = start.year:end.year) %>% 
  mutate(
    game.log = map(year, ~suppress.read.csv(glue('../raw-data/leaguegamelog/{.x}.csv'))),
    away.home = map(
      game.log,
      ~.x %>% 
        filter(str_detect(MATCHUP, '@')) %>%
        select(GAME_ID, MATCHUP) %>%
        separate(MATCHUP, into = c('AWAY', 'HOME'), sep = ' @ ') %>%
        pivot_longer(-GAME_ID, names_to = 'SIDE', values_to = 'TEAM_ABBREVIATION')
    ),
    game.ids = map(
      away.home,
      ~.x %>% 
        distinct(game.id = GAME_ID) %>%
        arrange(game.id)
    )
  ) %>% 
  select(-game.log) %>% 
  unnest(c(game.ids)) %>% 
  mutate(matchup = map2(away.home, game.id, ~.x %>% filter(GAME_ID == .y))) %>% 
  select(-away.home)

games

# pb = progress_estimated(nrow(games))

library(furrr)

availableCores()

plan(multiprocess)

future_pwalk(
  list(games$year, games$game.id, games$matchup),
  get.or.create.lineups,
  .progress = TRUE
)
