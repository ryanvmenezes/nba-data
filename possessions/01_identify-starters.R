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

read.quarter.box = function(year, gameid, q) {
  suppress.read.csv(
    glue('../raw-data/boxscoretraditionalv2/{year}/{gameid}_q{q}.csv'),
    col_types = cols(
      GAME_ID = col_character(),
      TEAM_ID = col_double(),
      TEAM_ABBREVIATION = col_character(),
      TEAM_CITY = col_character(),
      PLAYER_ID = col_double(),
      PLAYER_NAME = col_character(),
      START_POSITION = col_character(),
      COMMENT = col_logical(),
      MIN = col_time(format = ""),
      FGM = col_double(),
      FGA = col_double(),
      FG_PCT = col_double(),
      FG3M = col_double(),
      FG3A = col_double(),
      FG3_PCT = col_double(),
      FTM = col_double(),
      FTA = col_double(),
      FT_PCT = col_double(),
      OREB = col_double(),
      DREB = col_double(),
      REB = col_double(),
      AST = col_double(),
      STL = col_double(),
      BLK = col_double(),
      TO = col_double(),
      PF = col_double(),
      PTS = col_double(),
      PLUS_MINUS = col_double()
    )
  ) %>%
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
  outfolder = glue('starters-at-period/{year}/')
  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  outpath = glue('{outfolder}{gameid}.csv')
  
  if (file.exists(outpath)) {
    data = suppress.read.csv(outpath)
  } else {
    data = identify.period.starters(year, gameid)
    write_csv(data, outpath)
  }
  
  pb$tick()$print()
  
  return (data)
}

start.year = 2020
end.year = 2016

game.ids = tibble(year = start.year:end.year) %>%
  mutate(
    game.ids = map(
      year,
      ~suppress.read.csv(glue('../raw-data/leaguegamelog/{.x}.csv')) %>%
        distinct(game.id = GAME_ID) %>%
        arrange(game.id)
    )
  ) %>%
  unnest(c(game.ids))

pb = progress_estimated(nrow(game.ids))

walk2(
  game.ids$year,
  game.ids$game.id,
  get.or.read.starters
)