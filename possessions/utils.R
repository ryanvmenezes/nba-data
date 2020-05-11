suppress.read.csv = function(...) {
  suppressMessages(
    suppressWarnings(
      readr::read_csv(...)
    )
  )
}

read.pbp.v2 = function(year, gameid) {
  df = suppress.read.csv(
    glue::glue('../raw-data/playbyplayv2/{year}/{gameid}.csv'),
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
  df = dplyr::rename_all(df, stringr::str_to_lower)
  df = dplyr::rename_all(df, ~stringr::str_replace_all(., '_', '.'))
  df = dplyr::arrange(df, eventnum)
  return(df)
}

read.quarter.box = function(year, gameid, q) {
  df = suppress.read.csv(
    glue::glue('../raw-data/boxscoretraditionalv2/{year}/{gameid}_q{q}.csv'),
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
  )
  df = dplyr::rename_all(df, stringr::str_to_lower)
  df = dplyr::rename_all(df, ~stringr::str_replace_all(., '_', '.'))
  df = dplyr::mutate(df, period = q)
  return(df)
}

read.game.log = function(year) {
  df = suppress.read.csv(
    glue::glue('../raw-data/leaguegamelog/{year}.csv'),
    col_types = cols(
      SEASON_ID = col_double(),
      TEAM_ID = col_double(),
      TEAM_ABBREVIATION = col_character(),
      TEAM_NAME = col_character(),
      GAME_ID = col_character(),
      GAME_DATE = col_date(format = ""),
      MATCHUP = col_character(),
      WL = col_character(),
      MIN = col_double(),
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
      TOV = col_double(),
      PF = col_double(),
      PTS = col_double(),
      PLUS_MINUS = col_double(),
      VIDEO_AVAILABLE = col_double()
    )
  )
  df = dplyr::rename_all(df, stringr::str_to_lower)
  df = dplyr::rename_all(df, ~stringr::str_replace_all(., '_', '.'))
  bad.ids = suppress.read.csv(
    glue::glue('../raw-data/bad-ids.csv'),
    col_types = cols(game.id = col_character())
  )
  df = anti_join(df, bad.ids, by = 'game.id')
  return(df)
}

read.starters = function(year, gameid) {
  df = suppress.read.csv(
    glue::glue('starters-at-period/{year}/{gameid}.csv'),
    col_types = cols(
      PLAYER_NAME = col_character(),
      PLAYER_ID = col_double(),
      TEAM_ABBREVIATION = col_character(),
      PERIOD = col_double()
    )
  )
  return(df)
}