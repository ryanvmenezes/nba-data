overwrite = TRUE

start.year = 2020
end.year = 2020

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
    col_types = readr::cols(
      GAME_ID = readr::col_character(),
      EVENTNUM = readr::col_double(),
      EVENTMSGTYPE = readr::col_double(),
      EVENTMSGACTIONTYPE = readr::col_double(),
      PERIOD = readr::col_double(),
      WCTIMESTRING = readr::col_time(format = ""),
      PCTIMESTRING = readr::col_time(format = ""),
      HOMEDESCRIPTION = readr::col_character(),
      NEUTRALDESCRIPTION = readr::col_logical(),
      VISITORDESCRIPTION = readr::col_character(),
      SCORE = readr::col_character(),
      SCOREMARGIN = readr::col_character(),
      PERSON1TYPE = readr::col_double(),
      PLAYER1_ID = readr::col_double(),
      PLAYER1_NAME = readr::col_character(),
      PLAYER1_TEAM_ID = readr::col_double(),
      PLAYER1_TEAM_CITY = readr::col_character(),
      PLAYER1_TEAM_NICKNAME = readr::col_character(),
      PLAYER1_TEAM_ABBREVIATION = readr::col_character(),
      PERSON2TYPE = readr::col_double(),
      PLAYER2_ID = readr::col_double(),
      PLAYER2_NAME = readr::col_character(),
      PLAYER2_TEAM_ID = readr::col_double(),
      PLAYER2_TEAM_CITY = readr::col_character(),
      PLAYER2_TEAM_NICKNAME = readr::col_character(),
      PLAYER2_TEAM_ABBREVIATION = readr::col_character(),
      PERSON3TYPE = readr::col_double(),
      PLAYER3_ID = readr::col_double(),
      PLAYER3_NAME = readr::col_character(),
      PLAYER3_TEAM_ID = readr::col_double(),
      PLAYER3_TEAM_CITY = readr::col_character(),
      PLAYER3_TEAM_NICKNAME = readr::col_character(),
      PLAYER3_TEAM_ABBREVIATION = readr::col_character(),
      VIDEO_AVAILABLE_FLAG = readr::col_double()
    )
  )
  df = dplyr::rename_all(df, stringr::str_to_lower)
  df = dplyr::rename_all(df, ~stringr::str_replace_all(., '_', '.'))
  df = dplyr::mutate(
    df,
    time.elapsed = purrr::map2_dbl(
      period, pctimestring,
      calculate.time.elapsed
    )
  )
  df = dplyr::arrange(df, time.elapsed, eventnum)
  df = dplyr::mutate(
    df,
    eventorder = 1:nrow(df)
  )
  return(df)
}

calculate.time.elapsed = function(qtr, clock) {
  clock.split = stringr::str_split(as.character(clock), pattern = ':')[[1]]
  minutes = as.integer(clock.split[1])
  seconds = minutes * 60 + as.integer(clock.split[2])
  elapsed.this.qtr = dplyr::case_when(
    as.integer(qtr) <= 4 ~ 60 * 12 - seconds,
    TRUE ~ 60 * 5 - seconds
  )
  elapsed.prev.qtrs = dplyr::case_when(
    as.integer(qtr) <= 4 ~ (qtr - 1) * 60 * 12,
    TRUE ~ 60 * 12 * 4  + (qtr - 5) * 60 * 5
  )
  elapsed = elapsed.prev.qtrs + elapsed.this.qtr
  return(elapsed)
}

read.quarter.box = function(year, gameid, q) {
  df = suppress.read.csv(
    glue::glue('../raw-data/boxscoretraditionalv2/{year}/{gameid}_q{q}.csv'),
    col_types = readr::cols(
      GAME_ID = readr::col_character(),
      TEAM_ID = readr::col_double(),
      TEAM_ABBREVIATION = readr::col_character(),
      TEAM_CITY = readr::col_character(),
      PLAYER_ID = readr::col_double(),
      PLAYER_NAME = readr::col_character(),
      START_POSITION = readr::col_character(),
      COMMENT = readr::col_logical(),
      MIN = readr::col_time(format = ""),
      FGM = readr::col_double(),
      FGA = readr::col_double(),
      FG_PCT = readr::col_double(),
      FG3M = readr::col_double(),
      FG3A = readr::col_double(),
      FG3_PCT = readr::col_double(),
      FTM = readr::col_double(),
      FTA = readr::col_double(),
      FT_PCT = readr::col_double(),
      OREB = readr::col_double(),
      DREB = readr::col_double(),
      REB = readr::col_double(),
      AST = readr::col_double(),
      STL = readr::col_double(),
      BLK = readr::col_double(),
      TO = readr::col_double(),
      PF = readr::col_double(),
      PTS = readr::col_double(),
      PLUS_MINUS = readr::col_double()
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
    col_types = readr::cols(
      SEASON_ID = readr::col_double(),
      TEAM_ID = readr::col_double(),
      TEAM_ABBREVIATION = readr::col_character(),
      TEAM_NAME = readr::col_character(),
      GAME_ID = readr::col_character(),
      GAME_DATE = readr::col_date(format = ""),
      MATCHUP = readr::col_character(),
      WL = readr::col_character(),
      MIN = readr::col_double(),
      FGM = readr::col_double(),
      FGA = readr::col_double(),
      FG_PCT = readr::col_double(),
      FG3M = readr::col_double(),
      FG3A = readr::col_double(),
      FG3_PCT = readr::col_double(),
      FTM = readr::col_double(),
      FTA = readr::col_double(),
      FT_PCT = readr::col_double(),
      OREB = readr::col_double(),
      DREB = readr::col_double(),
      REB = readr::col_double(),
      AST = readr::col_double(),
      STL = readr::col_double(),
      BLK = readr::col_double(),
      TOV = readr::col_double(),
      PF = readr::col_double(),
      PTS = readr::col_double(),
      PLUS_MINUS = readr::col_double(),
      VIDEO_AVAILABLE = readr::col_double()
    )
  )
  df = dplyr::rename_all(df, stringr::str_to_lower)
  df = dplyr::rename_all(df, ~stringr::str_replace_all(., '_', '.'))
  bad.ids = suppress.read.csv(
    glue::glue('../raw-data/bad-ids.csv'),
    col_types = readr::cols(game.id = readr::col_character())
  )
  df = anti_join(df, bad.ids, by = 'game.id')
  return(df)
}

read.starters = function(year, gameid) {
  df = suppress.read.csv(
    glue::glue('starters-at-period/{year}/{gameid}.csv'),
    col_types = readr::cols(
      PLAYER_NAME = readr::col_character(),
      PLAYER_ID = readr::col_double(),
      TEAM_ABBREVIATION = readr::col_character(),
      PERIOD = readr::col_double()
    )
  )
  return(df)
}

read.tagged = function(year, gameid) {
  df = suppress.read.csv(
    glue::glue('tagged-pbp/{year}/{gameid}.csv'),
    col_types = readr::cols(
      eventorder = readr::col_double(),
      eventnum = readr::col_double(),
      eventmsgtype = readr::col_double(),
      eventmsgactiontype = readr::col_double(),
      period = readr::col_double(),
      pctimestring = readr::col_time(format = ""),
      time.elapsed = readr::col_double(),
      is.end.of.possession = readr::col_logical(),
      possession.end = readr::col_character(),
      possession.id = readr::col_double(),
      homedescription = readr::col_character(),
      visitordescription = readr::col_character(),
      score = readr::col_character(),
      scoremargin = readr::col_character(),
      player.1.id = readr::col_double(),
      neutraldescription = readr::col_logical(),
      is.made.shot = readr::col_logical(),
      is.missed.shot = readr::col_logical(),
      is.free.throw = readr::col_logical(),
      is.rebound = readr::col_logical(),
      is.turnover = readr::col_logical(),
      is.foul = readr::col_logical(),
      is.violation = readr::col_logical(),
      is.substitution = readr::col_logical(),
      is.timeout = readr::col_logical(),
      is.jump.ball = readr::col_logical(),
      is.ejection = readr::col_logical(),
      is.start.of.period = readr::col_logical(),
      is.end.of.period = readr::col_logical(),
      is.miss = readr::col_logical(),
      is.shooting.foul = readr::col_logical(),
      is.away.from.play.foul = readr::col_logical(),
      is.inbound.foul = readr::col_logical(),
      is.loose.ball.foul = readr::col_logical(),
      is.team.rebound = readr::col_logical(),
      is.defensive.rebound = readr::col_logical(),
      is.missed.free.throw = readr::col_logical(),
      is.1.of.1 = readr::col_logical(),
      is.2.of.2 = readr::col_logical(),
      is.3.of.3 = readr::col_logical(),
      is.technical = readr::col_logical(),
      is.last.multi.free.throw = readr::col_logical(),
      is.last.free.throw = readr::col_logical(),
      is.last.free.throw.made = readr::col_logical(),
      is.and.1 = readr::col_logical(),
      is.make.and.not.and.1 = readr::col_logical(),
      is.three = readr::col_logical(),
      is.team.turnover = readr::col_logical()
    )
  )
  return(df)
}

read.on.court = function(year, gameid) {
  df = suppress.read.csv(
    glue::glue('players-on-court/{year}/{gameid}.csv'),
    col_types = readr::cols(
      eventorder = readr::col_double(),
      period = readr::col_double(),
      away.1 = readr::col_double(),
      away.2 = readr::col_double(),
      away.3 = readr::col_double(),
      away.4 = readr::col_double(),
      away.5 = readr::col_double(),
      home.1 = readr::col_double(),
      home.2 = readr::col_double(),
      home.3 = readr::col_double(),
      home.4 = readr::col_double(),
      home.5 = readr::col_double()
    )
  )
  return(df)
}