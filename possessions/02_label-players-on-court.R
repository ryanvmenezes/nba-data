library(glue)
library(furrr)
library(tidyverse)

plan(multiprocess)

source('utils.R')

create.lineups.by.event = function(year, gameid, matchup) {
  pbp = read.pbp.v2(year, gameid)
  
  starters = read.starters(year, gameid)
  
  lineup.table = starters %>%
    left_join(
      matchup,
      by = 'team.abbreviation'
    ) %>% 
    group_by(period, side) %>% 
    mutate(
      side.id = str_c(side, row_number(), sep = '.')
    ) %>% 
    ungroup() %>% 
    select(period, side.id, player.id) %>%
    pivot_wider(names_from = 'side.id', values_from = 'player.id')
  
  sub.list = pbp %>% 
    filter(eventmsgtype == 8) %>% 
    mutate(
      sub.team = case_when(
        !is.na(homedescription) & is.na(visitordescription) ~ 'home',
        is.na(homedescription) & !is.na(visitordescription) ~ 'away',
      )
    ) %>% 
    select(eventorder, period, sub.team, sub.out = player1.id, sub.in = player2.id) %>% 
    right_join(
      pbp %>% select(eventorder, period),
      by = c("eventorder", "period")
    ) %>% 
    mutate(
      lineups = pmap(
        list(period, sub.team, sub.out, sub.in),
        function(this.event.period, sub.team, sub.out, sub.in) {
          if(!is.na(sub.team) & sub.team == 'home') {
            lineup.table <<- lineup.table %>% 
              mutate(
                home.1 = case_when(period == this.event.period & sub.out == home.1 ~ sub.in, TRUE ~ home.1),
                home.2 = case_when(period == this.event.period & sub.out == home.2 ~ sub.in, TRUE ~ home.2),
                home.3 = case_when(period == this.event.period & sub.out == home.3 ~ sub.in, TRUE ~ home.3),
                home.4 = case_when(period == this.event.period & sub.out == home.4 ~ sub.in, TRUE ~ home.4),
                home.5 = case_when(period == this.event.period & sub.out == home.5 ~ sub.in, TRUE ~ home.5),
              )
          }
          
          if(!is.na(sub.team) & sub.team == 'away') {
            lineup.table <<- lineup.table %>% 
              mutate(
                away.1 = case_when(period == this.event.period & sub.out == away.1 ~ sub.in, TRUE ~ away.1),
                away.2 = case_when(period == this.event.period & sub.out == away.2 ~ sub.in, TRUE ~ away.2),
                away.3 = case_when(period == this.event.period & sub.out == away.3 ~ sub.in, TRUE ~ away.3),
                away.4 = case_when(period == this.event.period & sub.out == away.4 ~ sub.in, TRUE ~ away.4),
                away.5 = case_when(period == this.event.period & sub.out == away.5 ~ sub.in, TRUE ~ away.5),
              )
          }
          
          return(lineup.table %>% filter(period == this.event.period) %>% select(-period))
        }
      )
    )
  
  lineups.changes = sub.list %>% 
    select(eventorder, period, lineups) %>% 
    unnest(c(lineups)) %>% 
    distinct(period, away.1, away.2, away.3, away.4, away.5, home.1, home.2, home.3, home.4, home.5, .keep_all = TRUE)

  return(lineups.changes)
}

write.lineups = function(year, gameid, matchup, overwrite = FALSE) {
  outfolder = glue('players-on-court/{year}/')
  if(!dir.exists(outfolder)) {
    dir.create(outfolder)
  }

  outpath = glue('{outfolder}{gameid}.csv')

  if(!file.exists(outpath) | overwrite) {
    data = create.lineups.by.event(year, gameid, matchup)
    write_csv(data, outpath)
  }
  
  return(NULL)
}

games = tibble(year = start.year:end.year) %>% 
  mutate(
    game.log = map(year, read.game.log),
    away.home = map(
      game.log,
      ~.x %>% 
        filter(str_detect(matchup, '@')) %>%
        select(game.id, matchup) %>%
        separate(matchup, into = c('away', 'home'), sep = ' @ ') %>%
        pivot_longer(-game.id, names_to = 'side', values_to = 'team.abbreviation')
    ),
    game.ids = map(
      away.home,
      ~.x %>% 
        distinct(game.id = game.id) %>%
        arrange(game.id)
    )
  ) %>% 
  select(-game.log) %>% 
  unnest(c(game.ids)) %>% 
  mutate(
    matchup = map2(away.home, game.id, ~.x %>% filter(game.id == .y)),
    overwrite = overwrite
  ) %>% 
  select(-away.home)

games

null.output = future_pmap(
  list(
    games$year,
    games$game.id,
    games$matchup,
    games$overwrite
  ),
  write.lineups,
  .progress = TRUE
)

beepr::beep()