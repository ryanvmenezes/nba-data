library(glue)
library(furrr)
library(tidyverse)

plan(multisession)

source('utils.R')

identify.period.starters = function(year, gameid) {
  pbp = read.pbp.v2(year, gameid)
  
  quarter.box.scores = 1:max(pbp$period) %>% 
    map(~read.quarter.box(year, gameid, .x)) %>% 
    bind_rows()
  
  full.subs = pbp %>% 
    filter(eventmsgtype == 8) %>% 
    select(period, eventnum, out = player1.id, IN = player2.id) %>% 
    pivot_longer(-period:-eventnum, names_to = 'sub', values_to = 'player.id') %>% 
    select(period, player.id, eventnum, sub)
  
  first.event.of.period = full.subs %>%
    group_by(period, player.id) %>% 
    filter(eventnum == min(eventnum))
  
  players.subbed.in = first.event.of.period %>% filter(sub == 'IN')

  starters.at.period = quarter.box.scores %>% 
    select(player.name, player.id, team.abbreviation, period) %>% 
    anti_join(players.subbed.in, by = c("player.id", "period"))
  
  return(starters.at.period)
}

get.and.write.starters = function(year, gameid, overwrite = FALSE) {
  outfolder = glue('starters-at-period/{year}/')
  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  outpath = glue('{outfolder}{gameid}.csv')
  
  if (!file.exists(outpath) | overwrite) {
    data = identify.period.starters(year, gameid)
    write_csv(data, outpath)
  }
  
  return(NULL)
}

game.ids = tibble(year = start.year:end.year) %>%
  mutate(
    game.ids = map(
      year,
      ~read.game.log(.x) %>%
        distinct(game.id) %>%
        arrange(game.id)
    )
  ) %>%
  unnest(c(game.ids)) %>%
  arrange(year, game.id)

game.ids

null.output = future_map2(
  game.ids$year,
  game.ids$game.id,
  get.and.write.starters,
  # ~get.and.write.starters(.x, .y, overwrite = TRUE),
  .progress = TRUE
)

beepr::beep()
