library(broom)
library(modelr)
library(tidyverse)

teams = read_csv('../raw-data/teams.csv')

teams

path = 'possessions/2020/'

poss.data = tibble(fpath = str_c(path, list.files(path))) %>% 
  mutate(
    game.id = str_replace(fpath, path, ''),
    game.id = str_replace(game.id, '.csv', ''),
    data = map(
      fpath,
      ~read_csv(
        .x,
        col_types = cols(
          .default = col_double(),
          how.possession.ended = col_character(),
          team.with.ball = col_character(),
          score = col_character(),
          scoremargin = col_character()
        )
      )
    )
  ) %>% 
  select(-fpath) %>% 
  unnest(c(data)) %>% 
  left_join(
    teams %>%
      select(team.with.ball.id = team.id, team.with.ball.name = team.name)
  ) %>% 
  left_join(
    teams %>%
      select(team.without.ball.id = team.id, team.without.ball.name = team.name)
  )

poss.data

with.ball.summary = poss.data %>%
  group_by(team.name = team.with.ball.name) %>% 
  summarise(
    ppp = sum(possession.points) / n(),
    possession.time = mean(possession.time)
  ) %>% 
  arrange(ppp)

with.ball.summary

without.ball.summary = poss.data %>%
  group_by(team.name = team.without.ball.name) %>% 
  summarise(
    ppp = sum(possession.points) / n(),
    possession.time = mean(possession.time)
  ) %>% 
  arrange(ppp)

without.ball.summary

ppp.summary = with.ball.summary %>% 
  inner_join(without.ball.summary, suffix = c('.off', '.def'), by = 'team.name') %>% 
  mutate(ppp.net.rating = ppp.off - ppp.def) %>% 
  arrange(-ppp.net.rating) %>% 
  drop_na()

ppp.summary

ppp.summary %>% 
  ggplot(aes(ppp.off, ppp.def)) +
  geom_text(aes(label = team.name)) +
  geom_abline(slope = 1, intercept = 0) +
  # scale_x_reverse() +
  # scale_y_reverse() +
  # geom_abline(slope = 1, intercept = 0) +
  # scale_x_continuous(limits = c(1.025, 1.175)) +
  # scale_y_continuous(limits = c(1.025, 1.175)) +
  theme_minimal()

poss.data.slim = poss.data %>% 
  select(team.with.ball.name, team.without.ball.name, possession.points, possession.time)

poss.data.slim

time.model = lm(possession.time ~ team.with.ball.name + team.without.ball.name, data = poss.data.slim)


summary(time.model)

tidy(time.model)
