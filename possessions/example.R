library(tidyverse)
library(lubridate)

ex.data = read_csv('
screen_name,created_at
GovAndyBeshear,2020-04-01T20:57:21Z
GovAndyBeshear,2020-04-01T19:25:00Z
GovAndyBeshear,2020-04-01T18:33:00Z
GovAndyBeshear,2020-04-01T18:07:55Z
GovAndyBeshear,2020-04-01T11:56:20Z
GovAndyBeshear,2020-04-01T00:21:03Z
GovAndyBeshear,2020-04-01T00:19:00Z
GovAndyBeshear,2020-03-31T21:00:05Z
GovAndyBeshear,2020-03-31T18:53:14Z
GovAndyBeshear,2020-03-31T17:18:16Z
GovAndyBeshear,2020-03-31T13:55:00Z
GovAndyBeshear,2020-03-31T13:04:26Z
GovAndyBeshear,2020-03-31T00:41:41Z
GovAndyBeshear,2020-03-30T23:15:19Z
GovAndyBeshear,2020-03-30T21:54:57Z
GovAndyBeshear,2020-03-30T20:58:58Z
GovAndyBeshear,2020-03-30T18:56:32Z
GovChrisSununu,2020-04-01T18:24:43Z
GovChrisSununu,2020-04-01T15:50:26Z
GovChrisSununu,2020-03-31T12:43:45Z
GovChrisSununu,2020-03-30T19:16:46Z
GovChrisSununu,2020-03-30T18:51:24Z
GovChrisSununu,2020-03-30T17:07:32Z
GovChrisSununu,2020-03-30T15:16:14Z
GovChrisSununu,2020-03-30T14:57:12Z
GovChrisSununu,2020-03-30T13:59:57Z
GovChrisSununu,2020-03-30T12:38:16Z
GovChrisSununu,2020-03-29T13:21:00Z
GovChrisSununu,2020-03-28T13:04:54Z
GovChrisSununu,2020-03-28T01:11:27Z
GovChrisSununu,2020-03-27T20:49:15Z
GovChrisSununu,2020-03-27T20:37:30Z
SteveSisolak,2020-04-01T20:24:08Z
SteveSisolak,2020-03-31T00:01:59Z
SteveSisolak,2020-03-28T19:52:33Z
SteveSisolak,2020-03-28T02:16:37Z
SteveSisolak,2020-03-27T22:57:18Z
SteveSisolak,2020-03-26T21:54:36Z
SteveSisolak,2020-03-26T02:43:27Z
SteveSisolak,2020-03-25T18:47:26Z
SteveSisolak,2020-03-25T18:47:17Z
SteveSisolak,2020-03-25T18:47:15Z
SteveSisolak,2020-03-25T16:18:03Z
SteveSisolak,2020-03-25T16:17:46Z
SteveSisolak,2020-03-25T01:14:01Z
SteveSisolak,2020-03-24T22:57:44Z
SteveSisolak,2020-03-24T22:22:12Z
SteveSisolak,2020-03-24T17:33:40Z
SteveSisolak,2020-03-23T19:10:47Z
SteveSisolak,2020-03-22T23:55:15Z
SteveSisolak,2020-03-22T23:55:13Z
SteveSisolak,2020-03-22T22:30:16Z
SteveSisolak,2020-03-21T23:51:33Z
')

governors.by.day = ex.data %>% 
  mutate(created_at_floor = floor_date(created_at, 'days')) %>% 
  group_by(screen_name, created_at_floor) %>% 
  count()

governors.by.day

governors.by.day = governors.by.day %>%
  arrange(screen_name, created_at_floor) %>% 
  group_by(screen_name) %>% 
  mutate(cum.tweets = cumsum(n))

governors.by.day

governors.by.day %>% 
  select(-n) %>% 
  pivot_wider(names_from = 'screen_name', values_from = 'cum.tweets') %>% 
  arrange(created_at_floor)
