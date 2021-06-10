library(tidyverse)
library(lubridate)
library(rstan)
source('helpers.R')
options(mc.cores=parallel::detectCores())

### Read In International Soccer Scores
### We'll just use data from 2014 onwards
df_scores <- 
  read_csv('international_soccer_scores.csv') %>% 
  mutate('year' = year(date)) %>% 
  filter(year >= 2014)

### Filter out games for countries that don't play at least 20 games
keep <- 
  df_scores %>% 
  select(home_team, away_team) %>% 
  pivot_longer(c('home_team', 'away_team'),
               values_to = 'team') %>% 
  group_by(team) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 20) 

df_scores <- 
  df_scores %>% 
  semi_join(keep, by = c('home_team' = 'team')) %>% 
  semi_join(keep, by = c('away_team' = 'team'))

### Team IDs
team_ids <- team_codes(df_scores)

df_scores <- 
  select(df_scores, home_team, away_team, home_score, away_score, neutral, tournament) %>% 
  mutate('home_id' = team_ids[home_team],
         'away_id' = team_ids[away_team],
         'home_ind' = as.numeric(!neutral))

### Weights
df_scores <-
  df_scores %>%
  mutate('weight' = case_when(
    tournament == 'Friendly' ~ 1,
    str_detect(tournament, 'Nations League') ~ 4,
    str_detect(tournament, '(Gold Cup|African Cup of Nations|Copa América|Confederations|UEFA Euro)') ~ 8,
    str_detect(tournament, 'FIFA') ~ 10,
    T ~ 1) ) %>% 
  mutate('weight' = pmax(1, weight * ifelse(str_detect(tournament, 'qualification'), 0.5, 1), 1))

### List of Stan Params
stan_data <- list(
  num_clubs = length(team_ids),
  num_games = nrow(df_scores),
  home_team_code = df_scores$home_id,
  away_team_code = df_scores$away_id,
  
  h_goals = df_scores$home_score,
  a_goals = df_scores$away_score,
  ind_home = df_scores$home_ind,
  
  weights = df_scores$weight
)

### Fit Model
model <- stan(file = 'stan/bvp_goals_no_corr.stan', 
              data = stan_data, 
              seed = 73097,
              chains = 3, 
              iter = 5000, 
              warmup = 2000, 
              control = list(adapt_delta = 0.95))
write_rds(model, 'model_objects/model.rds')

### Posterior Draws
posterior <- extract(model)
write_rds(posterior, 'model_objects/posterior.rds')

### Team Ratings
df_ratings <- 
  tibble('team' = names(team_ids),
         'team_id' = team_ids,
         'alpha' = apply(posterior$alpha, 2, mean),
         'delta' = apply(posterior$delta, 2, mean)) %>% 
  mutate('net_rating' = alpha + abs(delta)) %>% 
  arrange(desc(net_rating))

write_csv(df_ratings, 'ratings.csv')
