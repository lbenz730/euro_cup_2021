### Euro 2021 Simulations
library(tidyverse)
library(furrr)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multiprocess(workers = parallel::detectCores()-1))
source('helpers.R')

### Simulation Parameters
n_sims <- 10000
set.seed(12345)
run_date <- case_when(lubridate::hour(Sys.time()) <= 9 ~as.Date(Sys.Date()), 
                      T ~ as.Date(Sys.Date() + 1))

### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

### Read in Ratings and Schedule
df_ratings <- read_csv('predictions/ratings.csv')
schedule <- 
  read_csv('data/schedule.csv') %>% 
  mutate('date' = as.Date(date, '%m/%d/%y'))

### Expected Score for Each Game
schedule <- adorn_xg(schedule)

### Simulate Group Stage
df_group_stage <- filter(schedule, !is.na(group))
dfs_group_stage <- map(1:n_sims, ~df_group_stage)
group_stage_results <- future_map(dfs_group_stage, sim_group_stage)

### Knockout Round
knockout_brackets <- future_map(group_stage_results, build_knockout_bracket)

### R16
knockout_brackets <- 
  future_map(knockout_brackets, ~{
    schedule %>% 
      filter(str_detect(ko_round, 'R16')) %>% 
      mutate('team1' = .x$team1,
             'team2' = .x$team2) %>% 
      select(-lambda_1, -lambda_2) %>% 
      adorn_xg(.)
  })

r16_results <- future_map(knockout_brackets, sim_ko_round)

### QF
knockout_brackets <- 
  future_map(r16_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>% 
      filter(str_detect(ko_round, 'QF')) %>% 
      mutate('team1' = winners[c(1,3,5,7)],
             'team2' = winners[c(2,4,6,8)]) %>% 
      select(-lambda_1, -lambda_2) %>% 
      adorn_xg(.)
  })

qf_results <- future_map(knockout_brackets, sim_ko_round)

### SF
knockout_brackets <- 
  future_map(qf_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>% 
      filter(str_detect(ko_round, 'SF')) %>% 
      mutate('team1' = winners[c(1,3)],
             'team2' = winners[c(2,4)]) %>% 
      select(-lambda_1, -lambda_2) %>% 
      adorn_xg(.)
  })

sf_results <- future_map(knockout_brackets, sim_ko_round)

### Finals
knockout_brackets <- 
  future_map(sf_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>% 
      filter(str_detect(ko_round, 'FINAL')) %>% 
      mutate('team1' = winners[c(1)],
             'team2' = winners[c(2)]) %>% 
      select(-lambda_1, -lambda_2) %>% 
      adorn_xg(.)
  })

finals_results <- future_map(knockout_brackets, sim_ko_round)

### Aggregate Results
qf_teams <- bind_rows(qf_results) %>% pivot_longer(c('team1', 'team2')) %>% pull(value)
sf_teams <- bind_rows(sf_results) %>% pivot_longer(c('team1', 'team2')) %>% pull(value)
final_teams <- bind_rows(finals_results) %>% pivot_longer(c('team1', 'team2')) %>% pull(value)
winners <- bind_rows(finals_results) %>% mutate('champ' = ifelse(team1_score > team2_score, team1, team2)) %>% pull(champ)

df_stats <- 
  bind_rows(group_stage_results) %>% 
  group_by(team, group) %>% 
  summarise('mean_pts' = mean(points),
            'mean_gd' = mean(goal_diff),
            'r16' = mean(progress),
            'qf' = sum(team == qf_teams)/n_sims,
            'sf' = sum(team == sf_teams)/n_sims,
            'finals' = sum(team == final_teams)/n_sims,
            'champ' = sum(team == winners)/n_sims) %>% 
  ungroup()

### Save Results
write_csv(df_stats, 'predictions/sim_results.csv')

### Track History
history <- 
  read_csv('predictions/history.csv') %>% 
  filter(date != run_date) %>% 
  bind_rows(df_stats %>% mutate('date' = run_date)) %>% 
  arrange(date)
write_csv(history, 'predictions/history.csv')

