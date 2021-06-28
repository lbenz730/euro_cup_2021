library(tidyverse)
library(gt)

df_stats <- 
  read_csv('predictions/ratings.csv') %>% select(team, alpha, delta, net_rating) %>% 
  inner_join(  read_csv('predictions/sim_results.csv')) %>% 
  arrange(desc(champ), desc(finals),
          desc(sf), desc(qf), desc(r16)) %>% 
  mutate('logo' = paste0('flags/', team, '.png')) %>% 
  select(team, logo, group, everything()) 

make_table <- function(Group = 'all') {
  if(Group == 'all') {
    df <- df_stats
    subtitle <- ''
  } else {
    df <- 
      df_stats %>% 
      filter(group == Group) %>% 
      arrange(desc(r16))
    subtitle <- paste('Group', Group)
  }
  
  df %>% 
    gt() %>% 
    
    ### Round Numbers
    fmt_number(columns = vars(alpha, delta, net_rating, mean_pts, mean_gd), decimals = 2, sep_mark = '') %>% 
    fmt_percent(columns = vars(r16, qf, sf, finals, champ), decimals = 0, sep_mark = '') %>% 
    
    ### Align Columns
    cols_align(align = "center", columns = T) %>% 
    
    ### Colors
    data_color(columns = vars(mean_pts),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>% 
    data_color(columns = vars(mean_gd),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>% 
    data_color(columns = vars(r16, qf, sf, finals, champ),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
    data_color(columns = vars(alpha),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
    data_color(columns = vars(net_rating),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
    data_color(columns = vars(delta),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
    
    ### Borders
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_column_labels(
          columns = gt::everything()
        )
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_body(
          columns = vars(group, net_rating, mean_gd)
        )
      )
    ) %>% 
    
    tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>% 
    tab_spanner(label = 'Group Stage', columns = c('mean_pts', 'mean_gd')) %>% 
    tab_spanner(label = 'Knockout Round', columns = c('r16', 'qf', 'sf',  'finals', 'champ')) %>% 
    
    ### Logos
    text_transform(
      locations = cells_body(columns = "logo"), 
      fn = function(x) map_chr(x, ~{
        local_image(filename =  as.character(.x), height = 30)
      })
    ) %>% 
    
    ### Names
    cols_label(
      team = '',
      logo = '',
      group = 'Group',
      alpha = 'Offense',
      delta = 'Defense',
      net_rating = 'Overall',
      mean_pts = 'Mean Points',
      mean_gd = 'Mean Goal Diff',
      r16 = 'R16',
      qf = 'QF',
      sf = 'SF',
      finals = 'Finals',
      champ = 'Champ'
      
    ) %>% 
    tab_source_note("Luke Benz (@recspecs730)") %>%
    tab_source_note("Ratings = Change in Log Goal Expectations") %>%
    tab_source_note("Based on 10,000 Simulations") %>%
    tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
    tab_header(
      title = 'Euro Cup 2021',
      subtitle = subtitle
    ) %>% 
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold'
    )
}




round <- 'r16'
rounds <- names(df_stats)[9:which(names(df_stats) == round)]

df <- 
  df_stats[df_stats[[round]] > 0, ] %>% 
  select(-mean_pts, -mean_gd, -any_of(rounds))

table <- make_table('all')
gtsave(table, filename = 'figures/euro_2021.png')
map(LETTERS[1:6], ~gtsave(make_table(Group = .x), filename = paste0('figures/', .x, '.png')))




ko_table <-
df %>% 
  gt() %>% 
  
  ### Round Numbers
  fmt_number(columns = vars(alpha, delta, net_rating), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = vars(qf, sf, finals, champ), decimals = 0, sep_mark = '') %>% 
  
  ### Align Columns
  cols_align(align = "center", columns = T) %>% 
  
  data_color(columns = vars(qf, sf, finals, champ),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  data_color(columns = vars(alpha),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
  data_color(columns = vars(net_rating),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
  data_color(columns = vars(delta),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
  
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(group, net_rating)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>% 
  tab_spanner(label = 'Knockout Round', columns = c('qf', 'sf',  'finals', 'champ')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    team = '',
    logo = '',
    group = 'Group',
    alpha = 'Offense',
    delta = 'Defense',
    net_rating = 'Overall',
    # r16 = 'R16',
    qf = 'QF',
    sf = 'SF',
    finals = 'Finals',
    champ = 'Champ'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_source_note("Ratings = Change in Log Goal Expectations") %>%
  tab_source_note("Based on 10,000 Simulations") %>%
  tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
  tab_header(
    title = 'Euro Cup 2021',
    subtitle = 'Knockout Round'
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )

gtsave(ko_table, filename = 'figures/knockout_euro_2021.png')
