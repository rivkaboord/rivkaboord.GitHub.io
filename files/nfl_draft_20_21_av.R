# Scrape draft data from Pro Football Reference

library(rvest)
library(tidyverse)
library(janitor)

read_pfr_draft <- function(year) {
    
    read_html(paste0("https://www.pro-football-reference.com/years/", year, "/draft.htm")) %>% 
        html_node("table") %>% 
        html_table(fill = T, header = T) %>% 
        as.data.frame() %>% 
        row_to_names(1) %>% 
        clean_names() %>% 
        rename(pass_cmp = cmp,
               pass_att = att,
               pass_yds = yds,
               pass_td = td,
               pass_int = int,
               rush_att = att_2,
               rush_yds = yds_2,
               rush_td = td_2,
               rec_yds = yds_3,
               rec_td = td_3,
               solo_tkl = solo,
               def_int = int_2,
               from = 'x') %>% 
        filter(g != 'G') %>% 
        select(-from, -to) %>% 
        mutate(draft_year = {{year}})
}

# draft_21 <- read_html("https://www.pro-football-reference.com/years/2021/draft.htm") %>% 
#     html_elements("table") %>% html_table(fill = T, header = T) %>% 
#     as.data.frame() %>% row_to_names(1) %>% clean_names() %>% 
#     rename(pass_cmp = cmp, pass_att = att, pass_yds = yds, pass_td = td, pass_int = int, rush_att = att_2, rush_yds = yds_2,
#   rush_td = td_2, rec_yds = yds_3, rec_td = td_3, solo_tkl = solo, def_int = int_2, from = 'x') %>% 
#   filter(g != 'G') %>% 
#     select(-from, -to) %>% 
#     mutate(draft_year = 2021)

# draft_20_21 <- 
#     draft_20 %>% 
#     left_join(draft_21)

library(ggtext)
library(nflverse)
library(nflfastR)

draft_00_21 <- map_df(2000:2021, read_pfr_draft) %>% 
    mutate(
        tm = tm %>% 
            str_replace_all('SDG', 'LAC') %>% 
            str_replace_all('OAK', 'LVR') %>% 
            str_replace_all('STL', 'LAR')
           ) 


draft_20_21 <- map_df(2020:2021, read_pfr_draft) %>% 
    mutate(
        tm = tm %>% 
            str_replace_all('SDG', 'LAC') %>% 
            str_replace_all('OAK', 'LV') %>% 
            str_replace_all('LVR', 'LV') %>% 
            str_replace_all('STL', 'LA') %>% 
            str_replace_all('GNB', 'GB') %>% 
            str_replace_all('SFO', 'SF') %>% 
            str_replace_all('KAN', 'KC') %>% 
            str_replace_all('NOR', 'NO') %>% 
            str_replace_all('TAM', 'TB') %>% 
            str_replace_all('NWE', 'NE')
    )

draft_20_21 %>% 
    group_by(tm) %>% 
    mutate(w_av = !is.na(w_av)) %>% 
    summarize(total_w_av = sum(w_av)) %>% 
    arrange(desc(total_w_av)) %>% 
    left_join(teams_colors_logos, by = c("tm" = "team_abbr")) %>% 

    ggplot(aes(tm, total_w_av)) +
    geom_point(aes(fill = team_color, color = team_color2), size = 5,
             stat = "identity", alpha = 0.8) +
    
    theme_bw() +
    scale_color_identity(aesthetics = c("fill", "color")) +
    
    labs(title = 'Team Draft Weighted Approximate Value, 2020-21',
         subtitle = 'Data via Pro Football Reference to approximate the value of draft picks in the NFL',
         x = 'Team',
         y = 'Total Weighted Approximate Value (wAV)'
         ) +
    
    theme(legend.position = "none",
          plot.title = element_text(face = 'bold', size = 18, hjust = 0.5),
          plot.subtitle = element_text(face = 'italic', size = 14, hjust = 0.5),
          axis.title.x = element_text(face = 'bold', size = 14),
          axis.title.y = element_text(face = 'bold', size = 14)
          )

# 