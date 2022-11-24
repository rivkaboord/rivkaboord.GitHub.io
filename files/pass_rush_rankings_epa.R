library(tidyverse)
library(nflfastR)
library(gt)
library(ggimage)

# EPA and attempt ranks for run and pass ----
pbp <- load_pbp(2021)

rush_attempts_by_team <- pbp %>% 
    filter(play_type == 'run') %>% 
    group_by(posteam) %>% 
    count(name = 'total_rush_attempts') %>% 
    arrange(desc(total_rush_attempts)) %>% 
    rowid_to_column(var = 'rush_attempts_rank') %>% 
    select(posteam, everything()) %>% 
    ungroup()

run_epa <- pbp %>% 
    filter(play_type == 'run') %>% 
    group_by(posteam) %>% 
    summarize(rush_epa_per_play = mean(epa)) %>%
    arrange(desc(rush_epa_per_play)) %>% 
    rowid_to_column(var = 'rush_epa_rank') %>% 
    select(posteam, everything()) %>% 
    ungroup()

rush_attempts_by_team %>% 
    left_join(run_epa, by = c('posteam'))

pass_attempts_by_team <- pbp %>% 
    filter(play_type == 'pass') %>% 
    group_by(posteam) %>% 
    count(name = 'total_pass_attempts') %>% 
    arrange(desc(total_pass_attempts)) %>% 
    rowid_to_column(var = 'pass_attempts_rank') %>% 
    select(posteam, everything()) %>% 
    ungroup()

pass_epa <- pbp %>% 
    filter(play_type == 'pass') %>% 
    group_by(posteam) %>% 
    summarize(pass_epa_per_play = mean(epa)) %>%
    arrange(desc(pass_epa_per_play)) %>% 
    rowid_to_column(var = 'pass_epa_rank') %>% 
    select(posteam, everything()) %>% 
    ungroup()

pass_attempts_by_team %>% 
    left_join(pass_epa, by = c('posteam'))

rush_pass_epa_ranks <- rush_attempts_by_team %>% 
    left_join(run_epa, by = c('posteam')) %>% 
    left_join(pass_attempts_by_team, by = c('posteam')) %>% 
    left_join(pass_epa, by = c('posteam'))

rush_pass_epa_ranks

# overall offensive EPA ----

overall_offensive_epa <- pbp %>% 
    group_by(posteam) %>% 
    summarize(offensive_epa = mean(epa)) %>% 
    rowid_to_column(var = 'offensive_epa_rank')

rush_pass_overall_epa_ranks <- rush_pass_epa_ranks %>% 
    left_join(overall_offensive_epa, by = c('posteam'))

# win-loss records ----

home <- nflfastR::fast_scraper_schedules(2021) %>%
    filter(game_type == 'REG') %>% 
    select(game_id, season, week, home_team, result) %>% 
    rename(team = home_team)

away <- nflfastR::fast_scraper_schedules(2021) %>%
    filter(game_type == 'REG') %>% 
    select(game_id, season, week, away_team, result) %>% 
    rename(team = away_team) %>% 
    mutate(result = -result)

# converting scores to wins and losses
results <- bind_rows(home, away) %>% 
    arrange(week) %>% 
    mutate(win = case_when(
        result > 0 ~ 1,
        result < 0 ~ 0,
        result == 0 ~ 0.5
    )
    )

results

# binding wins to teams over a specific season
team_wins <- results %>% 
    
    group_by(team, season) %>% 
    
    summarize(
        wins = sum(win == 1),
        losses = sum(win == 0),
        ties = sum(win == 0.5)
    ) %>% 
    
    ungroup() %>%
    
    arrange(season) %>% 
    mutate(win_pct = wins / (wins + losses) %>% round(3))

team_wins

# join win-loss records to EPA stats ----

epa_and_wins <- rush_pass_overall_epa_ranks %>% 
    left_join(team_wins, by = c('posteam' = 'team')) %>% 
    select(posteam, season, wins, losses, win_pct, everything()) %>% 
    left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

epa_and_wins

# pass and rush epa, visualization ----

epa_and_wins %>% 
    ggplot(aes(x = rush_epa_per_play,
               y = pass_epa_per_play)) +
    geom_hline(yintercept = mean(epa_and_wins$pass_epa_per_play), linetype = "dashed") +
    geom_vline(xintercept = mean(epa_and_wins$rush_epa_per_play), linetype = "dashed") +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    theme_bw() +
    labs(title = 'Rush and Pass EPA per Play, 2021 Season',
         caption = 'by Rivka Boord, Jets X-Factor',
         x = 'Rush EPA per Play',
         y = 'Pass EPA per Play') +
    theme(plot.title = element_text(color = 'dark green', face = 'bold', size = 18, hjust = 0.5),
          axis.title.x = element_text(face = 'bold', size = 14),
          axis.title.y = element_text(face = 'bold', size = 14))

ggsave('pass_rush_epa_21.png', dpi = "retina")

# rush epa and wins, visualization ----

epa_and_wins %>% 
    ggplot(aes(x = rush_attempts_rank,
               y = wins)) +
    geom_smooth(method = 'lm', color = 'black', se = FALSE) +
    geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    labs(title = 'Rush Attempts and Winning',
         x = 'Rush Attempts Rank',
         y = 'Wins') +
    theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
          axis.title = element_text(size = 14, face = 'bold'))

# pass epa and wins, visualization ----

epa_and_wins %>% 
    ggplot(aes(x = pass_attempts_rank,
               y = wins)) +
    geom_smooth(method = 'lm', color = 'black', se = FALSE) +
    geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    labs(title = 'Pass Attempts and Winning',
         x = 'Pass Attempts Rank',
         y = 'Wins') +
    theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
          axis.title = element_text(size = 14, face = 'bold'))

# read_dvoa() function to batch import dvoa data ----
library(readr)

read_dvoa <- function(year, game_facet) {
    tables_yearly <- read_csv(
        paste0("C://Documents and Settings/rivki/Documents/Programming and Data Science/R/NFL/DVOA stats/2012-21 DVOA by team/", year, 
               " Team DVOA Ratings ", game_facet, ".csv")
    ) %>% 
        mutate(year = year)
}

# importing defensive dvoa data using read_dvoa() ----
offense_dvoa_21 <- 
    map_df(.x = 2021, game_facet = "Offense", .f = read_dvoa) %>%
    
    select(1:10, 17:21) %>% 
    rename(team                     = 'Team',
           total_off_dvoa_rank      = 'Total DVOA Rank',
           total_off_dvoa           = 'Total DVOA',
           prev_year_off_rank       = 'Prev. Year Rank',
           weighted_off_dvoa_rank   = 'Weighted DVOA Rank',
           weighted_off_dvoa        = 'Weighted DVOA',
           pass_off_dvoa_rank       = 'Pass DVOA Rank',
           pass_off_dvoa            = 'Pass DVOA',
           rush_off_dvoa_rank       = 'Rush DVOA Rank',
           rush_off_dvoa            = 'Rush DVOA',
           variance_off_rank        = 'Variance Rank',
           schedule_rank            = 'Schedule Rank',
           schedule                 = 'Schedule') %>% 
    select(team, year, everything())

offense_dvoa_21 <- offense_dvoa_21 %>% 
    mutate(team = team %>% str_replace('LAR', 'LA'),
           rush_off_dvoa = rush_off_dvoa %>% str_remove_all('%') %>% as.numeric(),
           pass_off_dvoa = pass_off_dvoa %>% str_remove_all('%') %>% as.numeric()
    )

record_off_dvoa_21 <- team_wins %>% 
    rename(year = season) %>% 
    group_by(team, year) %>% 
    inner_join(offense_dvoa_21, by = c("team", "year")) %>% 
    ungroup()

# join epa and dvoa numbers together ----

off_epa_dvoa_21 <- epa_and_wins %>% 
    left_join(offense_dvoa_21, by = c('posteam' = 'team'))


off_epa_dvoa_21 %>% 
    ggplot(aes(rush_off_dvoa, pass_off_dvoa)) +
    geom_hline(yintercept = mean(off_epa_dvoa_21$pass_off_dvoa), linetype = "dashed") +
    geom_vline(xintercept = mean(off_epa_dvoa_21$rush_off_dvoa), linetype = "dashed") +
    geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    theme_bw() +
    labs(title = '2021 NFL Team Rushing and Passing DVOA',
         subtitle = 'dashed lines represent NFL average',
         x = 'Rush Offense DVOA',
         y = 'Pass Offense DVOA') +
    theme(plot.title = element_text(face = 'bold', size = 18, hjust = 0.5),
          plot.subtitle = element_text(face = 'italic', size = 10, hjust = 0.5),
          axis.title = element_text(face = 'bold', size = 12))

ggsave('dvoa_2021_rush_pass.png', dpi = 'retina')

off_epa_dvoa_21 %>% 
    select(posteam, rush_off_dvoa, rush_attempts_rank) %>% 
    arrange((rush_off_dvoa)) %>% 
    View()
    