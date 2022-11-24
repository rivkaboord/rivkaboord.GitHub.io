library(rvest)
library(tidyverse)
# install.packages('baseballr')
library(baseballr)
library(ggplot2)

bref_team_results(Tm = 'NYM', year = 2022) %>% 
    select(R) %>% 
    group_by(R) %>% 
    count(name = 'games') %>% 
    mutate(R = as.factor(R))

bref_team_results(Tm = 'NYM', year = 2022) %>% 
    filter(R <= 2) %>% 
    count()

bref_team_results(Tm = 'NYM', year = 2022) %>% 
    filter(R <= 3) %>% 
    count()


runs_scored_breakdown_graph <- function(team) {
    
    team_2_runs_or_less <- bref_team_results(Tm = {{team}}, year = 2022) %>% 
        filter(R <= 2) %>% 
        count(name = 'games_2_runs_or_less')
    
    team_3_runs_or_less <- bref_team_results(Tm = {{team}}, year = 2022) %>% 
        filter(R <= 3) %>% 
        count(name = 'games_3_runs_or_less')
    
    bref_team_results(Tm = team, year = 2022) %>% 
        select(R) %>% 
        group_by(R) %>% 
        mutate(R = as.factor(R)) %>% 
        ggplot(aes(x = R)) +
        geom_bar(fill = "blue") +
        labs(title = paste0(team, ' Runs Scored Breakdown'),
             subtitle = paste0(team, ' has scored 2 runs or fewer ', team_2_runs_or_less,
                               ' times and 3 runs or fewer ', team_3_runs_or_less, ' times.'),
             x = 'Total Runs Scored',
             y = 'Games') +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
              plot.subtitle = element_text(size = 14, face = 'italic'),
              axis.title = element_text(size = 14, face = 'bold'),
              axis.text = element_text(size = 12))
    
}

runs_scored_breakdown <- function(team) {
    
    bref_team_results(Tm = team, year = 2022) %>% 
        select(R) %>% 
        group_by(R) %>%
        count() %>% 
        mutate(team = {{team}})
    
}

top_playoff_contenders = c('NYM', 'ATL', 'LAD', 'HOU', 'NYY', 'PHI', 'STL', 'SD')

playoff_contenders_runs_scored_breakdown_graph <- map(top_playoff_contenders, runs_scored_breakdown_graph)
playoff_contenders_runs_scored_breakdown <- map(top_playoff_contenders, runs_scored_breakdown)

playoff_contenders_runs_scored_breakdown_graph
playoff_contenders_runs_scored_breakdown

playoff_contenders_runs_scored_breakdown %>% 
    

playoff_contenders_runs_scored_breakdown_graph[1]
ggsave('mets_runs_scored_breakdown_22.png')

playoff_contenders_runs_scored_breakdown_graph[2]
ggsave('atl_runs_scored_breakdown_22.png')

install.packages('data.table')
library(data.table)

playoff_contenders_runs_scored_breakdown %>%
    rbindlist() %>% 
    View()

bref_team_results(Tm = 'NYM', year = 2022) %>% 
    select(Result, R, RA) %>% 
    filter(R <= 3) %>% 
    mutate(Result = Result %>% str_remove_all('-wo')) %>% 
    group_by(Result) %>% 
    count()

bref_team_results(Tm = 'MIL', year = 2022) %>% 
    select(Result, R, RA) %>% 
    filter(R >= 4) %>% 
    mutate(Result = Result %>% str_remove_all('-wo')) %>% 
    group_by(Result) %>% 
    count()

bref_team_results(Tm = 'MIL', year = 2022) %>% 
    select(Result, R, RA) %>% 
    filter(RA <= 2) %>% 
    mutate(Result = Result %>% str_remove_all('-wo')) %>% 
    group_by(Result) %>% 
    count()

# Mets runs scored further breakdown

mets_game_logs_22 <- bref_team_results(Tm = 'NYM', year = 2022)

mets_game_logs_22 %>% 
    mutate(quarter = case_when(
        Gm <= 40 ~ 1,
        Gm > 40 & Gm <= 80 ~ 2,
        Gm > 80 & Gm <= 120 ~ 3,
        TRUE ~ 4
    )) %>% 
    select(Result, R, quarter) %>% 
    mutate(Result = Result %>% str_remove_all('-wo')) %>% 
    group_by(R, quarter) %>% 
    count()

mets_gp <- mets_game_logs_22 %>% 
    select(R) %>% 
    summarize(rs_avg = mean(R))

# Pythagorean Wins

calculate_pyth_wins <- function(R, RA, GP) {
    
    pyth_win_pct <- {(R^1.81)}/{(R^1.81 + RA^1.81)} %>% round(digits = 3)
    
    pyth_wins = pyth_win_pct * GP
    
    return(pyth_wins)

}