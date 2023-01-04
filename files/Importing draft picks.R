# MLB DRAFT AND PROSPECT DATA ANALYSIS ----

# load libraries
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(ggplot2)
library(tidyquant)
library(data.table)
library(plotly)
library(Lahman)

## 1. Analyze some data from first round picks since 1965 ----

# 1A. Batch import data from first round of MLB draft ----
first_round_picks_excel_files <- "../Documents/Programming and Data Science/R/MLB Draft/MLB-draft-project/Draft Excel files/"

file.list <- list.files(path = first_round_picks_excel_files, pattern = "*.xlsx", full.names = T)
df.list <- lapply(file.list, read_excel)
first_round_picks <- rbindlist(df.list, idcol = "id")

# 1B. Clean the imported first round pick data ----
first_round_picks_cleaned_df <- first_round_picks %>% 
    select(-DT, -FrRnd, -"Drafted Out of", -Bonus, -id, -Type) %>% 
    rename(PickNo = RdPck) %>% 
    filter(Rnd == 1) %>%
    mutate(Rnd = str_replace(Rnd,"s","")) %>% 
    mutate(Name = str_remove(Name,"\\s\\(minors\\)"))

# 1C. create a new WAR tibble ----
first_round_war_df <- 
    first_round_picks_cleaned_df %>% 
    distinct(.keep_all = T) %>% 
    select(Name, Year, Rnd, PickNo, WAR) %>% 
    arrange(desc(WAR)) %>% 
    mutate_at(., c("WAR"), ~replace(., is.na(.), 0.01))

first_round_war_df

# 1D. mean and median WAR for each pick number ----
war_avg_by_pick_df <- first_round_war_df %>% 
    select(PickNo, WAR) %>% 
    group_by(PickNo) %>% 
    summarise(WAR_med = median(WAR),
              WAR_mean = mean(WAR)) %>% 
    mutate(label_text = str_glue("Pick: {PickNo}
                                 WAR: {WAR_med %>% round(3)}")) %>% 
    ungroup()

war_avg_by_pick_df

# 1E. lineplot depicting the average WAR for each pick number ----
war_med_lineplot <- war_avg_by_pick_df %>% 
    
    ggplot(aes(PickNo, WAR_med)) +
    geom_line(size = 1) +
    geom_point(aes(text = label_text), color = "#1c8e20", size = 3) +
    
    theme_tq() +
  
    labs(title = "Average WAR Produced By Each Draft Position", align = "center",
         subtitle = "A steep drop from Pick 1 to Pick 3 and a consistent dropoff from there", 
         caption = 'created by Rivka Boord using the Lahman database in R',
        x = "Pick",
        y = "Median WAR Produced") +
    
    theme(plot.title = element_text(face = 'bold', size = 18, hjust = 0.5),
          plot.subtitle = element_text(face = 'italic', size = 14),
          plot.caption = element_text(face = 'italic', size = 10))

war_med_lineplot

# 1F. interactive lineplot ----
war_med_lineplot_interactive <- 
  war_med_lineplot %>% ggplotly(tooltip = "text")

war_med_lineplot_interactive

# 1G. Look at percentages in each draft slot ----

total_picks <- first_round_war_df %>% 
    mutate(PickNo = as.factor(PickNo)) %>% 
    group_by(PickNo) %>% 
    count(name = 'total_picks_made')

war_10_plus <- first_round_war_df %>% 
    mutate(PickNo = as.factor(PickNo)) %>% 
    group_by(PickNo) %>% 
    filter(WAR >= 10.0) %>% 
    count(name = 'war_10_plus')

war_20_plus <- first_round_war_df %>% 
    mutate(PickNo = as.factor(PickNo)) %>% 
    group_by(PickNo) %>% 
    filter(WAR >= 20.0) %>% 
    count(name = 'war_20_plus')

war_30_plus <- first_round_war_df %>% 
    mutate(PickNo = as.factor(PickNo)) %>% 
    group_by(PickNo) %>% 
    filter(WAR >= 30.0) %>% 
    count(name = 'war_30_plus')

pick_war_rates <- total_picks %>% 
    left_join(war_10_plus, by = c('PickNo')) %>% 
    left_join(war_20_plus, by = c('PickNo')) %>% 
    left_join(war_30_plus, by = c('PickNo')) %>% 
    mutate(pct_10_plus = war_10_plus / total_picks_made,
           pct_20_plus = war_20_plus / total_picks_made,
           pct_30_plus = war_30_plus / total_picks_made)

pick_war_rates %>% 
    ggplot(aes(PickNo, war_20_plus)) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_blank())

## 2. Analyze Baseball America prospect data from 1990-2020 ----

# 2A. read in Excel files for prospect list ----
path = "../Documents/Programming and Data Science/R/MLB Draft/MLB-draft-project/BA Prospects 1990-2021.xlsx"
prospect_data_df <- excel_sheets(path = path) %>% 
    map(~ as.data.frame(read_excel(path, sheet = .,
                                   col_types = c("numeric", "text", "guess", "text", "text", "text", "text", "text",
                                                 "text", "date", "text", "text", "text", "text", "text", "text"))))

# 2B. function to pull and clean data from big list ----
pull_df_fn <- function(data, year) {
    data %>%
        filter_all(any_vars(!is.na(.))) %>% 
        mutate(year = year) %>% 
        select(year, everything())
}

# 2C. combine data into one large dataframe ----
combine_prospect_df <- map2_df(prospect_data_df, 1990:2020, pull_df_fn) %>% 
    separate(col = `Draft Info`,
             into = c("year_picked", "round_picked", "pickNo", "team_picked"),
             sep = "-") %>%
    rename("player_name" = `Player Name`,
           "team_name" = `Team Name`,
           "team_rank" = `Team Rank`,
           "born_date" = `Born Date`,
           "birthplace" = `Place Of Birth`,
           "mlb_years" = `MLB Years`,
           "stat_years" = `Stat Years`,
           "other_rankings" = `Other Rankings this Year`) %>% 
    mutate(team_rank = as.numeric(team_rank)) %>% suppressWarnings() %>% 
    mutate(player_name = as.character(player_name))

combine_prospect_df

combine_prospect_df_cleaned <- combine_prospect_df %>%
    select(-Ht, -Wt, -Ba, -Th, -birthplace,
           -mlb_years, -stat_years, -other_rankings) %>% 
    separate(col = `born_date`,
             into = c("birthYear", "birthMonth", "birthDay"),
             sep = "-") %>% 
    mutate("birthDay" = as.integer(birthDay),
           "birthMonth" = as.integer(birthMonth),
           "birthYear" = as.integer(birthYear))

# 2D. making sure we have players' full names and IDs to make it easier to join to prospect data ----
names_and_ids_df <- People %>% 
    unite(nameFirst, nameLast, col = "player_name", sep = " ", remove = FALSE) %>% 
    select(playerID, player_name, birthYear, birthMonth,
           birthDay, everything())

# 2E. join prospect data with their player IDs (+ height/weight and batting info in R-friendly format) ----
prospects_majors_since_1990_df <- combine_prospect_df_cleaned %>% 
    inner_join(names_and_ids_df, 
              c("player_name", "birthMonth", "birthYear")) %>% 
    select(year, playerID, player_name, Rk, team_name, team_rank,
           Pos, year_picked, round_picked, pickNo, team_picked,
           height, weight, bats, throws)

prospects_majors_since_1990_df

## 3. Analyze career WAR of top prospects since 1990 by combining prospects' names with their career stats ----

# 3A. adding WAR batting statistics to the Lahman database and then to batting prospects ----
lahman_batters_with_war_df <- read.csv("../Documents/Programming and Data Science/R/MLB Draft/MLB-draft-project/lahman_db_with_bbrefid_batters.csv") %>% 
    rename("playerID" = player_ID) %>% 
    select(playerID, WAR) %>% 
    group_by(playerID) %>% 
    summarize(career_WAR = round(sum(WAR), 1))

prospects_1990_war_batters_df <- prospects_majors_since_1990_df %>% 
    inner_join(lahman_batters_with_war_df, c("playerID")) %>% 
    filter(Pos != "P")

# 3B. adding WAR pitching statistics to Lahman database and then to pitching prospects ----
lahman_pitchers_with_war_df <- read.csv("../Documents/Programming and Data Science/R/MLB Draft/MLB-draft-project/lahman_db_with_bbrefid_pitchers.csv") %>% 
  select(playerID, WAR) %>% 
  group_by(playerID) %>% 
  summarize(career_WAR = round(sum(WAR), 1))

prospects_1990_war_pitchers_df <- prospects_majors_since_1990_df %>% 
  inner_join(lahman_pitchers_with_war_df, c("playerID")) %>% 
  filter(Pos == "P")

# 3C. filtering out prospects who never made the majors ----
never_made_majors_1990_df <- prospects_majors_since_1990_df %>% 
  filter(is.na(playerID))

# 3D. join together batters and pitchers ----
prospects_1990_war_full_df <- prospects_1990_war_batters_df %>% 
  full_join(prospects_1990_war_pitchers_df) %>% 
  mutate(Rk = as.factor(Rk)) %>% 
  arrange(year, Rk)

prospects_1990_war_full_df %>% 
    summarize(first_rounder_avg_career_war = mean(career_WAR, na.rm = T),
              first_rounder_med_career_war = median(career_WAR, na.rm = T))

# 3E. aggregate mean, median, stdev, and var of WAR ----
prospects_averages_df <- prospects_1990_war_full_df %>% 
  select(Rk, career_WAR) %>% 
  mutate(career_WAR = na.fill0(career_WAR, fill = 0)) %>% 
  group_by(Rk) %>% 
  summarize(sum_war = round(sum(career_WAR), 2),
            mean_war = round(mean(career_WAR), 2),
            med_war = round(median(career_WAR), 2),
            var_war = round(var(career_WAR), 2),
            stdv_war = round(StdDev(career_WAR), 2)
            ) %>% 
  ungroup()

prospects_averages_df %>% 
    head(1) %>% 
    summarize(median_war = median(med_war))

# 3F. scatterplot of median WAR data for prospects ----
top_prospects_career_war_scatterplot <- prospects_averages_df %>% 
    select(Rk, med_war) %>% 
    ggplot(aes(Rk, med_war)) +
    geom_smooth(method = 'lm') +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
          plot.subtitle = element_text(face = 'italic', size = 12),
          plot.caption = element_text(face = 'italic')) +
    labs(title = "Top Prospect Position and Career WAR",
         subtitle = 'WAR among prospects also has a decline, but it is not as dramatic as that of first-round picks.',
         caption = 'by Rivka Boord',
         x = "Prospect Ranking",
         y = "Median Career WAR")
    
top_prospects_career_war_scatterplot

## 3. Possible analysis of all players to compare to top prospects; should remove top prospects from data ----

# 3A. mean of all players in MLB history WAR ----
lahman_all_players_war_df <- lahman_batters_with_war_df %>% 
    full_join(lahman_pitchers_with_war_df) %>% 
    na.omit(career_WAR) %>% 
    group_by(playerID) %>% 
    arrange(desc(career_WAR)) %>% 
    mutate(career_WAR = sum(career_WAR))

lahman_all_players_war_df$career_WAR

# 3B. average career WAR for all MLB players ----
lahman_all_players_war_df$career_WAR %>% mean() %>% round(digits = 1)

## 4. Comparing top prospects' career data to All Stars' - in other words, a proven player ----

# 4A. All Stars' mean and median career WAR ----
lahman_players_war <- lahman_all_players_war_df %>% 
    distinct(playerID, .keep_all = T) %>% 
    mutate(career_WAR = career_WAR %>% na.omit())

all_all_star_app_df <- Lahman::AllstarFull %>% 
    distinct(playerID, .keep_all = T) %>% 
    inner_join(lahman_players_war, by = c('playerID'))

all_all_star_app_df

# 4B. The median career WAR for all All-Stars in MLB history ----

all_all_star_app_df$career_WAR %>% median() %>% round(1)
all_all_star_app_df$career_WAR %>% mean() %>% round(1)

# 4C. Join All-Star data with first-round pick data ----

all_star_app <- Lahman::AllstarFull %>% 
    group_by(playerID) %>% 
    count()

all_star_ids <- Lahman::AllstarFull %>% 
    distinct(playerID, .keep_all = T) %>% 
    left_join(names)

all_stars_full_info <- all_star_ids %>% 
    left_join(names_and_ids_df, by = c('playerID'))

all_stars_full_info %>% 
    glimpse()

first_round_all_star <- first_round_picks_cleaned_df %>% 
    inner_join(all_stars_full_info, by = c('Name' = 'player_name')) %>% 
    left_join(all_star_app, by = c('playerID'))

# 4D. first-round pick All-Star appearances ----

total_first_round_asg_app <- first_round_all_star %>% 
    rename(asg_app = n) %>% 
    select(playerID, Name, Year, Rnd, PickNo, asg_app)

total_asg_app_by_pick_slot <- total_first_round_asg_app %>% 
    group_by(PickNo) %>% 
    summarize(total_asg_app = sum(asg_app),
              med_asg_app = median(asg_app),
              mean_asg_app = mean(asg_app))

total_asg_app_by_pick_slot

total_asg_app_by_pick_slot %>% 
    ggplot(aes(PickNo, total_asg_app)) +
    geom_point(color = 'red') +
    labs(title = 'Total All-Star Game Appearances by First-Round Draft Slot',
         subtitle = 'It appears that higher picks have significantly more ASG appearances than later ones.',
         x = 'Pick No.',
         y = 'Total ASG Appearances',
         caption = 'by Rivka Boord (created using Lahman database in R)') +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, color = 'dark blue'),
          plot.subtitle = element_text(face = 'italic', color = 'blue'),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          plot.caption = element_text(face = 'italic'))

total_asg_app_by_pick_slot %>% 
    ggplot(aes(PickNo, med_asg_app)) +
    geom_point(color = 'red') +
    labs(title = 'Total All-Star Game Appearances by First-Round Draft Slot',
         subtitle = 'The median indicates that large outliers skew the total ASG appearances.',
         x = 'Pick No.',
         y = 'Median ASG Appearances',
         caption = 'by Rivka Boord (created using Lahman database in R)') +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, color = 'dark blue'),
          plot.subtitle = element_text(face = 'italic', color = 'blue'),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          plot.caption = element_text(face = 'italic')) +
    scale_y_continuous(limits = c(0,10))