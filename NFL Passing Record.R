# Download packages
install.packages('nflfastR')
install.packages('tidyverse')

# Open libraries
library(nflfastR) #NFL play-by-play data
library(tidyverse) #Data manipulation, data visualization (houses dplyr, ggplot2, etc.)

# Load play-by-play data from 2000-2025
pbp <- nflfastR:: load_pbp(2000:2025)
View(pbp)

## IS THERE ACTUALLY A DIFFERENCE IN KICKOFFS?---
# Extract the first play of each drive to determine starting field position
first_play_drive <- pbp %>% 
  filter(!is.na(drive), season_type == 'REG', play_type != 'kickoff', play_type != 'punt') %>% 
  arrange(game_id, drive, play_id) %>% 
  group_by(game_id, drive) %>% 
  slice(1) %>% 
  ungroup()

# Extract distance to the endzone on first play for analysis
starting_field_position <- first_play_drive %>% 
  select(
    season,
    game_id,
    posteam,
    drive,
    yardline_100,
    drive_start_yards_to_endzone = yardline_100,
    drive_start_transition
  )
View(starting_field_position)

# Extract the starting QB for each drive for future yardage calculation
drive_qb <- pbp %>% 
  filter(!is.na(drive), qb_dropback == 1) %>% 
  arrange(game_id, drive, play_id) %>% 
  group_by(game_id, drive) %>% 
  slice(1) %>% 
  ungroup() %>% 
  transmute(
    season,
    game_id,
    posteam,
    drive,
    driveqb_id = passer_player_id,
    qb_name = passer_player_name
            )
View(drive_qb)

# Join data together to get starting position and QB in same data frame
drive_starts <- starting_field_position %>% 
  left_join(
    drive_qb,
    by = c('season', 'game_id', 'drive', 'posteam')
  )
View(drive_starts)

# Calculate average, median, standard deviation, and variance for starting field position by year
avg_start_by_season <- drive_starts %>% 
  group_by(season) %>% 
  summarise(
    avg_yards_to_endzone = mean(drive_start_yards_to_endzone, na.rm = TRUE),
    med_yards_to_endzone = median(drive_start_yards_to_endzone, na.rm = TRUE),
    stdev_yards_to_endzone = sd(drive_start_yards_to_endzone, na.rm = TRUE),
    var_yards_to_endzone = var(drive_start_yards_to_endzone, na.rm = TRUE),
    drives= n()
  ) %>% 
  arrange(avg_yards_to_endzone)
View(avg_start_by_season)

# Calculate average starting field position by team and by year
avg_start_by_season_team <- drive_starts %>% 
  group_by(season) %>% 
  summarise(
    avg_start_yards_to_endzone = mean(drive_start_yards_to_endzone, na.rm = TRUE),
    drives = n()
  ) %>% 
  arrange(season)
View(avg_start_by_season_team)

# Subset data based on Kickoff rules
# 2000-2010 seasons
drive_starts_00_10 <- drive_starts %>% 
  filter(season <2011)
View(drive_starts_00_10)

# 2011-2015 seasons
drive_starts_11_15 <- drive_starts %>% 
  filter(between(season, 2011, 2015))
View(drive_starts_11_15)

# 2016-2023 seasons
drive_starts_16_23 <- drive_starts %>% 
  filter(between(season, 2016, 2023))
View(drive_starts_16_23)

# 2024-Present seasons
drive_starts_24_pres <- drive_starts %>% 
  filter(season >= 2024)
View(drive_starts_24_pres)

# Conduct t-test to compare average starting field position for each period in comparison to 2024 - Present
t.test(drive_starts_00_10$drive_start_yards_to_endzone, drive_starts_24_pres$drive_start_yards_to_endzone)
t.test(drive_starts_11_15$drive_start_yards_to_endzone, drive_starts_24_pres$drive_start_yards_to_endzone)
t.test(drive_starts_16_23$drive_start_yards_to_endzone, drive_starts_24_pres$drive_start_yards_to_endzone)

## HOW HAVE KICKOFFS AFFECTED STARTING FIELD POSITION---
# Create box plots for each distinct kickoff rule to visualize impact of the rule changes
drive_starts$season_group <- cut(
  drive_starts$season,
  breaks = c(2000, 2011, 2016, 2024, Inf),
  right = FALSE,
  labels = c(
    "2000–2010",
    "2011–2015",
    "2016–2023",
    "2024–Present"
  )
)
ggplot(drive_starts,
       aes(x = season_group, y = drive_start_yards_to_endzone)) +
  geom_boxplot() +
  scale_y_reverse(limits = (c(100,0)))+
  coord_flip() +
  # Median
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0("Med: ", round(..y.., 1))),
    vjust = -0.5,
    size = 3
  ) +
  # Q1 (25th percentile)
  stat_summary(
    fun = function(x) quantile(x, 0.25),
    geom = "text",
    aes(label = paste0("Q1: ", round(..y.., 1))),
    vjust = 1.5,
    size = 3
  ) +
  # Q3 (75th percentile)
  stat_summary(
    fun = function(x) quantile(x, 0.75),
    geom = "text",
    aes(label = paste0("Q3: ", round(..y.., 1))),
    vjust = -1.5,
    size = 3
  ) +
  # n
  stat_summary(
    fun.data = function(x) {
      data.frame(
        y = max(x, na.rm = TRUE) + 3,
        label = paste0("n=", length(x))
      )
    },
    geom = "text",
    size = 3
  ) +
  labs(
    x = "Season Range",
    y = "Yards to Endzone",
    title = "Drive Start Field Position by Season Range"
  )

# Finding the number of touchbacks per season
# Filter all plays to find regular season kickoffs
kickoffs <- pbp %>% 
  filter(season_type == 'REG', play_type == 'kickoff')
View(kickoffs)

# Count the number of touchbacks by each season
touchbacks_by_year <- kickoffs %>% 
  group_by(season) %>% 
  summarise(total_touchbacks = sum(touchback))
View(touchbacks_by_year)

# Create a line chart to show the changes in touchbacks by year
touchbacks_by_year_lc <- ggplot(touchbacks_by_year,aes( x = season, y = total_touchbacks)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_rect(xmin = 1999.5, xmax = 2010.5,
            ymin = -Inf, ymax = Inf,
            fill = 'lightgrey', alpha = 0.03) +
  geom_rect(xmin = 2010.5, xmax = 2015.5,
            ymin = -Inf, ymax = Inf,
            fill = 'lightblue', alpha = 0.03) +
  geom_rect(xmin = 2015.5, xmax = 2023.5,
            ymin = -Inf, ymax = Inf,
            fill = 'lightpink', alpha = 0.03) +
  geom_rect(xmin = 2023.5, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = 'lightgreen', alpha = 0.03) +
  labs(
    x = 'Season',
    y = 'Number of TouchBacks',
    title = "Number of Touchbacks In Each Season"
  ) +
  theme_minimal()
touchbacks_by_year_lc
  
## STARTING FIELD POSITION AND RECORDS---
# Compute amount of yardage possible by team and year
total_possible_yardage <- drive_starts %>%
  filter(!is.na(posteam)) %>% 
  group_by(season, posteam) %>% 
  summarise(possible_yardage = sum(drive_start_yards_to_endzone, na.rm = TRUE),
            drives = n(),
            .groups = "drop"
  ) %>% 
  arrange(desc(possible_yardage))
summary(total_possible_yardage)
View(total_possible_yardage)

# Compute the amount of actual yardage recorded by team and year
actual_yardage <- pbp %>% 
  filter(!is.na(posteam), season_type == "REG") %>% 
  group_by(season, posteam) %>% 
  summarise(actual_yardage = sum(yards_gained, na.rm = TRUE), 
            .groups = "drop") %>% 
  arrange(desc(actual_yardage))
View(actual_yardage)

# Join actual and possible datasets together
total_actual_possible_yardage <- actual_yardage %>% 
  left_join(
    total_possible_yardage,
    by = c('season', 'posteam')
  ) %>%
  mutate(
    actual_yardage = as.numeric(actual_yardage),
    possible_yardage = as.numeric(possible_yardage),
    perct_of_actual_to_possible = actual_yardage / possible_yardage) %>% 
  arrange(desc(perct_of_actual_to_possible))
View(total_actual_possible_yardage)
summary(total_actual_possible_yardage)

# Manning Analysis
# Extract the Broncos' 2013 starting points
broncos_2013_drive_starts <- drive_starts %>% 
  filter(season == 2013, posteam == 'DEN') %>% 
  arrange(desc(drive_start_yards_to_endzone))
View(broncos_2013_drive_starts)
summary(broncos_2013_drive_starts)

# Count the number of drives starting after kickoff, punt, and turnovers
broncos_drive_transition_breakdown <- broncos_2013_drive_starts %>% 
  count(drive_start_transition)
broncos_drive_transition_breakdown

# Create histogram of 2013 starting points
broncos_hist <- ggplot(broncos_2013_drive_starts, aes(x = drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 1, boundary =0, closed = 'left', color = 'blue', fill = 'orange') +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Starting Yardage to Endzone for 2013 Broncos", x = "Yardage to Endzone", y = ' Frequency') +
  theme_minimal()
broncos_hist

# Calculate total possible yardage for Manning's record breaking season (5477 yards)
broncos_2013_total_possible_yardage = sum(broncos_2013_drive_starts$drive_start_yards_to_endzone)
print(broncos_2013_total_possible_yardage)
print(count(broncos_2013_drive_starts))
print(paste("Peyton Manning's share of possible yardage:", 5477/14369))
print(paste("Peyton Manning's share of actual yardage:", 5477/7317))

# Burrow Analysis
# Extract the Bengals' 2024 starting points
bengals_2024_drive_starts <- drive_starts %>% 
  filter(season == 2024, posteam == 'CIN')
View(bengals_2024_drive_starts)
summary(bengals_2024_drive_starts)

# Count the number of drives starting after kickoff, punt, and turnovers
bengals_drive_transition_breakdown <- bengals_2024_drive_starts %>% 
  count(drive_start_transition)
bengals_drive_transition_breakdown

# Create histogram of 2024 Bengals starting points
bengals_hist <- ggplot(bengals_2024_drive_starts, aes(x = drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 1, boundary =0, closed = 'left', color = 'orange', fill = 'black') +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Starting Yardage to Endzone for 2024 Bengals", x = "Yardage to Endzone", y = ' Frequency') +
  theme_minimal()
bengals_hist

# Calculate total possible yardage for Burrow's 2024 season
bengals_2024_total_possible_yardage = sum(bengals_2024_drive_starts$drive_start_yards_to_endzone)
print(bengals_2024_total_possible_yardage)
print(count(bengals_2024_drive_starts))
print(paste("Joe Burrow's share of possible yardage:", 4918/12431))
print(paste("Joe Burrow's share of actual yardage:", 4918/6216))

# Stafford Analysis
# Extract the Rams' 2025 starting points
rams_2025_drive_starts <- drive_starts %>% 
  filter(season == 2025, posteam == 'LA')
View(rams_2025_drive_starts)
summary(rams_2025_drive_starts)

# Count the number of drives starting after kickoff, punt, and turnovers
rams_drive_transition_breakdown <- rams_2025_drive_starts %>% 
  count(drive_start_transition)
rams_drive_transition_breakdown

# Create histogram of 2025 Rams starting points
rams_hist <- ggplot(rams_2025_drive_starts, aes(x = drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 1, boundary =0, closed = 'left', color = 'yellow', fill = 'blue') +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Starting Yardage to Endzone for 2025 Rams", x = "Yardage to Endzone", y = ' Frequency') +
  theme_minimal()
rams_hist

# Calculate total possible yardage for Stafford's 2025 season
rams_2025_total_possible_yardage = sum(rams_2025_drive_starts$drive_start_yards_to_endzone)
print(rams_2025_total_possible_yardage)
print(count(rams_2025_drive_starts))
print(paste("Matthew Stafford's share of possible yardage:", 4707/12404))
print(paste("Matthew Stafford's share of actual yardage:", 4707/6709))

# League analysis
# Creating a line chart of median starting points
median_by_season <- drive_starts %>%
  group_by(season) %>%
  summarise(
    median_start = median(drive_start_yards_to_endzone, na.rm = TRUE)
  )
View(median_by_season)
median_starting_pos <- ggplot(median_by_season, aes(x= season, y = median_start)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(70, 80, by = 1)) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 1)) +
  geom_rect(xmin = 1999.5, xmax = 2010.5,
            ymin = -Inf, ymax = Inf,
            fill = 'lightgrey', alpha = 0.03) +
  geom_rect(xmin = 2010.5, xmax = 2015.5,
            ymin = -Inf, ymax = Inf,
            fill = 'lightblue', alpha = 0.03) +
  geom_rect(xmin = 2015.5, xmax = 2023.5,
            ymin = -Inf, ymax = Inf,
            fill = 'lightpink', alpha = 0.03) +
  geom_rect(xmin = 2023.5, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = 'lightgreen', alpha = 0.03) +
  labs(
    x = 'Season',
    y = 'Median Starting Field Position',
    title = "Median Starting Field Position"
  ) +
  theme_minimal()
median_starting_pos

## OTHER TOPICS TO CONSIDER---
# Calculate the percentage of kickoffs resulting in a touchback by year and team
touchback_perct_by_year <- kickoffs %>% 
  group_by(season, posteam) %>% 
  summarise(total_kickoffs = n(),
            total_touchbacks = sum(touchback),
            touchback_perct = mean(touchback)
            )
View(touchback_perct_by_year)

# Calculate the percentage of touchbacks during 2011-2015 seasons
touchback_perc_11_15 = mean(touchback_perct_by_year$touchback_perct[touchback_perct_by_year$season >= 2011 & touchback_perct_by_year$season <=2015])
touchback_perc_11_15
# Calculate the percentage of touchbacks during 2016-2023 seasons
touchback_perc_16_23 = mean(touchback_perct_by_year$touchback_perct[touchback_perct_by_year$season >= 2016 & touchback_perct_by_year$season <=2023])
touchback_perc_16_23




## ADDITIONAL CODE NOT USED IN REPORT---
#Wilcox tests to check robustness of results
wilcox.test(drive_starts_00_10$drive_start_yards_to_endzone, drive_starts_24_pres$drive_start_yards_to_endzone)
wilcox.test(drive_starts_11_15$drive_start_yards_to_endzone, drive_starts_24_pres$drive_start_yards_to_endzone)
wilcox.test(drive_starts_16_23$drive_start_yards_to_endzone, drive_starts_24_pres$drive_start_yards_to_endzone)

#Basic summary of drives to help support thought process and provide additional background
summary(drive_starts_00_10)
summary(drive_starts_11_15)
summary(drive_starts_16_23)
summary(drive_starts_24_pres)

#Creating starting field position histogram for each kickoff rule
drive_start_hist_00_10 <- ggplot(drive_starts_00_10, aes(drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 5, boundary =0, closed = 'left', color = 'black', fill = 'darkgreen') +
  labs(title = "Starting Field Position From The 2000 to 2010 Seasons", x = 'Yards to Endzone', y= 'Frequency') +
  theme_minimal()
drive_start_hist_00_10

drive_start_hist_11_15 <- ggplot(drive_starts_11_15, aes(drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 5, boundary =0, closed = 'left', color = 'black', fill = 'darkgreen') +
  labs(title = "Starting Field Position From The 2011 to 2015 Seasons", x = 'Yards to Endzone', y= 'Frequency') +
  theme_minimal()
drive_start_hist_11_15

drive_start_hist_16_23 <- ggplot(drive_starts_16_23, aes(drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 5, boundary =0, closed = 'left', color = 'black', fill = 'darkgreen') +
  labs(title = "Starting Field Position From The 2016 to 2023 Seasons", x = 'Yards to Endzone', y= 'Frequency') +
  theme_minimal()
drive_start_hist_16_23

drive_start_hist_24_pres <- ggplot(drive_starts_24_pres, aes(drive_start_yards_to_endzone)) +
  geom_histogram(binwidth = 5, boundary =0, closed = 'left', color = 'black', fill = 'darkgreen') +
  labs(title = "Starting Field Position From The 2024 to Present Season", x = 'Yards to Endzone', y= 'Frequency') +
  theme_minimal()
drive_start_hist_24_pres






