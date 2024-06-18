# Load required libraries
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

# Set working directory
setwd('C:\\Users\\HP\\Downloads')

install.packages("fitdistrplus")
# Load datasets
ipl_bbb <- read_csv('IPL_ball_by_ball_updated till 2024.csv')
ipl_salary <- read_excel('IPL SALARIES 2024.xlsx')

# Display the first two rows of ipl_salary
head(ipl_salary, 2)

# Group the data and aggregate
grouped_data <- ipl_bbb %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE), 
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Summarise player runs and wickets
player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Sort player runs for season 2023
player_runs_2023 <- player_runs %>%
  filter(Season == '2023') %>%
  arrange(desc(runs_scored))

# Get top 3 run-getters and bottom 3 wicket-takers per season
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  top_n(3, runs_scored) %>%
  ungroup()

bottom_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  top_n(3, wicket_confirmation) %>%
  ungroup()

# Print results
cat("Top Three Run Getters:\n")
print(top_run_getters)

cat("Top Three Wicket Takers:\n")
print(bottom_wicket_takers)

# Create a dataframe for match id and year
ipl_year_id <- data.frame(
  id = ipl_bbb$`Match id`,
  year = format(as.Date(ipl_bbb$Date, format = "%d/%m/%Y"), "%Y")
)

# Create a copy of ipl_bbb dataframe and add a year column
ipl_bbbc <- ipl_bbb %>%
  mutate(year = format(as.Date(Date, format = "%d/%m/%Y"), "%Y"))

# Display the first few rows of the modified dataframe
head(ipl_bbbc %>% select(`Match id`, year, runs_scored, wicket_confirmation, Bowler, Striker))

# Load required libraries
library(dplyr)
library(fitdistrplus)
library(data.table)

# Define a function to get the best distribution
get_best_distribution <- function(data) {
  dist_names <- c('norm', 'lnorm', 'gamma', 'weibull', 'exponential', 'logis', 'cauchy')
  dist_results <- list()
  params <- list()
  for (dist_name in dist_names) {
    fit <- fitdist(data, dist_name)
    ks_test <- ks.test(data, dist_name, fit$estimate)
    p_value <- ks_test$p.value
    cat("p value for", dist_name, "=", p_value, "\n")
    dist_results[[dist_name]] <- p_value
    params[[dist_name]] <- fit$estimate
  }
  best_dist <- names(which.max(unlist(dist_results)))
  best_p <- max(unlist(dist_results))
  cat("\nBest fitting distribution:", best_dist, "\n")
  cat("Best p value:", best_p, "\n")
  cat("Parameters for the best fit:", params[[best_dist]], "\n")
  return(list(best_dist, best_p, params[[best_dist]]))
}

# Total runs each year
total_run_each_year <- ipl_bbbc %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year, desc(runs_scored))

print(total_run_each_year)

list_top_batsman_last_three_year <- list()
for (i in unique(total_run_each_year$year)[1:3]) {
  list_top_batsman_last_three_year[[as.character(i)]] <- total_run_each_year %>%
    filter(year == i) %>%
    top_n(3, runs_scored) %>%
    pull(Striker)
}

print(list_top_batsman_last_three_year)

# Suppress warnings
options(warn = -1)

# Runs for each batsman
runs <- ipl_bbbc %>%
  group_by(Striker, `Match id`) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

for (key in names(list_top_batsman_last_three_year)) {
  for (Striker in list_top_batsman_last_three_year[[key]]) {
    cat("************************\n")
    cat("year:", key, " Batsman:", Striker, "\n")
    get_best_distribution(runs %>% filter(Striker == Striker) %>% pull(runs_scored))
    cat("\n\n")
  }
}

# Total wickets each year
total_wicket_each_year <- ipl_bbbc %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year, desc(wicket_confirmation))

print(total_wicket_each_year)

list_top_bowler_last_three_year <- list()
for (i in unique(total_wicket_each_year$year)[1:3]) {
  list_top_bowler_last_three_year[[as.character(i)]] <- total_wicket_each_year %>%
    filter(year == i) %>%
    top_n(3, wicket_confirmation) %>%
    pull(Bowler)
}

print(list_top_bowler_last_three_year)

# Load required libraries
library(dplyr)
library(stringdist)
library(fitdistrplus)

# Suppress warnings
options(warn = -1)

# Aggregate wickets data
wickets <- ipl_bbbc %>%
  group_by(Bowler, `Match id`) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Get best distribution for top bowlers in the last three years
for (key in names(list_top_bowler_last_three_year)) {
  for (bowler in list_top_bowler_last_three_year[[key]]) {
    cat("************************\n")
    cat("year:", key, " Bowler:", bowler, "\n")
    get_best_distribution(wickets %>% filter(Bowler == bowler) %>% pull(wicket_confirmation))
    cat("\n\n")
  }
}

# Load necessary libraries
library(dplyr)
library(fitdistrplus)

# Filter the runs scored by CV Varun
CV_Varun_runs <- runs %>% filter(Striker == "CV Varun") %>% pull(runs_scored)

# Function to fit the best distribution
get_best_distribution <- function(data) {
  # Fit different distributions
  fit_norm <- fitdist(data, "norm")
  fit_pois <- fitdist(data, "pois")
  fit_exp <- fitdist(data, "exp")
  
  # Compare the distributions
  gof_stat <- gofstat(list(fit_norm, fit_pois, fit_exp), fitnames = c("Normal", "Poisson", "Exponential"))
  
  # Print the goodness-of-fit statistics
  print(gof_stat)
  
  # Return the best fit distribution
  best_fit <- names(which.min(gof_stat$aic))
  return(best_fit)
}

# Fit the distribution to CV Varun's runs scored and get the best distribution
best_distribution <- get_best_distribution(CV_Varun_runs)

# Print the best distribution
print(paste("Best fitting distribution:", best_distribution))

# Filter total runs for the year 2024
R2024 <- total_run_each_year %>%
  filter(year == 2024)

# Function to match names using string distance
match_names <- function(name, names_list) {
  match <-match(name, names_list, method = "jw", maxDist = 0.2)
  if (!is.na(match)) {
    return(names_list[match])
  } else {
    return(NA)
  }
}

# Create a new column in ipl_salary with matched names from R2024
ipl_salary$Matched_Player <- sapply(ipl_salary$Player, function(x) match_names(x, R2024$Striker))

# Merge the dataframes on the matched names
df_merged <- merge(ipl_salary, R2024, by.x = "Matched_Player", by.y = "Striker")

# Display structure of the merged dataframe
str(df_merged)

# Calculate the correlation between Salary and Runs
correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")

cat("Correlation between Salary and Runs:", correlation, "\n")




