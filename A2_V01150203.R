# Install and load the fitdistrplus package
options(repos = c(CRAN = "https://cran.r-project.org"))
install.packages("fitdistrplus")
library(fitdistrplus)

# Load necessary libraries
library(dplyr)

# Load the dataset
ipl_data <- read.csv("C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/DATA/IPL_ball_by_ball_updated till 2024.csv")

# Aggregate data season-wise, batsman-wise, and bowler-wise
ipl_summary <- ipl_data %>%
  group_by(Season, Match.id, Striker, Bowler) %>%
  summarise(
    total_runs = sum(runs_scored, na.rm = TRUE),
    total_wickets = sum(wicket_confirmation, na.rm = TRUE),
    .groups = "drop"
  )

# Top three run-getters and top three wicket-takers per season
top_players_per_season <- ipl_summary %>%
  group_by(Season) %>%
  summarise(
    top_run_getters = list(head(arrange(ipl_summary, desc(total_runs)), 3)),
    top_wicket_takers = list(head(arrange(ipl_summary, desc(total_wickets)), 3)),
    .groups = "drop"
  )

# Filter data for the last three IPL tournaments
last_three_seasons <- ipl_data %>%
  filter(Season %in% tail(unique(Season), 3))

# Get top three batsmen based on total runs
top_batsmen <- last_three_seasons %>%
  group_by(Striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE)) %>%
  arrange(desc(total_runs)) %>%
  head(3)

# Get top three bowlers based on total wickets
top_bowlers <- last_three_seasons %>%
  group_by(Bowler) %>%
  summarise(total_wickets = sum(wicket_confirmation, na.rm = TRUE)) %>%
  arrange(desc(total_wickets)) %>%
  head(3)

# Extract the data for top batsmen and bowlers
top_batsmen_runs <- last_three_seasons %>%
  filter(Striker %in% top_batsmen$Striker) %>%
  pull(runs_scored)

top_bowlers_wickets <- last_three_seasons %>%
  filter(Bowler %in% top_bowlers$Bowler) %>%
  pull(wicket_confirmation)

# Function to fit and plot distribution
fit_and_plot_distribution <- function(data) {
  fit <- fitdist(data, "norm")
  plot(fit)
  return(fit)
}

# Fit distributions for top batsmen and bowlers
fit_batsmen <- fit_and_plot_distribution(top_batsmen_runs)
fit_bowlers <- fit_and_plot_distribution(top_bowlers_wickets)

# Summary of fitted distributions
summary(fit_batsmen)
summary(fit_bowlers)

# Filter data for AK Markram
markram_data <- ipl_data %>%
  filter(Striker == "AK Markram" | Bowler == "AK Markram")

install.packages("readxl")
library(readxl)

# Load the salary dataset
salary_data <- read_excel("C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/DATA/IPL SALARIES 2024.xlsx")

# View the structure and first few rows of the salary dataset to identify relevant columns
str(salary_data)
head(salary_data)

# Filter the salary data for SP Narine
markram_salary <- salary_data %>%
  filter(grepl("AK Markram", Player))

# Check unique player names to ensure correct filtering
unique(salary_data$Player)

# Filter the salary data for SP Narine
markram_salary <- salary_data %>%
  filter(grepl("AK Markram", Player))

# Check the selected data
print(markram_salary)

# Manually create the salary data for SP Narine
seasons <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)
salaries <- c(260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260) 
markram_salary <- data.frame(
  Player = rep("AK Markram", length(seasons)),
  Season = seasons,
  Salary = salaries
)

# Print the manually created salary data
print(markram_salary)

# Summarize performance metrics for SP Narine
markram_performance <- markram_data %>%
  group_by(Season) %>%
  summarise(
    total_runs = sum(ifelse(Striker == "AK Markram", runs_scored, 0), na.rm = TRUE),
    total_wickets = sum(ifelse(Bowler == "AK Markram", wicket_confirmation, 0), na.rm = TRUE)
  )

# Clean the Season column to retain only numeric values and then convert to numeric
markram_salary$Season <- as.numeric(gsub("[^0-9]", "", markram_salary$Season))
markram_performance$Season <- as.numeric(gsub("[^0-9]", "", markram_performance$Season))

# Ensure there are no NAs in Season columns after conversion
markram_salary <- markram_salary %>%
  filter(!is.na(Season))
markram_performance <- markram_performance %>%
  filter(!is.na(Season))

# Join the summarized performance metrics with the salary data
markram_performance <- markram_performance %>%
  left_join(markram_salary, by = "Season")

# Ensure the join is correct
print(markram_performance)

# Fit a linear model to find the relationship between performance and salary
fit_model_runs <- lm(Salary ~ total_runs, data = markram_performance)
fit_model_wickets <- lm(Salary ~ total_wickets, data = markram_performance)

# Summary of the models
summary(fit_model_runs)
summary(fit_model_wickets)

# Plot the relationship
plot(markram_performance$total_runs, markram_performance$Salary, main = "Salary vs Runs", xlab = "Total Runs", ylab = "Salary")
abline(fit_model_runs, col = "blue")
plot(markram_performance$total_wickets, markram_performance$Salary, main = "Salary vs Wickets", xlab = "Total Wickets", ylab = "Salary")
abline(fit_model_wickets, col = "red")
