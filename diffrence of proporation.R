# Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# Import dataset (make sure the CSV is in your working directory)
nobel <- read_csv("nobel_prize_by_winner.csv")

# Parse born and died dates (robust)
nobel <- nobel %>%
  mutate(
    born_date = suppressWarnings(parse_date_time(born, orders = "mdy")),
    died_date = suppressWarnings(parse_date_time(died, orders = "mdy"))
  )

# Clean dataset: keep Physics & Chemistry, valid birth dates, and create age at award
nobel_clean <- nobel %>%
  filter(category %in% c("physics", "chemistry")) %>%
  filter(!is.na(born_date)) %>%
  mutate(
    fullname = paste(firstname, surname),
    age_at_award = year - year(born_date)
  ) %>%
  select(fullname, category, age_at_award, year, gender, bornCountry)

# Histogram: Age distribution for Physics & Chemistry
ggplot(nobel_clean, aes(x = age_at_award, fill = category)) +
  geom_histogram(color = "black", bins = 20, alpha = 0.6, position = "identity") +
  labs(
    title = "Age Distribution of Nobel Prize Winners (Physics & Chemistry)",
    x = "Age at Award",
    y = "Count",
    fill = "Category"
  ) +
  theme_minimal()

# Boxplot: Age comparison between Physics and Chemistry
ggplot(nobel_clean, aes(x = category, y = age_at_award, fill = category)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Age Comparison Between Physics and Chemistry Nobel Laureates",
    x = "Prize Category",
    y = "Age at Award",
    fill = "Category"
  ) +
  theme_minimal()

# Summary statistics by category
summary_by_category <- nobel_clean %>%
  group_by(category) %>%
  summarise(
    count = n(),
    mean_age = mean(age_at_award, na.rm = TRUE),
    median_age = median(age_at_award, na.rm = TRUE),
    sd_age = sd(age_at_award, na.rm = TRUE)
  )

print(summary_by_category)

# t-test: compare ages between Physics and Chemistry
t_test_result <- t.test(
  age_at_award ~ category,
  data = nobel_clean,
  alternative = "two.sided"
)

print(t_test_result)
