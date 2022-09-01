library(tidyverse)
library(skimr)

heart_dat <- read_csv("data/unprocessed/heart.csv") %>%
  janitor::clean_names()

glimpse(heart_dat)
skim_without_charts(heart_dat)

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar() +
  labs(title = "Distribution of heart_disease") +
  theme_minimal()

# The data does not have any missingness issues
# All of the initial character variables and two numeric variables 
# (`fasting_bs`, `heart_disease`) have to be turned into factor variables. 

heart_dat <- heart_dat %>%
  mutate(sex = factor(sex),
         chest_pain_type = factor(chest_pain_type),
         resting_ecg = factor(resting_ecg),
         exercise_angina = factor(exercise_angina),
         st_slope = factor(st_slope),
         fasting_bs = factor(fasting_bs),
         heart_disease = factor(heart_disease))

saveRDS(heart_dat, file = "data/processed/heart.rds")

# Variable distributions
ggplot(data = heart_dat, mapping = aes(x = age)) +
  geom_histogram(color = "white") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = resting_bp)) +
  geom_histogram(color = "white") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = cholesterol)) +
  geom_histogram(color = "white") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = max_hr)) +
  geom_histogram(color = "white") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = oldpeak)) +
  geom_histogram(color = "white") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = sex)) +
  geom_bar() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = chest_pain_type)) +
  geom_bar() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = fasting_bs)) +
  geom_bar() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = resting_ecg)) +
  geom_bar() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = exercise_angina)) +
  geom_bar() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = st_slope)) +
  geom_bar() +
  theme_minimal()

# Bivariate relationships
ggplot(data = heart_dat, mapping = aes(x = resting_bp, y = cholesterol)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = resting_bp, y = max_hr)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = resting_bp, y = oldpeak)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = resting_bp, y = age)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = cholesterol, y = max_hr)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = cholesterol, y = oldpeak)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = cholesterol, y = age)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = max_hr, y = oldpeak)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = max_hr, y = age)) +
  geom_point() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = oldpeak, y = age)) +
  geom_point() +
  theme_minimal()

# heart_disease vs. cont variables
ggplot(data = heart_dat, mapping = aes(x = heart_disease, y = age)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease, y = resting_bp)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease, y = cholesterol)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease, y = oldpeak)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease, y = max_hr)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar(aes(fill = sex), position = "dodge") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar(aes(fill = chest_pain_type), position = "dodge") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar(aes(fill = fasting_bs), position = "dodge") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar(aes(fill = resting_ecg), position = "dodge") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar(aes(fill = exercise_angina), position = "dodge") +
  theme_minimal()

ggplot(data = heart_dat, mapping = aes(x = heart_disease)) +
  geom_bar(aes(fill = st_slope), position = "dodge") +
  theme_minimal()
