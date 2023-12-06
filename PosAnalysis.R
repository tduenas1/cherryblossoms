#-----age groups over the years ------
# Categorize age into groups
ageGroup <- c("-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")
Performance <- df %>%
  mutate(age_group = case_when(
    Age < 20 ~ ageGroup[1],
    Age >= 20 & Age < 30 ~ ageGroup[2],
    Age >= 30 & Age < 40 ~ ageGroup[3],
    Age >= 40 & Age < 50 ~ ageGroup[4],
    Age >= 50 & Age < 60 ~ ageGroup[5],
    Age >= 60 & Age < 70 ~ ageGroup[6],
    Age >= 70 ~ ageGroup[7]
  ))

age_count <- Performance %>%
  count(Year, age_group, Sex)

# Plotting the filled stacked line chart
ggplot(age_count, aes(x = Year, y = n, fill = age_group)) +
  geom_area(position = "stack", color = "lightgrey", size=0.1) +
  scale_fill_brewer(palette = "Spectral") + # You can choose a different color palette if needed
  labs(
    title = "Distribution of Age Groups",
    x = "Year of the Race",
    y = "Count"
  ) +
  theme_minimal()+
  scale_x_continuous(breaks = unique(age_count$Year))+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+ # Rotate x-axis text by 45 degrees
  facet_wrap(~as.factor(Sex), nrow = 2)

# Calculate proportions of each age group for each year and sex
age_proportions <- Performance %>%
  count(Year, age_group, Sex) %>%
  group_by(Year, Sex) %>%
  mutate(proportion = n / sum(n))

# Plotting the filled stacked line chart faceted by sex with proportions
ggplot(age_proportions, aes(x = Year, y = proportion, fill = age_group)) +
  geom_area(position = "stack", color = "lightgrey", size=0.1) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Proportion of Age Groups in Cherry Blossom 10 Run Results",
    x = "Year of the Race",
    y = "Proportion"
  ) +
  theme_minimal() +
  facet_wrap(~Sex, ncol = 1) +
  scale_x_continuous(breaks = unique(age_proportions$Year))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis text by 45 degrees

#-------

performance_full <- df_cleaned_full %>%
  mutate(fitness_ratio = pos_by_sex / total_by_sex)

performance_full <- performance_full %>%
  mutate(age_group = case_when(
    Age < 20 ~ ageGroup[1],
    Age >= 20 & Age < 30 ~ ageGroup[2],
    Age >= 30 & Age < 40 ~ ageGroup[3],
    Age >= 40 & Age < 50 ~ ageGroup[4],
    Age >= 50 & Age < 60 ~ ageGroup[5],
    Age >= 60 & Age < 70 ~ ageGroup[6],
    Age >= 70 ~ ageGroup[7]
  ))

age_group_colors <- c("-19" = '#d53e4f', "20-29" = '#fc8d59', "30-39" = '#fee08b',
                      "40-49" = '#ffffbf', "50-59" = '#e6f598', "60-69" = '#99d594', "70+" = '#3288bd')



ggplot(performance_full, aes(x = age_group, y = fitness_ratio, fill = age_group)) +
  geom_boxplot() +
  scale_fill_manual(values = age_group_colors) +
  labs(
    title = "Relative Position by Gender verses Age Group",
    x = "Age Group",
    y = "Relative Position by Gender "
  ) +
  theme_minimal() +
  facet_wrap(~Sex, ncol = 1)


#-------- lm--------
# Separate data for males and females
male_data <- performance_full %>% filter(Sex == "M")
female_data <- performance_full %>% filter(Sex == "F")

# Randomly select 500 participants for each age group for males and fit a linear model
male_random_sample <- male_data %>%
  group_by(age_group) %>%
  sample_n(size = 100, replace = FALSE)

male_lm_model <- lm(fitness_ratio ~ Age, data = male_random_sample)

# Randomly select 500 participants for each age group for females and fit a linear model
female_random_sample <- female_data %>%
  group_by(age_group) %>%
  sample_n(size = 100, replace = FALSE)

female_lm_model <- lm(fitness_ratio ~ Age, data = female_random_sample)

# Summary of the linear models for males and females
summary(male_lm_model)
summary(female_lm_model)

# Scatter plot with regression line for males
ggplot(male_random_sample, aes(x = Age, y = fitness_ratio)) +
  geom_point(alpha=1, aes(fill=age_group), color="grey", shape = 21) +
  scale_fill_manual(values = age_group_colors) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  stat_poly_eq(use_label(c("eq","R2"))) +
  labs(
    title = "Scatter Plot and Regression Line for Males",
    x = "Age",
    y = "Relative Positon"
  )+
  theme_minimal()

# Scatter plot with regression line for females
ggplot(female_random_sample, aes(x = Age, y = fitness_ratio)) +
  geom_point(alpha=1, aes(fill=age_group), color="grey", shape = 21) +
  scale_fill_manual(values = age_group_colors) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  stat_poly_eq(use_label(c("eq","R2"))) +
  labs(
    title = "Scatter Plot and Regression Line for Females",
    x = "Age",
    y = "Relative Positon"
  )+
  theme_minimal()


# Residual plot for males
ggplot(male_lm_model, aes(x = .fitted, y = .resid)) +
  geom_point(color = '#3288bd', alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red")+
  labs(
    title = "Residual Plot for Males",
    x = "Fitted values",
    y = "Residuals"
  )+
  theme_minimal()

# Calculate R-squared for males
male_r_squared <- summary(male_lm_model)$r.squared
cat("R-squared for Males:", male_r_squared, "\n")


# Residual plot for females
ggplot(female_lm_model, aes(x = .fitted, y = .resid))  +
  geom_point(color = '#d53e4f', alpha = 0.5) +
  geom_hline(yintercept = 0, color = "blue")+
  labs(
    title = "Residual Plot for Females",
    x = "Fitted values",
    y = "Residuals"
  )+
  theme_minimal()

# Calculate R-squared for females
female_r_squared <- summary(female_lm_model)$r.squared
cat("R-squared for Females:", female_r_squared, "\n")

ols_plot_resid_qq(female_lm_model, print_plot = FALSE)+
  labs(title = "Normal Q-Q plot for Females")+
  theme_minimal()
ols_plot_resid_qq(male_lm_model, print_plot = FALSE)+
  labs(title = "Normal Q-Q plot for Males")+
  theme_minimal()
