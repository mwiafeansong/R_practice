library(tidyverse)
library(nhanesA)
library(moments)
library(rstatix)

demo <- nhanes("DEMO_J")
diq <- nhanes("DIQ_J")
bmx <- nhanes("BMX_J")

# AMONG ADULTS, IS BMI SIGNIFICANTLY DIFFERENT 
# merge and select columns of interest
df_clean <- demo %>%
  inner_join(diq, by = "SEQN") %>%
  inner_join(bmx, by = "SEQN") %>%
  #select columns of interest
  select(id = SEQN, age = RIDAGEYR, diabetes = DIQ010, bmi = BMXBMI) %>%
  # filter for only adults - 18 and above
  filter(age >= 18) %>%
  # convert diabetes status to diabetic, non-diabetic
  mutate(diabetes_status = case_when(
    diabetes == 'Yes' ~ "Diabetic",
    diabetes == "No" ~ "Non-diabetic",
    TRUE ~ NA_character_ #this excludes borderline, refused, dont know)
  ),
  # convert new column type into factor
  diabetes_status = factor(diabetes_status)
  ) %>%
  # drop rows that have NA values in diabetes_status column
  drop_na(diabetes_status, bmi)

# inspect
str(df_clean)
glimpse(df_clean)
summary(df_clean)


### EXPLORATORY DATA ANALYSIS
## CHECK FOR NORMALITY/SKEW
# visualize pattern - are they normal distributions?
ggplot(df_clean, aes(bmi)) +
  geom_histogram() +
  facet_wrap(~diabetes_status) +
  labs(title = "Distribution of BMI by Diabetes Status")
# visually, there seems to be a right skwe in both distributions

# use Q-Q plot to check for skweness
ggplot(df_clean, aes(sample = bmi)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~diabetes_status) +
  labs(title = "Q-Q Plot of BMI in Diabetics vs. Non-Diabetics")
# looks skewed in both

# check for skewness - gives a numerical value
df_diabetics <- df_clean[df_clean$diabetes_status == "Diabetic", ]
# unique(df_diabetics$diabetes_status)   # check to see if diabetes_status only has "Diabetic" as value
df_non_diabetics <- df_clean[df_clean$diabetes_status == "Non-diabetic", ]
#unique(df_non_diabetics$diabetes_status)
skewness(df_diabetics$bmi) # 1.5 - strong skew
skewness(df_non_diabetics$bmi) # 1.2 - strong skew


## Check for outliers
ggplot(df_clean, aes(diabetes_status, bmi)) +
  geom_boxplot() +
  labs(title = "BMI by Diabetes Status")
#visually there seem to be outliers

# identify the outliers, see if they are plausible
Q1 <- quantile(df_clean$bmi, 0.25)
Q3 <- quantile(df_clean$bmi, 0.75)
IQR <- Q3 - Q1

outliers <- df_clean %>%
  filter(bmi < Q1 - 1.5 * IQR | bmi > Q3 + 1.5 * IQR)


## calculate summary statistics per group
summary_stats <- df_clean %>%
  group_by(diabetes_status) %>%
  summarize(n = n(),
            mean = mean(bmi),
            sd = sd(bmi),
            var = var(bmi),
            median = median(bmi),
            min = min(bmi),
            max = max(bmi))

# check if variances differ
var.test(bmi ~ diabetes_status, data= df_clean) #p_value = 0.001 - variances are unequal


### PERFORM THE TEST
# Wilcoxon rank sum test chosen because
# 1. data is strongly skewed, 2. there are significant outliers
wilcox_res <- wilcox.test(bmi ~ diabetes_status, data = df_clean)

# get effect size
effect_size <- df_clean %>%
  wilcox_effsize(bmi ~ diabetes_status)  # 0.164 - small magnitude

#summary table
summ_table <- tibble(
  W = wilcox_res$statistic,
  effect_size = effect_size$effsize,
  p_value = wilcox_res$p.value,
  p_sig = ifelse(wilcox_res$p.value < 0.05, "Yes", "No"),
  alternative = wilcox_res$alternative 
)

