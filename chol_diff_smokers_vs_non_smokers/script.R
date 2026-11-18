library(tidyverse)
library(nhanesA)
library(moments)
library(rstatix)
library(DescTools)
library(boot)
library(gtsummary)

# BETWEEN SMOKERS AND NON-SMOKERS, IS THERE A DIFFERENCE IN TOTAL CHOLESTEROL?

demo <- nhanes('DEMO_J')
smq <- nhanes('SMQ_J')
tchol <- nhanes('TCHOL_J')

df_clean_3 <- demo %>%
  #merge the smoking questionnaire table
  inner_join(smq, by = "SEQN") %>%
  #merge the cholesterol table
  inner_join(tchol, by = "SEQN") %>%
  #select desired columns
  select(id = SEQN, age = RIDAGEYR, smoker = SMQ040, chol = LBXTC) %>%
  # merge "everyday" and "somedays"
  mutate(smoking_status = case_when (
    smoker == "Every day" ~ "Smoker",
    smoker == "Some days" ~ "Smoker",
    smoker == "Not at all" ~ "Non-smoker",
    TRUE ~ NA_character_
  ), 
  smoking_status = factor(smoking_status)) %>%
  #drop values that have NA values in chol and smoking_status columns
  drop_na(chol, smoking_status)


#inspect
str(df_clean_3)
glimpse(df_clean_3)
summary(df_clean_3)

# from summary, minimum age is 18. no need to filter for adults

## EXPLORATORY DATA ANALYSIS
# find out if data is normally distributed or if there is a skew
ggplot(df_clean_3, aes(chol)) +
  geom_histogram() +
  facet_wrap(~smoking_status) +
  labs(title = "Total Cholesterol Levels of Smokers vs. Non-smokers")

# Q-Q plot to check for normality
ggplot(df_clean_3, aes(sample = chol)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~smoking_status) +
  labs(title = "Q-Q Plot of Smokers vs. Non-smokers") # skew present in both

# put a figure to the skew
df_clean_3_smokers <- df_clean_3[df_clean_3$smoking_status == "Smoker", ]
df_clean_3_non_smokers <- df_clean_3[df_clean_3$smoking_status == "Non-smoker", ]
skewness(df_clean_3_smokers$chol) # 1 - strong right skew
skewness(df_clean_3_non_smokers$chol) # 0.5 - moderate right skew

#boxplot, check for outliers
ggplot(df_clean_3, aes(smoking_status, chol)) +
  geom_boxplot() +
  labs(title = "Distribution of Total Cholesterol Levels in Smokers vs. Non-smokers")

#outliers present, identify them
Q1 <- quantile(df_clean_3$chol, 0.25)
Q3 <- quantile(df_clean_3$chol, 0.75)
IQR <- Q3 - Q1

outliers <- df_clean_3 %>%
  filter(chol < Q1 - 1.5 * IQR | chol > Q3 + 1.5 * IQR)

#summarize for both groups
summary_stats <- df_clean_3 %>%
  group_by(smoking_status) %>%
  summarize(
    n = n(),
    mean = mean(chol),
    sd = sd(chol),
    var = var(chol),
    median = median(chol),
    min = min(chol),
    max = max(chol)
  )

# check for differences in variance
var.test(chol ~ smoking_status, data = df_clean_3) #p-value = 0.952 - variances are not very different between groups

##THE TEST
# one group has a strong skew, the other has a moderate skew
# there are plausible outliers
# decision 1 - perform sensitivity analysis - run all tests (ttest, wilcox, bootstrap) and compare
# decision 2 - median bootstrap (not wilcox because the 2 groups have different levels of skew)
# t-test can still be done because n > 100 for each group, so the central limit theory can save it

# TEST 1 - t.test
t_res <- t.test(chol ~ smoking_status, data = df_clean_3)
# calculate effect size
t_eff_size <- df_clean_3 %>%
  cohens_d(chol ~ smoking_status)
  
t_res_table <- tibble(
  statistic = t_res$statistic,
  effect_size = t_eff_size$effsize,
  magnitude = t_eff_size$magnitude,
  p_value = t_res$p.value,
  p_sig = ifelse(t_res$p.value < 0.05, "Yes", "No")
)

# TEST 2 - wilcoxon rank sum test
wilcox_res_3 <- wilcox.test(chol ~ smoking_status, data = df_clean_3)
#calculate effect size
wilcox_eff_size <- df_clean_3 %>%
  wilcox_effsize(chol ~ smoking_status)

wilcox_table_3 <- tibble(
  statistic = wilcox_res_3$statistic,
  effect_size = wilcox_eff_size$effsize,
  magnitude = wilcox_eff_size$magnitude,
  p_value = wilcox_res_3$p.value,
  p_sig = ifelse(wilcox_res_3$p.value < 0.05, "Yes", "No")
)

# TEST 3 - median bootstrap
median_diff <- function(data, i) {
  d <- data[i, ]
  median(data$chol[d$smoking_status == "Smoker"]) - median(data$chol[d$smoking_status == "Non-smoker"])
}

boot_result <- boot(df_clean_3, median_diff, R = 1000)
boot.ci(boot_result, type = "perc") # (-4.0, 4.5) - not statistically significant
plot(boot_result)

### ALL TESTS SAY THAT THE DIFFERENCE BETWEEN THE MEANS/MEDIANS ARE NOT STATISTICALLY SIGNIFICANT
### THOUGH EFFECT SIZES SLIGHTLY VARY

# Run test with gt_summary
df_clean_3 %>%
  select(age, chol, smoking_status) %>%
  tbl_summary(by = "smoking_status") %>%
  add_p()
# gtsummary used the wilcoxon rank sum test
