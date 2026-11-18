library(tidyverse)
library(nhanesA)
library(moments)
library(rstatix)

#nhanes tables by category
#nhanesTables('DEMO', 2018)  # 2018 demographics info
#nhanesTables('DIET', 2018) # 2018 diet info
#nhanesTables('EXAM', 2018) # 2018 examinations table
#nhanesTables('LAB', 2018) # 2018 laboratory info
#nhanesTables('Q', 2018) # 2018 laboratory info

# QUESTION: DO MEN AND WOMEN DIFFER IN MEAN SYSTOLIC BLOOD PRESSURE?
 
# demographics - DEMO_J, blood pressure - BPX_J, body measurements - BMX_J, cholesterol - TCHOL_J
demo <- nhanes('DEMO_J')
bpx <- nhanes('BPX_J')

demo_bpx <- demo %>%
  inner_join(bpx, by = "SEQN")

df <- demo_bpx %>%
  select(id = SEQN, age = RIDAGEYR, sex = RIAGENDR, systolic_bp = BPXSY1)

#inspect
glimpse(df)
summary(df)

# filter for adults (18 and above), drop rows with NA values
df <- df %>%
  filter(age >= 18) %>%
  drop_na(systolic_bp)


### EXPLORATORY DATA ANALYSIS
# 1. Check if distribution normal for both sexes
ggplot(df, aes(systolic_bp)) +
  geom_histogram() +
  facet_wrap(~sex) +
  labs(title = "Systolic Blood Pressure Distribution by Sex")
# not normally distributed - both are right skewed 

# skewed so do the skew test: -0.5-0.5 - okay (proceed with t-test), 0.5-1 - moderate skew (transform the data), >1 - strong skew (use a non-parametric test)
skewness(df$systolic_bp) # strong skew

# qq plot - to check for normality: if points follow the line, approximately normal, if not, skewed
ggplot(df, aes(sample = systolic_bp)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~sex) +
  labs(title = "Q-Q plot of BMI by Sex")  # Q-Q plot shows a skew


# 2. Compare groups, find out if there are outliers with box plots
ggplot(df, aes(sex, systolic_bp)) + 
  geom_boxplot() +
  labs(title = "Systolic BP by Sex")
# looks like there are outliers and there is a slight difference in median

# identify the outliers explicitly and ask if outlier is plausible
Q1 <- quantile(df$systolic_bp, 0.25)
Q3 <- quantile(df$systolic_bp, 0.75)
IQR <- Q3 - Q1

outliers <- df %>%
  filter(systolic_bp < Q1 - 1.5 * IQR | systolic_bp > Q3 + 1.5 * IQR)
# all outliers looks plausible. Keep them because dataset is large


# summary statistics
summary_stats <- df %>%
  group_by(sex) %>%
  summarize(n = n(),
            mean = mean(systolic_bp),
            sd = sd(systolic_bp),
            median = median(systolic_bp),
            min = min(systolic_bp),
            max = max(systolic_bp))



# check equality fo variances
var.test(systolic_bp ~ sex, data = df)
# p-value < 0.05 - variances are unequal ---- Welch t test preferred\


#### Test for differences
# Mann Whitney U / Wilcoxon Rank sum test will be used because:
# 1. histograms, Q-Q plot and skewness test suggested a strong skew
# 2. there were a number of plausible outliers. They will be kept
wilcox_result <- wilcox.test(systolic_bp ~ sex, data = df)
#inspect
str(wilcox_result)
names(wilcox_result)

#extract individual values
wilcox_result$statistic
wilcox_result$p.value
wilcox_result$method
wilcox_result$alternative

# get the effect size (rank-biserial correlation) - r < 0.1 - negligible, 0.1-0.3 - small, 0.3-0.5 - moderate, >0.5 - large
eff_size <- df %>%
  wilcox_effsize(systolic_bp ~ sex)

wilcox_table <- tibble(
  W = wilcox_result$statistic,
  p_value = wilcox_result$p.value,
  p_sig = ifelse(wilcox_result$p.value < 0.05, "Yes", "No"),
  alternative = wilcox_result$alternative
  )
# report median, iqr and n() alongside - like in summary_stats above

