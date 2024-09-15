#The treatment effect is then calculated as the difference in the 
#average outcomes between the two groups.
library(dplyr)

data <- read.csv("nsw_dw.csv")
data1 <- read.csv("nsw_cps.csv")


# question 1

'treat = 0 are individuals who did not get any treatment while 
treat= 1 are individuals who got the treatment'
mean_earnings_treatment <- mean(data$re78[data$treat == 1], na.rm = TRUE)

# Calculate average earnings for control group
mean_earnings_control <- mean(data$re78[data$treat == 0], na.rm = TRUE)

# Calculate treatment effect (simple difference in means)
treatment_effect <- mean_earnings_treatment - mean_earnings_control

treatment_effect

# question 2 

'Create such a data set by keeping only the treated
individuals from nsw_dw.csv and merge the data with the observations from the CPS
in nsw_cps.csv'

treated_data <- data %>% filter(treat == 1)

# Merge the treated data with CPS data based on the common identifier
combined_data <- rbind(data1, treated_data)
combined_data

# question 3 
'Compute the simple difference in observed outcomes for treated and control individuals
in your merged data set. How does it compare to the treatment effect estimate from 
the experimental data?'

# Calculate the mean outcome for treated individuals
mean_outcome_treated <- mean(combined_data$re78[combined_data$treat == 1], na.rm = TRUE)

# Calculate the mean outcome for control individuals
mean_outcome_control <- mean(combined_data$re78[combined_data$treat == 0], na.rm = TRUE)

# Compute the simple difference in means
simple_difference <- mean_outcome_treated - mean_outcome_control

simple_difference

# question 4

'Now, use a propensity score matching estimator on the observational data to estimate
the treatment effect of the training program on earnings in 1978. Estimate the propensity
score with a binary logit regression with the following predictors: a cubic polynomial in
age, a quadratic polynomial in education, marriage, no degree, black, hips, re74, re65,
two dummies for whether the individual was unemployed in 1974 and 1975, respectively,
an interaction term between education and earnings in 1974.'

logit_formula <- formula(treat ~ poly(age, 3) + poly(educ, 2) + marr + nodegree + black + hisp + re74 + re75 + educ:re74)

ps_model <- glm(logit_formula, data = data, family = binomial(link = 'logit'))
ps_model

# the dataset is added with a new column containing propensity score
data$propensity_score <- predict(ps_model, type = 'response')


# Install and load necessary packages
library(ggplot2)

# Plot histogram of propensity scores
ggplot(data, aes(x = propensity_score, fill = factor(treat))) +
  geom_histogram(binwidth = 0.05, position = "identity", alpha = 0.7) +
  labs(title = "Histogram of Propensity Scores",
       x = "Propensity Score",
       y = "Frequency",
       fill = "Treatment") +
  theme_minimal()

# Separate histograms for treatment and control groups
ggplot(data, aes(x = propensity_score)) +
  geom_histogram(binwidth = 0.02, fill = "blue", alpha = 0.7, subset = (treat ==1)) +
  labs(title = "Histogram of Propensity Scores - Treatment Group",
       x = "Propensity Score",
       y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = propensity_score)) +
  geom_histogram(binwidth = 0.02, fill = "red", alpha = 0.7, subset = (treat == 0)) +
  labs(title = "Histogram of Propensity Scores - Control Group",
       x = "Propensity Score",
       y = "Frequency") +
  theme_minimal()

# Check summary statistics of propensity scores
summary(data$propensity_score)

# Plot kernel density of propensity scores
ggplot(data, aes(x = propensity_score, fill = factor(treat))) +
  geom_density(alpha = 0.7) +
  labs(title = "Kernel Density Plot of Propensity Scores",
       x = "Propensity Score",
       y = "Density",
       fill = "Group") +
  theme_minimal()


'q5: Use your estimated propensity scores to compute an average of the treatment effect
of the job training program. Discuss and implement two different approaches: (1)
inverse probability weighting and (2) nearest-neighbor matching. Compare the two
estimates and discuss how they compare to the estimate that you computed from the
experimental data'

# (1) inverse probability weighting
trimmed_data <- data[data$propensity_score > 0.1 & data$propensity_score < 0.9, ]
weights <- ifelse(trimmed_data$treat == 1, 1 / trimmed_data$propensity_score, 1 / (1 - trimmed_data$propensity_score))
treatment_effect_ipw <- weighted.mean(trimmed_data$re78 * trimmed_data$treat, weights)
treatment_effect_ipw



#(2) nearest-neighbor matching
# Install the MatchIt package
# install.packages("MatchIt")

library(MatchIt)
# we have to match the propensity scores of treat =0 and treat=1 values for nearest-neighbor
#matching. 

match_result <- matchit(treat ~ propensity_score, data = data, method = "nearest")
matched_data <- match.data(match_result)
treatment_effect_matching <- mean(matched_data$re78)
treatment_effect_matching


# Empirical Results
c(treatment_effect, simple_difference, treatment_effect_ipw, treatment_effect_matching)

