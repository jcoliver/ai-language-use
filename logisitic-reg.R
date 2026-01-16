# Create observation-level data and run logistic regression
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-01-16

# Using aod package for Wald test
library(aod)

# In courses where AI is banned
# Count | AI Use | Native English
#   101 |     No |            Yes
#    10 |     No |             No
#    10 |    Yes |            Yes
#     4 |    Yes |             No

no_ai_counts <- data.frame(count = c(101, 10, 10, 4),
                           ai_use = c(0, 0, 1, 1),
                           native_eng = c(1, 0, 1, 0))

# In courses where AI is included
# Count | AI Use | Native English
#    43 |     No |            Yes
#     3 |     No |             No
#   157 |    Yes |            Yes
#    14 |    Yes |             No

ai_counts <- data.frame(count = c(43, 3, 157, 14),
                        ai_use = c(0, 0, 1, 1),
                        native_eng = c(1, 0, 1, 0))

# Define a function to convert our contingency table to individual observations
# From http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/#countstocases-function
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}

# Create the individual-level data and renumber rows for both data sets
no_ai_obs <- countsToCases(no_ai_counts, countcol = "count")
rownames(no_ai_obs) <- NULL

ai_obs <- countsToCases(ai_counts, countcol = "count")
rownames(ai_obs) <- NULL

# Reality checks
table(no_ai_obs)
table(ai_obs)

# Run logistic regression
# More info at https://stats.oarc.ucla.edu/r/dae/logit-regression/

# In courses where AI is banned
no_ai_model <- glm(formula = ai_use ~ native_eng,
                family = binomial,
                data = no_ai_obs)
summary(no_ai_model)
# Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  -0.9163     0.5916  -1.549   0.1214  
# native_eng   -1.3962     0.6782  -2.059   0.0395 *

wald.test(b = coef(no_ai_model), 
          Sigma = vcov(no_ai_model), 
          Terms = 2:2)
# Chi-squared test:
#   X2 = 4.2, df = 1, P(> X2) = 0.04

# Run logistic regression
# In courses where AI is banned
ai_model <- glm(formula = ai_use ~ native_eng,
                family = binomial,
                data = ai_obs)
summary(ai_model)
# Estimate Std. Error z value Pr(>|z|)  
# (Intercept)   1.5404     0.6362   2.421   0.0155 *
#   native_eng   -0.2454     0.6591  -0.372   0.7096  

wald.test(b = coef(ai_model), 
          Sigma = vcov(ai_model), 
          Terms = 2:2)
# Chi-squared test:
#   X2 = 0.14, df = 1, P(> X2) = 0.71

# Reality check. Convert language background to logical and check to see that 
# the results are identical for non-AI course
no_ai_obs_tf <- no_ai_obs
no_ai_obs_tf$native_eng <- no_ai_obs$native_eng == 1
summary(glm(formula = ai_use ~ native_eng,
    family = binomial,
    data = no_ai_obs_tf))
