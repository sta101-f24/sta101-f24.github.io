# ==============================================================================
# read in data
# ==============================================================================

library(tidyverse)

# ==============================================================================
# read in data
# ==============================================================================

voting_survey <- read_csv("data/voting-survey.csv")

voting_survey <- voting_survey |>
  mutate(vote = if_else(vote == "Certain to vote", "Certain", "Uncertain"))

n <- nrow(voting_survey)

# ==============================================================================
# example of bootstrap sample
# ==============================================================================

cat("\014")  
new_survey <- sample(voting_survey$vote, n, replace = TRUE)
head(new_survey)
mean(new_survey == "Certain")

# ==============================================================================
# do this a bunch of times
# ==============================================================================

reps <- 5000
set.seed(8675309)
new_surveys <- data.frame(matrix(sample(voting_survey$vote, n * reps, replace = TRUE), nrow = n, ncol = reps))
colnames(new_surveys) <- paste("survey_", 1:reps, sep = "")
props <- matrix(colMeans(new_surveys == "Certain"), reps, 1)
head(props, n = 10)

# ==============================================================================
# plot the bootstrap distribution
# ==============================================================================

par(mar = c(5, 0.5, 5, 0.5))
hist(props, breaks = "Scott", col = "salmon", freq = F,
     main = "Bootstrap approximation to the sampling distirbution", yaxt = "n", ylab = "",
     xlab = "sample proportion")

# ==============================================================================
# add normal distribution
# ==============================================================================

sample_prop <- mean(voting_survey$vote == "Certain")

x_vals <- seq(0, 1, length.out = 1000)
y_vals <- dnorm(x_vals, mean = sample_prop,
                sd = sqrt(sample_prop * (1 - sample_prop) / n))

lines(x_vals, y_vals, lwd = 3)
