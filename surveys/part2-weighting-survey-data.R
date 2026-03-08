# =============================================================================
# Part 2: Weighting Survey Data
# Working with Surveys — E1493, Data Journalism, Simon Munzert
# =============================================================================

# This script builds on Part 1. Run part1-simulating-sampling.R first to
# create the population, biased_sample, and true_support objects.

library(tidyverse)
library(survey)
library(srvyr)


# -----------------------------------------------------------------------------
# Computing weights manually
# -----------------------------------------------------------------------------

# True population margins
pop_margins <- population |>
  count(age_group) |>
  mutate(pop_prop = n / sum(n)) |>
  select(age_group, pop_prop)

# Sample margins
sample_margins <- biased_sample |>
  count(age_group) |>
  mutate(sample_prop = n / sum(n)) |>
  select(age_group, sample_prop)

# Compute post-stratification weights
weight_table <- pop_margins |>
  left_join(sample_margins, by = "age_group") |>
  mutate(weight = pop_prop / sample_prop)

weight_table


# -----------------------------------------------------------------------------
# Applying weights
# -----------------------------------------------------------------------------

biased_weighted <- biased_sample |>
  left_join(weight_table |> select(age_group, weight), by = "age_group")

# Weighted estimate
weighted_est <- sum(biased_weighted$support * biased_weighted$weight) /
                sum(biased_weighted$weight)

cat("Unweighted estimate:", round(mean(biased_sample$support), 3), "\n")
cat("Weighted by age:", round(weighted_est, 3), "\n")
cat("True value:", round(true_support, 3), "\n")

# Weighting by age alone improves the estimate, but may not fully correct
# the bias — because education also matters. In practice, pollsters weight
# on multiple variables simultaneously using raking.


# -----------------------------------------------------------------------------
# Weighting with the survey package
# -----------------------------------------------------------------------------

# The survey package (and its tidyverse-friendly wrapper srvyr) is the
# standard tool for analyzing weighted survey data in R. It correctly
# adjusts standard errors for the weighting.

biased_design <- biased_weighted |>
  as_survey_design(weights = weight)

# Weighted mean with correct SE
result <- biased_design |>
  summarize(
    support = survey_mean(support, vartype = "ci")
  )

result

# WHY USE THE SURVEY PACKAGE?
# When you compute a weighted mean "by hand", you get the point estimate
# right. But the SE needs to account for the weighting — observations with
# larger weights contribute more variance. The survey package handles this
# automatically, producing correct (wider) confidence intervals.


# -----------------------------------------------------------------------------
# Raking: weighting on multiple dimensions
# -----------------------------------------------------------------------------

# In practice, pollsters use raking (iterative proportional fitting) to
# match multiple margins simultaneously.

pop_age <- data.frame(
  age_group = c("18-29", "30-49", "50-64", "65+"),
  Freq = c(0.15, 0.30, 0.28, 0.27) * nrow(biased_sample)
)

pop_education <- data.frame(
  education = c("low", "medium", "high"),
  Freq = c(0.30, 0.45, 0.25) * nrow(biased_sample)
)

pop_region <- data.frame(
  region = c("West", "East"),
  Freq = c(0.80, 0.20) * nrow(biased_sample)
)

# Start with an unweighted design
unweighted_design <- svydesign(ids = ~1, data = biased_sample)

# Rake to match all three margins
raked_design <- rake(
  unweighted_design,
  sample.margins = list(~age_group, ~education, ~region),
  population.margins = list(pop_age, pop_education, pop_region)
)

# Raked estimate
svymean(~support, raked_design)


# -----------------------------------------------------------------------------
# Compare all estimates
# -----------------------------------------------------------------------------

comparison <- tibble(
  Method = c("True value", "Unweighted (biased)", "Weighted by age only",
             "Raked (age + edu + region)"),
  Estimate = c(
    true_support,
    mean(biased_sample$support),
    weighted_est,
    as.numeric(svymean(~support, raked_design))
  )
)

comparison

# Weighting reduces bias, but cannot eliminate it entirely if the response
# mechanism depends on variables you're not weighting on, or if entire
# groups are missing from the sample. Weighting is correction, not magic.


# -----------------------------------------------------------------------------
# A major caveat: why weighting works so well here
# -----------------------------------------------------------------------------

# In our simulation, raking gets very close to the truth. That's because
# we built a world where support depends on age, education, and region,
# and then weighted on exactly those variables.
#
# In reality, we almost never know:
# - Which variables drive both self-selection and the outcome
# - Whether the relationship between demographics and opinions is the same
#   among respondents and non-respondents
# - Whether our census benchmarks are right
#
# To see this, let's add a hidden variable that affects both response and
# support — but that we *can't* weight on.

pop_hidden <- population_with_response |>
  mutate(
    # Political engagement: correlated with education but not identical
    engagement = case_when(
      education == "high" ~ rbinom(n(), 1, 0.6),
      education == "medium" ~ rbinom(n(), 1, 0.35),
      education == "low" ~ rbinom(n(), 1, 0.15)
    ),
    # Engaged people are more likely to support AND more likely to respond
    support_v2 = rbinom(n(), 1, pmin(1, p_support + 0.12 * engagement)),
    p_respond_v2 = case_when(
      engagement == 1 ~ p_respond * 3,
      TRUE ~ p_respond * 0.5
    ) |> pmin(1),
    responds_v2 = rbinom(n(), 1, p_respond_v2)
  )

true_support_v2 <- mean(pop_hidden$support_v2)
biased_v2 <- pop_hidden |> filter(responds_v2 == 1)

# Rake on the same variables as before
pop_age_v2 <- data.frame(
  age_group = c("18-29", "30-49", "50-64", "65+"),
  Freq = c(0.15, 0.30, 0.28, 0.27) * nrow(biased_v2)
)
pop_edu_v2 <- data.frame(
  education = c("low", "medium", "high"),
  Freq = c(0.30, 0.45, 0.25) * nrow(biased_v2)
)
pop_reg_v2 <- data.frame(
  region = c("West", "East"),
  Freq = c(0.80, 0.20) * nrow(biased_v2)
)

raked_v2 <- rake(
  svydesign(ids = ~1, data = biased_v2),
  sample.margins = list(~age_group, ~education, ~region),
  population.margins = list(pop_age_v2, pop_edu_v2, pop_reg_v2)
)

tibble(
  Method = c("True value", "Unweighted (biased)",
             "Raked (age + edu + region)"),
  Estimate = c(
    true_support_v2,
    mean(biased_v2$support_v2),
    as.numeric(svymean(~support_v2, raked_v2))
  )
) |>
  mutate(Residual_bias = Estimate - true_support_v2)

# Raking still helps, but a residual bias remains — because the hidden
# confounder (political engagement) drives both selection and opinion,
# and we can't weight on it. This is the situation pollsters face every day.
#
# THE LIMITS OF WEIGHTING:
# Weighting corrects for *observable* imbalances (too few old people, too
# many graduates) but cannot fix *unobservable* ones (too many politically
# engaged people, too few people who distrust surveys). This is why
# non-probability samples remain controversial. The Pew 2023 benchmarking
# study found that even after extensive weighting, opt-in panels had roughly
# twice the average error of probability-based panels.
