# =============================================================================
# Part 1: Simulating Sampling — Good and Bad
# Working with Surveys — E1493, Data Journalism, Simon Munzert
# =============================================================================

library(tidyverse)
set.seed(42)


# -----------------------------------------------------------------------------
# Creating a population
# -----------------------------------------------------------------------------

# Simulate a German-like population of 100,000 adults.
# Each person has an age group, a region (East/West), an education level,
# and an opinion on a policy question ("support for four-day work week").
# Support depends on demographics: younger and more educated people
# are more supportive.

N <- 100000

population <- tibble(
  id = 1:N,
  age_group = sample(
    c("18-29", "30-49", "50-64", "65+"),
    N, replace = TRUE,
    prob = c(0.15, 0.30, 0.28, 0.27)
  ),
  region = sample(
    c("West", "East"),
    N, replace = TRUE,
    prob = c(0.80, 0.20)
  ),
  education = sample(
    c("low", "medium", "high"),
    N, replace = TRUE,
    prob = c(0.30, 0.45, 0.25)
  )
) |>
  mutate(
    # True support probability depends on demographics
    p_support = case_when(
      age_group == "18-29" & education == "high" ~ 0.78,
      age_group == "18-29" & education == "medium" ~ 0.68,
      age_group == "18-29" & education == "low" ~ 0.55,
      age_group == "30-49" & education == "high" ~ 0.62,
      age_group == "30-49" & education == "medium" ~ 0.52,
      age_group == "30-49" & education == "low" ~ 0.40,
      age_group == "50-64" & education == "high" ~ 0.45,
      age_group == "50-64" & education == "medium" ~ 0.35,
      age_group == "50-64" & education == "low" ~ 0.28,
      age_group == "65+" & education == "high" ~ 0.32,
      age_group == "65+" & education == "medium" ~ 0.22,
      age_group == "65+" & education == "low" ~ 0.18,
    ),
    support = rbinom(N, 1, p_support)
  )

# The true population parameter — every sample estimate should try
# to get close to this number
(true_support <- mean(population$support))


# -----------------------------------------------------------------------------
# Simple random sampling
# -----------------------------------------------------------------------------

# Draw a simple random sample of n = 1,000 and compute the estimate
# with a 95% confidence interval

n <- 1000
srs <- population |> slice_sample(n = n)

p_hat <- mean(srs$support)
se <- sqrt(p_hat * (1 - p_hat) / n)
ci <- p_hat + c(-1.96, 1.96) * se

tibble(
  method = "SRS (n=1000)",
  estimate = p_hat,
  se = se,
  ci_low = ci[1],
  ci_high = ci[2],
  true_value = true_support,
  covers_truth = ci[1] <= true_support & true_support <= ci[2]
)


# -----------------------------------------------------------------------------
# Repeating the experiment
# -----------------------------------------------------------------------------

# One sample is one draw. Repeat 1,000 times and see how the
# sampling distribution behaves.

srs_results <- map_dbl(1:1000, \(i) {
  s <- population |> slice_sample(n = 1000)
  mean(s$support)
})

tibble(estimate = srs_results) |>
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 40, fill = "#1D375B", alpha = 0.8) +
  geom_vline(xintercept = true_support, color = "#C2001A", linewidth = 1) +
  labs(
    title = "Sampling distribution under SRS (n = 1,000)",
    subtitle = paste0("Red line = true population value (", round(true_support, 3), ")"),
    x = "Sample proportion", y = "Count"
  ) +
  theme_minimal(base_size = 14)

# The distribution is centered on the truth — that's what
# unbiasedness looks like.


# -----------------------------------------------------------------------------
# A biased sample: simulating opt-in self-selection
# -----------------------------------------------------------------------------

# What happens when participation is not random? Suppose younger,
# more educated people are much more likely to respond (as in many
# online opt-in surveys).

population_with_response <- population |>
  mutate(
    p_respond = case_when(
      age_group == "18-29" & education == "high" ~ 0.15,
      age_group == "18-29" & education == "medium" ~ 0.10,
      age_group == "18-29" & education == "low" ~ 0.04,
      age_group == "30-49" & education == "high" ~ 0.12,
      age_group == "30-49" & education == "medium" ~ 0.07,
      age_group == "30-49" & education == "low" ~ 0.03,
      age_group == "50-64" & education == "high" ~ 0.08,
      age_group == "50-64" & education == "medium" ~ 0.04,
      age_group == "50-64" & education == "low" ~ 0.02,
      age_group == "65+" & education == "high" ~ 0.05,
      age_group == "65+" & education == "medium" ~ 0.02,
      age_group == "65+" & education == "low" ~ 0.01,
    ),
    responds = rbinom(N, 1, p_respond)
  )

biased_sample <- population_with_response |> filter(responds == 1)

cat("Biased sample size:", nrow(biased_sample), "\n")
cat("Biased estimate:", round(mean(biased_sample$support), 3), "\n")
cat("True value:", round(true_support, 3), "\n")
cat("Bias:", round(mean(biased_sample$support) - true_support, 3), "\n")

# Despite a large sample, the biased estimate is far from the truth.


# -----------------------------------------------------------------------------
# The big data paradox: sample size vs. bias
# -----------------------------------------------------------------------------

# What happens as we grow the biased sample? Key insight: the CI
# shrinks, but it narrows around the *wrong value*.

biased_increasing <- map_dfr(c(100, 500, 1000, 5000, 10000, 50000), \(target_n) {
  pool <- population_with_response |> filter(responds == 1)
  s <- pool |> slice_sample(n = min(target_n, nrow(pool)))
  p_hat <- mean(s$support)
  se <- sqrt(p_hat * (1 - p_hat) / nrow(s))
  tibble(
    n = nrow(s),
    estimate = p_hat,
    ci_low = p_hat - 1.96 * se,
    ci_high = p_hat + 1.96 * se
  )
})

ggplot(biased_increasing, aes(x = factor(n), y = estimate)) +
  geom_point(size = 3, color = "#1D375B") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "#1D375B") +
  geom_hline(yintercept = true_support, color = "#C2001A", linewidth = 1, linetype = "dashed") +
  labs(
    title = "The Big Data Paradox: more data, more confident — and more wrong",
    subtitle = "Dashed red = true value. CI narrows but doesn't cover the truth.",
    x = "Sample size (biased)", y = "Estimated support"
  ) +
  theme_minimal(base_size = 14)

# KEY TAKEAWAY: The CI formula p_hat ± 1.96*sqrt(p_hat*(1-p_hat)/n) assumes
# random sampling. Applied to a biased sample, it produces intervals that are
# narrow and wrong — the most dangerous combination.


# -----------------------------------------------------------------------------
# Finite population correction (FPC)
# -----------------------------------------------------------------------------

# Shouldn't we account for the fact that a survey of 50,000 covers half
# our population of 100,000? For truly random samples, the FPC adjusts
# the SE by sqrt((N-n)/N). But at realistic population sizes it's irrelevant.

fpc_table <- tibble(
  label = c(
    "n=1,000 of N=100,000",
    "n=10,000 of N=100,000",
    "n=50,000 of N=100,000",
    "n=80,000 of N=100,000",
    "n=90,000 of N=100,000",
    "n=95,000 of N=100,000",
    "n=1,000 of N=60,000,000",
    "n=10,000 of N=60,000,000",
    "n=50,000 of N=60,000,000"
  ),
  n = c(1000, 10000, 50000, 80000, 90000, 95000, 1000, 10000, 50000),
  N_pop = c(rep(100000, 6), rep(60000000, 3))
) |>
  mutate(
    sampling_fraction = n / N_pop,
    fpc_factor = sqrt((N_pop - n) / N_pop),
    se_without_fpc = sqrt(0.5 * 0.5 / n),    # worst-case p = 0.5
    se_with_fpc = se_without_fpc * fpc_factor,
    reduction_pct = round((1 - fpc_factor) * 100, 2)
  )

fpc_table |>
  select(label, sampling_fraction, fpc_factor, reduction_pct) |>
  rename(
    Scenario = label,
    `Sampling fraction` = sampling_fraction,
    `FPC factor` = fpc_factor,
    `SE reduction (%)` = reduction_pct
  )

# For the real world (N = 60 million), even n = 50,000 reduces the SE
# by less than 0.05%. The FPC is irrelevant at realistic population sizes.


# -----------------------------------------------------------------------------
# FPC vs. bias: the comparison that matters
# -----------------------------------------------------------------------------

comparison_fpc <- tibble(
  n = c(100, 500, 1000, 5000, 10000, 50000)
) |>
  mutate(
    # SE with FPC for a random sample (p = true_support, N = 100,000)
    se_random_fpc = sqrt(true_support * (1 - true_support) / n) *
                    sqrt((100000 - n) / 100000),
    # Actual error from biased samples
    bias_error = map_dbl(n, \(target_n) {
      pool <- population_with_response |> filter(responds == 1)
      s <- pool |> slice_sample(n = min(target_n, nrow(pool)))
      abs(mean(s$support) - true_support)
    })
  )

ggplot(comparison_fpc, aes(x = n)) +
  geom_line(aes(y = se_random_fpc, color = "Random sample (SE with FPC)"),
            linewidth = 1) +
  geom_point(aes(y = se_random_fpc, color = "Random sample (SE with FPC)"),
             size = 3) +
  geom_line(aes(y = bias_error, color = "Biased sample (actual error)"),
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = bias_error, color = "Biased sample (actual error)"),
             size = 3) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c(
    "Random sample (SE with FPC)" = "#1D375B",
    "Biased sample (actual error)" = "#C2001A"
  )) +
  labs(
    title = "The finite population correction doesn't save biased samples",
    subtitle = "Random sample error shrinks with n; biased error stays flat",
    x = "Sample size (log scale)", y = "Error",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# BOTTOM LINE: Sample size beyond ~1,500 buys you very little additional
# precision — and no amount of additional biased data compensates for
# non-random selection. A Civey poll of 10,000 self-selected online
# respondents is not automatically better than an infratest dimap
# telephone poll of 1,200.
