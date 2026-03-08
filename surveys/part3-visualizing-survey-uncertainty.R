# =============================================================================
# Part 3: Visualizing Survey Uncertainty
# Working with Surveys — E1493, Data Journalism, Simon Munzert
# =============================================================================

# As data journalists, how you *present* survey results matters as much as
# how you compute them. This script focuses on communicating uncertainty
# honestly. Run part1-simulating-sampling.R first.

library(tidyverse)
library(colorspace)


# -----------------------------------------------------------------------------
# Simulated poll data: Sonntagsfrage
# -----------------------------------------------------------------------------

parties <- tibble(
  party = c("CDU/CSU", "SPD", "Grüne", "AfD", "FDP", "BSW", "Linke"),
  support = c(0.26, 0.15, 0.12, 0.24, 0.05, 0.03, 0.1),
  n = 1200  # typical poll sample size
) |>
  mutate(
    se = sqrt(support * (1 - support) / n),
    ci_low = support - 1.96 * se,
    ci_high = support + 1.96 * se
  )

# Party colors for consistent use
party_colors <- c(
  "CDU/CSU" = "#000000", "AfD" = "#009EE0", "SPD" = "#E3000F",
  "Grüne" = "#64A12D", "BSW" = "#7B2D8B", "FDP" = "#FFED00",
  "Linke" = "#BE3075"
)


# -----------------------------------------------------------------------------
# Bar plot with uncertainty bands
# -----------------------------------------------------------------------------

# The bar chart is the most common format for Sonntagsfrage results.
# Most outlets show just the bars — no uncertainty. Adding error bars
# is straightforward.

ggplot(parties, aes(x = reorder(party, -support), y = support, fill = party)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2, linewidth = 0.6, color = "gray30") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "gray50",
             linewidth = 0.5) +
  annotate("text", x = 7.3, y = 0.058, label = "5% threshold",
           size = 3, color = "gray50", hjust = 1) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0),
                     limits = c(0, 0.36)) +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Sonntagsfrage with 95% confidence intervals",
    subtitle = paste0(
      "n = ", parties$n[1],
      ". The error bars show: a party polling at 5% could be at 3.5–6.5%."
    ),
    x = NULL, y = "Vote share"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Notice: the FDP bar overlaps with the 5% threshold. A journalist
# reporting "FDP at 5%" should note the party could plausibly be
# anywhere between ~3.5% and ~6.5%.


# -----------------------------------------------------------------------------
# Dot plots with simulated polling results
# -----------------------------------------------------------------------------

# Dot plots are more reduced than bar plots — they focus on the point
# estimate and give more room for context like uncertainty or other polls.
# Very much in the spirit of Tufte's data-ink ratio.

n_outcomes <- 100
set.seed(2024)

sim_outcomes <- parties |>
  rowwise() |>
  mutate(
    sim_results = list(
      rnorm(n_outcomes, mean = support,
            sd = sqrt(support * (1 - support) / n))
    )
  ) |>
  unnest(sim_results) |>
  mutate(sim_id = rep(1:n_outcomes, times = nrow(parties)))

# Track FDP's chances of clearing 5%
fdp_sims <- sim_outcomes |> filter(party == "FDP")
fdp_in_pct <- round(mean(fdp_sims$sim_results >= 0.05) * 100)

ggplot(sim_outcomes, aes(x = sim_results,
                         y = reorder(party, support),
                         color = party)) +
  geom_jitter(alpha = 0.35, size = 1.5, height = 0.15) +
  # Poll point estimate as large diamond
  geom_point(data = parties,
             aes(x = support, y = reorder(party, support)),
             size = 4, shape = 21, color = "black", fill = "white") +
  # 5% threshold
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "gray50",
             linewidth = 0.5) +
  scale_x_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.36)) +
  scale_color_manual(values = party_colors) +
  labs(
    title = "100 plausible election outcomes based on current polls",
    subtitle = paste0(
      "Each dot = one possible result. White dot = poll estimate. ",
      "\nFDP clears 5% in only ", fdp_in_pct, "% of simulations."
    ),
    x = "Vote share", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# This turns an abstract "margin of error of ±2.6pp" into something
# concrete: a cloud of dots representing plausible election nights.


# -----------------------------------------------------------------------------
# Simulation histograms: a close race
# -----------------------------------------------------------------------------

# For a two-party comparison, overlapping histograms make the overlap
# viscerally obvious. Let's build this for SPD vs. AfD in Brandenburg 2024.

n_sims <- 10000
n_poll <- 1200

spd_poll <- 0.30
afd_poll <- 0.29

set.seed(2024)
sims <- tibble(
  sim = 1:n_sims,
  SPD = rnorm(n_sims, mean = spd_poll,
             sd = sqrt(spd_poll * (1 - spd_poll) / n_poll)),
  AfD = rnorm(n_sims, mean = afd_poll,
             sd = sqrt(afd_poll * (1 - afd_poll) / n_poll))
) |>
  mutate(spd_wins = SPD > AfD)

spd_win_pct <- round(mean(sims$spd_wins) * 100)

sims_long <- sims |>
  pivot_longer(cols = c(SPD, AfD), names_to = "party", values_to = "vote_share")

ggplot(sims_long, aes(x = vote_share, fill = party)) +
  geom_histogram(alpha = 0.5, bins = 60, position = "identity",
                 color = "white", linewidth = 0.1) +
  geom_vline(xintercept = spd_poll, linetype = "dashed",
             color = "#E3000F", linewidth = 0.7) +
  geom_vline(xintercept = afd_poll, linetype = "dashed",
             color = "#009EE0", linewidth = 0.7) +
  scale_fill_manual(values = c("SPD" = "#E3000F", "AfD" = "#009EE0")) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "10,000 simulated election nights: SPD vs. AfD in Brandenburg",
    subtitle = paste0(
      "SPD finishes ahead in ", spd_win_pct, "% of simulations. ",
      "\nThe overlap shows why this race was genuinely a toss-up."
    ),
    x = "Vote share", y = "Number of simulated outcomes",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = c(0.85, 0.85))

# The dual histogram makes the overlap viscerally obvious. Readers can
# *see* that in thousands of plausible outcomes, either party could win.
# One limitation: only works well for two-party comparisons.


# -----------------------------------------------------------------------------
# Discretized gradient plots (Yang et al. 2023)
# -----------------------------------------------------------------------------

# Yang et al. tested four uncertainty visualization formats during the
# 2022 U.S. midterms. Their "dual histogram intervals" use a distinctive
# design: horizontal bars split into discrete bins where color saturation
# encodes likelihood. This tested as the most effective format across all
# their outcome measures (emotion, trust, participation intention).

n_bins <- 8

gradient_bins <- parties |>
  rowwise() |>
  mutate(
    bin_center = list({
      half_range <- 3.5 * se
      bin_width <- (2 * half_range) / (2 * n_bins)
      seq(support - half_range + bin_width / 2,
          support + half_range - bin_width / 2,
          by = bin_width)
    }),
    bin_width = list(rep((2 * 3.5 * se) / (2 * n_bins), 2 * n_bins))
  ) |>
  unnest(c(bin_center, bin_width)) |>
  ungroup() |>
  mutate(
    dist_se = abs(bin_center - support) / se,
    raw_density = dnorm(dist_se)
  ) |>
  group_by(party) |>
  mutate(
    norm_density = raw_density / max(raw_density)
  ) |>
  ungroup()

# Create faded color for each bin: full party color at center,
# blending toward white at the tails.
gradient_bins <- gradient_bins |>
  mutate(
    base_color = party_colors[party],
    fade_amount = 1 - norm_density,
    tile_color = map2_chr(base_color, fade_amount, \(col, amt) {
      lighten(col, amount = amt, space = "HLS")
    })
  )

# CI range labels
ci_labels <- parties |>
  mutate(
    label = paste0(
      round(ci_low * 100, 1), "%–", round(ci_high * 100, 1), "%"
    )
  )

ggplot(gradient_bins) +
  geom_tile(
    aes(x = bin_center, y = reorder(party, support),
        width = bin_width, fill = tile_color),
    height = 0.55,
    color = "white", linewidth = 0.3
  ) +
  scale_fill_identity() +
  geom_text(
    data = ci_labels,
    aes(x = ci_high + 0.012, y = reorder(party, support),
        label = label),
    size = 3, hjust = 0, color = "gray40"
  ) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "gray50",
             linewidth = 0.5) +
  scale_x_continuous(
    labels = scales::percent_format(),
    limits = c(0, 0.40),
    expand = c(0, 0)
  ) +
  labs(
    title = "Discretized gradient plot: saturated color = more likely outcome",
    subtitle = paste0(
      "n = ", parties$n[1],
      ". Each tile is one bin of the sampling distribution.",
      "\nInspired by Yang et al. (2023)."
    ),
    x = "Vote share", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# The discrete bins make uncertainty tangible in a way that a single error
# bar cannot. The fully saturated center tile says "most likely here"; the
# pale tails fading toward white say "possible but unlikely."
#
# Reference: Yang, F., Cai, M., et al. (2023). Swaying the Public? Impacts
# of Election Forecast Visualizations on Emotion, Trust, and Intention in
# the 2022 U.S. Midterms. IEEE TVCG, 30(1), 23–33.
