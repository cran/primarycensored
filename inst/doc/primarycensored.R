## ----setup, message = FALSE---------------------------------------------------
# Load packages
library(primarycensored)
library(ggplot2)
# Set seed for reproducibility
set.seed(123)

## ----sample-lognormal---------------------------------------------------------
n <- 1e4
meanlog <- 1.5
sdlog <- 0.75
obs_time <- 10
pwindow <- 1

# Random samples without secondary event censoring
samples <- rprimarycensored(
  n,
  rdist = rlnorm, rprimary = runif,
  pwindow = pwindow, swindow = 0, D = obs_time,
  meanlog = meanlog, sdlog = sdlog
)
# Random samples with secondary event censoring
samples_sc <- rprimarycensored(
  n,
  rdist = rlnorm, rprimary = runif,
  pwindow = pwindow, swindow = 1, D = obs_time,
  meanlog = meanlog, sdlog = sdlog
)
# Calculate the PMF for the samples with secondary event censoring
samples_sc_pmf <- data.frame(
  pmf =
    table(samples_sc) /
      sum(table(samples_sc))
)
# Compare the samples with and without secondary event censoring
# to the true distribution
ggplot() +
  geom_density(
    data = data.frame(samples = samples),
    aes(x = samples),
    fill = "#4292C6",
    col = "#252525",
    alpha = 0.5
  ) +
  geom_col(
    data = samples_sc_pmf,
    aes(
      x = as.numeric(as.character(pmf.samples_sc)),
      y = pmf.Freq
    ),
    fill = "#20b986",
    col = "#252525",
    alpha = 0.5,
    width = 0.9,
    just = 0
  ) +
  geom_function(
    fun = dlnorm,
    args = list(meanlog = meanlog, sdlog = sdlog),
    color = "#252525",
    linewidth = 1
  ) +
  labs(
    title = "Comparison of Samples from Log-Normal Distribution",
    x = "Delay",
    y = "Density",
    caption = paste0(
      "Blue density: Truncated samples without secondary event censoring\n",
      "Green bars: Truncated samples with secondary event censoring\n",
      "Black line: True log-normal distribution without truncation"
    )
  ) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

## ----cdf-lognormal------------------------------------------------------------
empirical_cdf <- ecdf(samples)
theoretical_cdf <- pprimarycensored(
  seq(0, obs_time, length.out = 100),
  pdist = plnorm, dprimary = dunif,
  pwindow = pwindow, D = obs_time,
  meanlog = meanlog, sdlog = sdlog
)

# Create a data frame for plotting
cdf_data <- data.frame(
  x = seq(0, obs_time, length.out = 100),
  Theoretical = theoretical_cdf,
  Empirical = empirical_cdf(seq(0, obs_time, length.out = 100))
)

# Plot the empirical and theoretical CDFs
ggplot(cdf_data, aes(x = x)) +
  geom_step(aes(y = Theoretical), color = "black", linewidth = 1) +
  geom_step(aes(y = Empirical), color = "#4292C6", linewidth = 1) +
  labs(
    title = "Comparison of Empirical and Theoretical CDFs",
    x = "Delay",
    y = "Cumulative Probability",
    caption = paste0(
      "Blue line: Empirical CDF from samples without secondary
       event  censoring\n",
      "Black line: Theoretical CDF computed using pprimarycensored()"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

## ----pmf-lognormal------------------------------------------------------------
# Calculate the theoretical PMF using dprimarycensored
theoretical_pmf <- dprimarycensored(
  0:(obs_time - 1),
  pdist = plnorm, dprimary = dunif,
  pwindow = pwindow, swindow = 1, D = obs_time,
  meanlog = meanlog, sdlog = sdlog
)

pmf_df <- data.frame(
  x = 0:obs_time,
  pmf = c(theoretical_pmf, 0)
)

# Plot the empirical and theoretical PMFs
ggplot() +
  geom_col(
    data = samples_sc_pmf,
    aes(
      x = as.numeric(as.character(pmf.samples_sc)),
      y = pmf.Freq
    ),
    fill = "#20b986",
    col = "#252525",
    alpha = 0.5,
    width = 0.9,
    just = 0
  ) +
  geom_step(
    data = pmf_df,
    aes(x = x, y = pmf),
    color = "black",
    linewidth = 1
  ) +
  labs(
    title = "Comparison of Samples from Log-Normal Distribution",
    x = "Delay",
    y = "Density",
    caption = paste0(
      "Green bars: Empirical PMF from samples with secondary event censoring\n",
      "Black line: Theoretical PMF computed using dprimarycensored()"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

