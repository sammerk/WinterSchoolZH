library(tidyverse)

# Function to simulate beta-binomial distributed data
simulate_beta_binomial <- function(N, mean, sd, max_val) {
  # For beta-binomial: E[X] = n*p where p = alpha/(alpha+beta)
  #                    Var[X] = n*p*(1-p) * (alpha+beta+n)/(alpha+beta+1)
  
  n <- max_val
  p <- mean / max_val  # target proportion
  target_var <- sd^2
  
  # The beta-binomial variance is: Var[X] = n*p*(1-p) * (alpha+beta+n)/(alpha+beta+1)
  # We need to solve for alpha and beta such that this equals our target variance
  
  # Let rho = alpha + beta (concentration parameter)
  # Then: target_var = n*p*(1-p) * (rho+n)/(rho+1)
  # Solving for rho: rho = (n - target_var/(n*p*(1-p))) / (target_var/(n*p*(1-p)) - 1)
  
  binomial_var <- n * p * (1 - p)
  
  if (target_var >= binomial_var) {
    warning("SD too large for beta-binomial with these parameters. Using maximum possible...")
    target_var <- binomial_var * 0.99
  }
  
  if (target_var <= 0) {
    stop("SD must be positive")
  }
  
  # Calculate concentration parameter rho
  var_ratio <- target_var / binomial_var
  rho <- (n - var_ratio) / (var_ratio - 1)
  
  if (rho <= 0) {
    warning("Parameters out of range. Adjusting...")
    rho <- 0.5
  }
  
  # Calculate alpha and beta from rho and p
  alpha <- p * rho
  beta <- (1 - p) * rho
  
  # Ensure positive parameters
  alpha <- max(0.1, alpha)
  beta <- max(0.1, beta)
  
  # Generate beta-binomial data
  probs <- rbeta(N, alpha, beta)
  data <- rbinom(N, size = n, prob = probs)
  
  list(
    data = data,
    alpha = alpha,
    beta = beta,
    n = n
  )
}

# Example: Generate 1000 data points
set.seed(267)
target_mean <- 15
target_sd <- 5
target_max <- 30

result <- simulate_beta_binomial(N = 10000, mean = target_mean, sd = target_sd, max_val = target_max)

# 1) Compare empirical to given mean and sd
comparison <- tibble(
  Statistic = c("Mean", "SD"),
  Target = c(target_mean, target_sd),
  Empirical = c(mean(result$data), sd(result$data)),
  Difference = Empirical - Target,
  Percent_Error = round((Difference / Target) * 100, 2)
)

print(comparison)

# 2) Plot the distribution
data_df <- tibble(value = result$data)

ggplot(data_df, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 31, fill = "#267326", alpha = 0.7, color = "black", center = 1) +
  geom_density(color = "#d77d00", linewidth = 1.2) +
  geom_vline(xintercept = target_mean, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(result$data), color = "blue", linetype = "dotted", linewidth = 1) +
  annotate("text", x = target_mean + 2, y = Inf, vjust = 1.5, 
           label = paste0("Target μ = ", target_mean, ", σ = ", target_sd), color = "red", hjust = 0) +
  annotate("text", x = mean(result$data) + 2, y = Inf, vjust = 3, 
           label = paste0("Empirical μ = ", round(mean(result$data), 2), ", σ = ", round(sd(result$data), 2)), color = "blue", hjust = 0) +
  labs(
    title = "Beta-Binomial Distribution",
    subtitle = paste0("N = 10, α = ", round(result$alpha, 2), ", β = ", round(result$beta, 2)),
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()