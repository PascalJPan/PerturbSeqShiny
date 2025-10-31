perm_bootstrap_CI <- function(
    DR_obs,
    DR_div_vec,
    SE_obs = NULL,                # NULL → discrete wobble, numeric → Gaussian jitter
    bootstrap_replicates = 1000,
    seed = 1,
    conf_level = 0.95,
    log10_space = FALSE,
    pseudocount = 0.001
) {
  stopifnot(is.numeric(DR_obs), length(DR_obs) == 1L)
  DR_div_vec <- DR_div_vec[is.finite(DR_div_vec)]
  if (length(DR_div_vec) < 2L) stop("Need at least 2 finite values in DR_div_vec.")
  
  set.seed(seed)
  
  mu_div <- mean(DR_div_vec, na.rm = TRUE)
  if (!is.finite(mu_div)) stop("mu_div not finite.")
  if (abs(mu_div) < .Machine$double.eps) warning("mu_div is ~0; ratio may be unstable.")
  
  # Bootstrap distribution of the score
  boot_scores <- replicate(bootstrap_replicates, {
    mu_div_star <- mean(sample(DR_div_vec, size = length(DR_div_vec), replace = TRUE))
    
    DR_obs_star <- if (is.null(SE_obs)) {
      # discrete wobble ±1
      pmax(0, DR_obs + sample(c(-1L, 0L, 1L), size = 1, prob = c(0.25, 0.5, 0.25)))
    } else {
      # Gaussian jitter (can be 0 for no variation)
      pmax(0, rnorm(1, mean = DR_obs, sd = SE_obs)) # assuming sd == se here with n=1 for DR_obs...
    }
    
    if (!log10_space) {
    DR_obs_star / mu_div_star
    } else {
      log10((DR_obs_star + pseudocount) / mu_div_star)
    }
  })
  
  if (!log10_space) {
    score_hat <- DR_obs / mu_div
    alpha <- 1 - conf_level
    qs <- stats::quantile(boot_scores, c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  } else {
    score_hat <- log10((DR_obs + pseudocount) / mu_div)
    alpha <- 1 - conf_level
    qs <- stats::quantile(boot_scores, c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  }
  
  
  list(df = data.frame(
    estimate = score_hat,
    lwr = unname(qs[1]),
    upr = unname(qs[2]),
    mu_div = mu_div,
    n_div = length(DR_div_vec),
    B = bootstrap_replicates,
    SE_obs = if (is.null(SE_obs)) NA_real_ else SE_obs,
    conf_level = conf_level,
    row.names = NULL
  ),
  scores_list = list(boot_scores))
}
