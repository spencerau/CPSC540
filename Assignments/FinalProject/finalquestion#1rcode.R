library(brms)

priors <- c(
  prior("normal(-10000,10000)", class = "b"),
  prior("normal(-10000,10000)", class = "Intercept"),
  prior("gamma(0.1, 1000)", class = "sigma")
)
results <- brm(positive~average_playtime_forever+peak_ccu, data=games_may2024_cleaned, prior = priors)
summary(results)
#postieror check
results |> pp_check()
#traceplot
library(bayesplot)
mcmc_trace(results)