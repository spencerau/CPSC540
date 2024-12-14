# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)    # For data visualization
library(dplyr)      # For data manipulation
library(car)        # For multicollinearity check
library(equivalence)# For equivalence testing
library(mgcv)       # For GAM
library(brms)


# Assuming your dataframe is named 'steam_data'
steam_data <- read.csv("C:/Users/jrmul/OneDrive/Desktop/games_may2024_cleaned.csv/games_may2024_cleaned.csv", header=FALSE)

count_true <- sum(steam_data$V15 == "True")
print(count_true)

count_total <- sum(steam_data$V15 == "True" | steam_data$V15 == "False")
print(count_total)

#Set Variable Names based off of Data
estimated_owners <- steam_data$V36
windows <- steam_data$V15
mac <- steam_data$V16
linux <- steam_data$V17
price <- steam_data$V5

# View the first few entries of the estimated_owners column
head(steam_data$V36)

# Step 1: Split the range strings into two separate columns
steam_data <- steam_data %>%
  separate(V36, into = c("lower_bound", "upper_bound"), sep = " - ")

# Step 2: Remove any commas or other non-numeric characters, if present
steam_data <- steam_data %>%
  mutate(
    lower_bound = gsub(",", "", lower_bound),
    upper_bound = gsub(",", "", upper_bound)
  )

# Step 3: Convert the bounds to numeric
steam_data <- steam_data %>%
  mutate(
    lower_bound = as.numeric(lower_bound),
    upper_bound = as.numeric(upper_bound)
  )

# Step 4: Calculate the midpoint
steam_data <- steam_data %>%
  mutate(
    estimated_owners_numeric = (lower_bound + upper_bound) / 2
  )

# View the updated data
head(steam_data[, c("lower_bound", "upper_bound", "estimated_owners_numeric")])

# Fit the GLM without confounders
glm_model <- glm(estimated_owners_numeric ~ windows + mac + linux, data=steam_data, family=gaussian())

# Summary of the model
summary(glm_model)

# Fit the GLM with price as a confounder
glm_model_conf <- glm(estimated_owners_numeric ~ windows + mac + linux + price, data=steam_data, family=gaussian())

# Summary of the updated model
summary(glm_model_conf)

# Calculate Variance Inflation Factor (VIF)
vif(glm_model_conf)

# 95% Confidence intervals for model coefficients
exp(confint.defualt(glm_model_conf))

# Fit the GAM
gam_model <- gam(estimated_owners_numeric ~ s(price) + windows + mac + linux, data=steam_data)

# Summary of the GAM
summary(gam_model)

# Plot the smooth term for price
plot(gam_model, select=1, shade=TRUE)

# Filter the data to include only games with price ≤ $350
# Filter using base R syntax
data_filtered <- steam_data[steam_data$V5 <= 350, ]


gam_model_filtered <- gam(estimated_owners_numeric ~ s(price) + windows + mac + linux, data=steam_data)
# Summary of the filtered GAM
summary(gam_model_filtered)

# Plot the smooth term for data_filtered
plot(gam_model_filtered, select=1, shade=TRUE, seWithMean=TRUE)


# Load the mgcv package if not already loaded
library(mgcv)

# Fit the GAM
gam_model <- gam(estimated_owners_numeric ~ s(price) + windows + mac + linux, data=steam_data)

# Summary of the GAM
summary(gam_model)

# Plot the smooth term for price
plot(gam_model, select=1, shade=TRUE, seWithMean=TRUE)

# Ensure 'price' is numeric
steam_data$price <- as.numeric(steam_data$price)

# Check for missing values
sum(is.na(steam_data$V5))  # Should be 0

# Check the number of unique values
length(unique(steam_data$price))
#________________________________________________________________________________
steam_data <- steam_data %>%
  rename(
    windows = V15,
    mac = V16,
    linux = V17,
    price = V5
  )

# Specify the formula
formula <- bf(estimated_owners_numeric ~ windows + mac + linux + price)

# Calculate the mean of the estimated owners to set the intercept prior
mean_estimated_owners <- mean(steam_data$estimated_owners_numeric, na.rm = TRUE)

# Set priors with small positive means for platform coefficients
priors <- c(
  set_prior(paste0("normal(", mean_estimated_owners, ", 1e6)"), class = "Intercept"),
  set_prior("normal(10000, 50000)", class = "b", coef = "windowsTrue"),
  set_prior("normal(10000, 50000)", class = "b", coef = "macTrue"),
  set_prior("normal(10000, 50000)", class = "b", coef = "linuxTrue"),
  set_prior("normal(0, 1e5)", class = "b", coef = "price"),  # Non-informative prior for price
  set_prior("student_t(3, 0, 1e5)", class = "sigma")
)

# Fit the model
bayesian_model <- brm(
  formula = formula,
  data = steam_data,
  prior = priors,
  family = gaussian(),
  seed = 123,  # For reproducibility
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.95)
)

# Summary of the model
summary(bayesian_model)

# Plot trace plots
plot(bayesian_model, combo = c("dens_overlay", "trace"))
