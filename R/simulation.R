### simulate means from beta-regression null-model intercept estimate and standard error and ordbeta dispersion parameter
# 1. Define your null model estimates
intercept <- summary(alien.no)$coef$cond[,1]        # Intercept estimate
se <- summary(alien.no)$coef$cond[,2]               # Standard error
phi <- summary(alien.no)$sigma               # Precision/dispersion parameter (phi)

# 2. Simulate the intercept (accounting for uncertainty)
sim_intercept <- rnorm(n = 10, mean = intercept, sd = se)

# 3. Convert to mean (mu) using inverse logit link
mu <- 1 / (1 + exp(-sim_intercept))

# 4. Calculate beta distribution shape parameters
p <- mu * phi
q <- (1 - mu) * phi

# 5. Simulate data points
simulated_data <- rbeta(n = 10, shape1 = p, shape2 = q)



### simulate means from beta-regression (from glmmTMB) null-model intercept estimate and standard error and ordbeta dispersion parameter

# 1. Define your estimated parameters
intercept_estimate <- summary(alien.no)$coef$cond[,1]   # example logit-scale intercept from your glmmTMB summary
intercept_se       <- summary(alien.no)$coef$cond[,2]   # example standard error
log_dispersion     <- summary(alien.no)$sigma   # example log-dispersion parameter (betadisp component)

# 2. Convert dispersion to the original scale
phi <- exp(log_dispersion)

# 3. Simulate hypothetical population intercepts (accounting for uncertainty)
nsim <- 10
simulated_intercepts <- rnorm(nsim, mean = intercept_estimate, sd = intercept_se)

# 4. Convert intercepts to the mean response scale [0, 1]
simulated_means <- plogis(simulated_intercepts)

# Summary of simulated means
summary(simulated_means)

# 5. Simulate observation-level data for ordbeta/beta
# Beta distribution utilizes mu and phi to model variance
simulated_data <- rbeta(nsim, simulated_means * phi, phi * (1 - simulated_means))
