library(ggplot2)

metropolis_step <- function(x, sigma, target) {
    # generate proposal
    proposed_x <- rnorm(1, mean = x, sd = sigma)
    
    # calculate A
    accept_prob <- min(1, target(proposed_x) / target(x))
    
    # accept/reject
    accepted <- runif(1) <= accept_prob
    value <- ifelse(accepted,
                    proposed_x,
                    x)
    
    value
}


target <- function(x) {
    exp(-x^2) * (2 + sin(5 * x) + sin(2 * x))
}

iter <- 1000

samp <- 0
samples <- rep(0,iter)
for (i in 2:iter){
    samp <- metropolis_step(samp, sigma = 1,
                            target = target)
    samples[i] <- samp
    
}


df <- data.frame(samples = samples, t = 1:length(samples))

ggplot(df, aes(x = t, y = samples)) + 
    geom_line() + 
    theme_minimal() + 
    labs(title = "Sample Trace Plot: Sigma = 1")

# graph with smaller std dev / sigma
samples_df <- mh(1000, sigma = 0.25, target = target)

ggplot(samples_df, aes(x = t, y = samples)) + 
    geom_line() + 
    theme_minimal() + 
    labs(title = "Sample Trace Plot: Sigma = 0.25")


# X ~ N(0,1)
X <- rnorm(1e5,0,1) # n,mu,sd
Z <- X*4.5 + 10
W <- X^2
print(paste0("Z: ", round(mean(Z),2), ", W: ", round(mean(W),2)))

