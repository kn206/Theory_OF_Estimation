library(dplyr)
sample_vals<-rnorm(5,50,16)
margin_error_95 <- function(sample_vals, sig){
  n <- length(sample_vals)
  mar_err <- 1.96*(sig/sqrt(n))
}

CI_95 <- function(sample_vals, sig){
  error <- margin_error_95(sample_vals, sig)
  CI <- mean(sample_vals) + c(-1, 1)*error
}

sample_size_95 <- function(width, std_dev){
  # width = 2 * error, hence
  error <- width / 2
  n <- (1.96 * std_dev / error)^2
  return(n)
}

width_95 <- function(n, std_dev){
  error <- 1.96 * std_dev / sqrt(n)
  return(2 * error)
}

mean_inside_CI <- function(samp, pop_mu, pop_sig){
  upper <- mean(samp) + margin_error_95(samp, pop_sig)
  lower <- mean(samp) - margin_error_95(samp, pop_sig)
  return(between(pop_mu, lower, upper))
}

set.seed(2020)
B <- 10000
n <- 50

CI_95_exp <- replicate(B, 
                       {samp <- rexp(n, 2)  # generate a sample
                       # check is mean is inside 95% CI
                       mean_inside_CI(samp, pop_mu = 1 / 2 , pop_sig = 1 / 2)
                       }
)

mean( CI_95_exp )
#calculate mean CI_95_exp to get empirical expected values

plot_CI_95 <- function(seed){
  B <- 100
  n <- 30
  mu <- 5
  sig <- 1.2
  
  set.seed(seed)
  # extract upper bound of CI's
  
  x_1 <- replicate(B,
                   {samp <- rnorm(n, mean = mu, sd = sig )
                   max(CI_95(samp, sig))
                   }
  )
  
  #extract lower bound of CI's
  
  set.seed(seed)
  
  x_0 <- replicate(B,
                   {samp <- rnorm(n, mean = mu, sd = sig )
                   min(CI_95(samp, sig))
                   }
  )
  
  set.seed(seed)
  
  means <- replicate(B, mean(rnorm(n, mean = mu, sd = sig)))
  
  plot(means, 1:B, pch = 20,
       xlim = c(mu - sig, mu + sig),
       ylim = c(0,B+1),
       xlab = "sample means",
       ylab = "index of the CI",
       main = paste(B, "Confidence intervals")
  )
  
  for (i in 1:B){
    if(between(mu, x_0[i], x_1[i])){
      segments(x_0[i], i, x_1[i], i, lwd = 2) #plot CI's that contain the mean in black
    } else {
      segments(x_0[i], i, x_1[i], i, col = "red", lwd = 2) #plot CI's that don't contain the mean in red
    }
  }
  
  abline(v=mu, col = "blue") #plot a vertical line at the population mean
}

seeds <- c(2020, 42, 911, 14, 101, 34) #choose six different seed values

par(mfrow = c(1,1))

for (i in seeds) {
  plot_CI_95(i)
}