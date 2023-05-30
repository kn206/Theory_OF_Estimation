library(dplyr)

CI_95 <- function(sample_vals, sig){
  error <- margin_error_95(sample_vals, sig)
  CI <- mean(sample_vals) + c(-1, 1)*error
}


margin_error_95 <- function(sample_vals, sig){
  n <- length(sample_vals)
  mar_err <- 1.96*(sig/sqrt(n))
}

png("group_plot.png",height=7,width=3,units = 'in',res=1000)

plot_CI_95 <- function(seed){
  B <- 100
  n <- 30
  # mean and std dev as a vector of values
  # need to have same length
  # assume one value per group
  mu <- c(5,4.5) # group1, group2
  sig <- c(1.2,1) # group1, group2
  offset<- 0.25 # controls point and line offset from nominal value
  # colors for 2 groups
  colsuse  <- c('steelblue','gold')
  
  # loop over groups
  # mu and sig are now indexed by this loop
  for(j in 1:length(mu)){
    # use seed+j to make different random sample for each group
    
    # extract upper bound of CI's
    set.seed(seed+j)
    x_1 <- replicate(B,
                     {samp <- rnorm(n, mean = mu[j], sd = sig[j])
                     max(CI_95(samp, sig[j]))
                     }
    )
    
    #extract lower bound of CI's
    set.seed(seed+j)
    x_0 <- replicate(B,
                     {samp <- rnorm(n, mean = mu[j], sd = sig[j])
                     min(CI_95(samp, sig[j]))
                     }
    )
    
    set.seed(seed+j)
    
    means <- replicate(B, mean(rnorm(n, mean = mu[j], sd = sig[j])))
    
    # for first group, establish the plot
    # for second group, add values to the plot
    # if groups are very different this might need to be modified with the xlim
    if(j == 1){
      plot(means, (1:B)+offset*ifelse(j==1,1,-1), pch = 20,
           xlim = c(mu[j] - sig[j], mu[j] + sig[j]),
           ylim = c(0,B+1),
           xlab = "sample means",
           ylab = "index of the CI",
           main = paste(B, "Confidence intervals"),
           col=colsuse[j]
      )
    }else{
      points(means, (1:B)+offset*ifelse(j==1,1,-1), pch = 20,
             col=colsuse[j])
    }
    
    for (i in 1:B){
      if(between(mu[j], x_0[i], x_1[i])){
        segments(x_0[i], i+offset*ifelse(j==1,1,-1), x_1[i], i+offset*ifelse(j==1,1,-1), col=colsuse[j], lwd = 1) #plot CI's that contain the mean in black
      } else {
        segments(x_0[i], i+offset*ifelse(j==1,1,-1), x_1[i], i+offset*ifelse(j==1,1,-1), col = "red", lwd = 1) #plot CI's that don't contain the mean in red
      }
    }
    
    abline(v=mu[j], col = colsuse[j]) #plot a vertical line at the population mean
  }
  
  par(xpd=F)
  # legend for the groups
  legend("topright",legend = c('Male','Female'),lty=1,col=colsuse,cex=0.5)
}

plot_CI_95(1)

dev.off()