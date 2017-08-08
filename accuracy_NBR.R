#!/usr/bin/Rscript

# ==============================================================================
# author          :Ghislain Vieilledent
# email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
# web             :https://ghislainv.github.io
# license         :GPLv3
# ==============================================================================

## Load data
data <- read.csv("accuracy_NBR.csv", header = TRUE)

## Data for each country
data_cambodia <- data[data$ctry == "cambodia", ]
data_laos <- data[data$ctry == "laos", ]

## Accuracy indices
accuracy_indices <- function (pred, obs) {
  # Create data-frame
  df <- data.frame(pred = pred, obs = obs)
  
  # Confusion matrix
  n00 <- sum((df["pred"] == 0) & (df["obs"] == 0))
  n10 <- sum((df["pred"] == 1) & (df["obs"] == 0))
  n01 <- sum((df["pred"] == 0) & (df["obs"] == 1))
  n11 <- sum((df["pred"] == 1) & (df["obs"] == 1))
  
  # Accuracy indices
  OA <- (n11 + n00) / (n11 + n10 + n00 + n01)
  FOM <- n11 / (n11 + n10 + n01)
  Sensitivity <- n11 / (n11 + n01)
  Specificity <- n00 / (n00 + n10)
  TSS <- Sensitivity + Specificity - 1
  N <- n11 + n10 + n00 + n01
  Observed_accuracy <- (n11 + n00) / N
  Expected_accuracy <-
    ((n11 + n10) * ((n11 + n01) / N) + (n00 + n01) * ((n00 + n10) / N)) / N
  Kappa <-
    (Observed_accuracy - Expected_accuracy) / (1 - Expected_accuracy)
  
  r <- list(
    OA = round(OA, 2),
    FOM = round(FOM, 2),
    Sen = round(Sensitivity, 2),
    Spe = round(Specificity, 2),
    TSS = round(TSS, 2),
    K = round(Kappa, 2)
  )
  
  return (r)
}

## Bootstrap function
nrep = 1000  ## convergence achieved with high number of nrep
bootstrap <- function (data,
                       proportion = 0.7,
                       repetitions = nrep) {
  
  ## Variables
  nobs <- length(data$NBR)
  nsamp <- round(nobs * proportion)
  repetitions <- repetitions
  ## Data-frame to hold indices
  indices <- data.frame(OA = rep(NA, repetitions),
                        TSS = NA,
                        Kappa = NA)
  
  ## Loop on repetitions
  for (i in 1:repetitions) {
    # Sample data
    samp <- sample(1:nobs, size = nsamp, replace = TRUE)  ## Sample with replacement
    d_samp <- data[samp, ]
    # Compute indices
    r <- accuracy_indices(pred = d_samp$Ass_2, obs = d_samp$NBR)
    indices$OA[i] <- r$OA
    indices$TSS[i] <- r$TSS
    indices$Kappa[i] <- r$K
  }
  
  ## Means and SD
  mean_ind <- round(apply(indices, 2, mean),2)
  sd_ind <- round(apply(indices, 2, sd),2)
  
  ## Results
  return(list(indices=indices, mean=mean_ind, sd=sd_ind))
}

## Run bootstrap
r_cambodia <- bootstrap(data = data_cambodia)
r_laos <- bootstrap(data = data_laos)

## Plot
## see here: https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
dat <- data.frame(OA=c(r_cambodia$indices$OA, r_laos$indices$OA), ctry=rep(c("cambodia","laos"), each=nrep))

## Mean OA per country
library(plyr)
cdat <- ddply(dat, "ctry", summarise, rating.mean=mean(OA))

## Superposed histograms with facet and normal density
library(ggplot2)
binwidth= 0.01
grid <- with(dat, seq(min(OA), max(OA), length = 100))
normaldens <- ddply(dat, "ctry", function(df) {
  data.frame( 
    OA = grid,
    density = dnorm(grid, mean(df$OA), sd(df$OA)) * length(df$OA) * binwidth
  )
})

ggplot(dat, aes(x=OA)) + 
  geom_histogram(breaks=seq(min(dat$OA), max(dat$OA), binwidth), colour="black", fill="white") + 
  geom_line(aes(y=density), data=normaldens, colour = "blue") +
  geom_vline(data=cdat, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red") +
  facet_grid(ctry ~ .)
ggsave(file="distributions.png")

## Test for significant differences
wilcox.test(OA ~ ctry, data = dat)  ## shift in medians
t.test(OA ~ ctry, data = dat)  ## diff in means
