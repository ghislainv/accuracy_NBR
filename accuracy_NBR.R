#!/usr/bin/Rscript

# ==============================================================================
# author          :Ghislain Vieilledent
# email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
# web             :https://ghislainv.github.io
# license         :GPLv3
# ==============================================================================

## Load data
data <- read.csv("accuracy_NBR.csv", header=TRUE)
data

## Cambodia
data_cambodia <- data[data$ctry=="cambodia",]

## Accuracy indices
accuracy_indices <- function (pred, obs) {
    # Create data-frame
    df <- data.frame(pred=pred, obs=obs)

    # Confusion matrix
    n00 <- sum((df["pred"] == 0) & (df["obs"] == 0))
    n10 <- sum((df["pred"] == 1) & (df["obs"] == 0))
    n01 <- sum((df["pred"] == 0) & (df["obs"] == 1))
    n11 <- sum((df["pred"] == 1) & (df["obs"] == 1))

    # Accuracy indices
    OA <- (n11+n00)/(n11+n10+n00+n01)
    FOM <- n11/(n11+n10+n01)
    Sensitivity <- n11/(n11+n01)
    Specificity <- n00/(n00+n10)
    TSS <- Sensitivity+Specificity-1
    N <- n11+n10+n00+n01
    Observed_accuracy <- (n11+n00)/N
    Expected_accuracy <- ((n11+n10)*((n11+n01)/N) + (n00+n01)*((n00+n10)/N)) / N
    Kappa <- (Observed_accuracy-Expected_accuracy)/(1-Expected_accuracy)

    r <- list(OA=round(OA, 2), FOM=round(FOM, 2),
         Sen=round(Sensitivity, 2),
         Spe=round(Specificity, 2),
         TSS=round(TSS, 2), K=round(Kappa, 2))

    return (r)
}

## Bootstrap


## Results
results <- accuracy_indices(pred=data_cambodia$Ass_2, obs=data_cambodia$NBR)
