
## This code was used to conduct the analysis reported in Part A of the paper's Appendix

DF = data.frame(pred_mean = numeric(), pred_var=numeric(), alpha=numeric(), beta=numeric())

forecast_precision = 25

for (i in 1:100000) {
        Unbiased_Pred_Mean = runif(1,0.01,.99)
        if (Unbiased_Pred_Mean > 1 - Unbiased_Pred_Mean) {
                Precision_Max = (1-Unbiased_Pred_Mean) / 10
        } else {
                Precision_Max = (Unbiased_Pred_Mean) / 10
        }
        Unbiased_Pred_Precision = runif(1,0.00001,Precision_Max)
        alpha = estBetaParams(Unbiased_Pred_Mean,Unbiased_Pred_Precision)[1]
        beta =  estBetaParams(Unbiased_Pred_Mean,Unbiased_Pred_Precision)[2]
        
        new_row = data.frame(pred_mean = Unbiased_Pred_Mean, 
                             pred_var = Unbiased_Pred_Precision, 
                             alpha=alpha,
                             beta=beta)
        DF = rbind.data.frame(DF, new_row)
}

usableDF = subset(DF, 
                  (DF$alpha >=1 | DF$beta >= 1) & 
                          (DF$alpha <= forecast_precision & DF$beta <=forecast_precision))
bimodalDF = subset(DF, DF$alpha <1 & DF$beta <1)
overconfidentDF = subset(DF, DF$alpha > forecast_precision | DF$beta > forecast_precision)
summary(usableDF)
hist(usableDF$pred_mean, breaks=51, 
     main = "Distribution of Mu Values After Pruning",
     xlab = "mu")

paste("Percentage of Rows Pruned is", 100*(nrow(DF) - nrow(usableDF)) / nrow(DF), "%")