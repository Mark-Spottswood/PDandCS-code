
## Plotting the burden of proof


r = 8.155
C1 = 1 / ((1 / (1+exp(-r/2))) - (1 / (1+exp(r/2))))
C2 = -C1 / (1+exp(r/2))
D=10
curve(D*((C1 / (1 + exp(-r*x + (r/2)))) + C2), from=0, to=1,
      main= paste("Logistic Burden of Proof"),
      ylab = "Percentage of Total Damages Awarded",
      xlab = "Probability of Liability",
      sub = ("Red Curve - Logistic Function; Dashed Line - Step Function; Dotted Line - Linear Function"),
      col = "red",
      cex.sub=0.8,
      axes=FALSE)
axis(side=1)
axis(side=2, at=c(0,D/2,D), labels=c(0, "D/2", "D"))

segments(0,0,0.5,0, lty = 2)
segments(0.5,0,0.5,D, lty = 2)
segments(0.5,D,1,D, lty = 2)

segments(0,0,1,D, lty = "dotted")


## Let's visualize the impact on the settlement rate 

disc_settle = c(sum(CompareDF$Disc_Settle_True_False),nrow(CompareDF) - sum(CompareDF$Disc_Settle_True_False))
logistic_settle = c(sum(CompareDF$Logistic_Settle_True_False),nrow(CompareDF) - sum(CompareDF$Logistic_Settle_True_False))
settlementrates = as.table(rbind(disc_settle, logistic_settle))
settlementrates
prop.test(settlementrates)

settlerates_bydamages = matrix(
  c(
    sum(subset(CompareDF, Damages < 5000)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages < 5000)),
    sum(subset(CompareDF, Damages < 5000)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages < 5000)),
    sum(subset(CompareDF, Damages < 5000)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages < 5000)),
    
    sum(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)),
    sum(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)),
    sum(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)),

    sum(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)),
    sum(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)),
    sum(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)),

    sum(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)),
    sum(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)),
    sum(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)),

    
    sum(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)),
    sum(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)),
    sum(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)),

    sum(subset(CompareDF, Damages >= 1000000)$Disc_Settle_True_False, na.rm=TRUE) / nrow(subset(CompareDF, Damages >= 1000000)),
    sum(subset(CompareDF, Damages >= 1000000)$Logistic_Settle_True_False, na.rm=TRUE) / nrow(subset(CompareDF, Damages >= 1000000)),
    sum(subset(CompareDF, Damages >= 1000000)$Linear_Settle_True_False, na.rm=TRUE) / nrow(subset(CompareDF, Damages >= 1000000)),
    
    sum(CompareDF$Disc_Settle_True_False, na.rm = TRUE) / nrow(CompareDF),
    sum(CompareDF$Logistic_Settle_True_False, na.rm = TRUE) / nrow(CompareDF),
    sum(CompareDF$Linear_Settle_True_False, na.rm = TRUE) / nrow(CompareDF)
  ),
  
  nrow=3, ncol=7)        


barplot(settlerates_bydamages, ylim = c(0,1), main="Settlement Rate Under Each Rule", 
        col = c("Black", "Green", "Blue"), beside=TRUE,
        xlab = "Amount in Controversy", 
        names = c("<5K", "5K-10K", "10K-25K", "25K-100K", "100K-1M", ">1M","All"), cex.names = .75)

legend("topright", inset = 0.05, legend=c("Discontinuous Burden", "Logistic Burden", "Linear Burden"), 
       fill=c("Black", "Green", "Blue"), cex=1)

nrow(subset(CompareDF, Damages > 100000)) / nrow(CompareDF)

nrow(subset(CompareDF, Damages < 5000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages > 1000000)) / nrow(CompareDF)

moderate_to_high_damages_subsetDF = subset(CompareDF, Damages>25000)
disc_settle_mod_to_large = c(sum(moderate_to_high_damages_subsetDF$Disc_Settle_True_False),
                      nrow(moderate_to_high_damages_subsetDF) - sum(moderate_to_high_damages_subsetDF$Disc_Settle_True_False))
Logistic_settle_mod_to_large = c(sum(moderate_to_high_damages_subsetDF$Logistic_Settle_True_False),
                          nrow(moderate_to_high_damages_subsetDF) - sum(moderate_to_high_damages_subsetDF$Logistic_Settle_True_False))
settlementrates_mod_to_large = as.table(rbind(disc_settle_mod_to_large, Logistic_settle_mod_to_large))
settlementrates_mod_to_large
prop.test(settlementrates_mod_to_large)

## By case strength

settlerates_by_case_strength = matrix(
  c(
    sum(subset(CompareDF, Unbiased_Pred_Mean < .1)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean <= .1)),
    sum(subset(CompareDF, Unbiased_Pred_Mean < .1)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean <= .1)),
    sum(subset(CompareDF, Unbiased_Pred_Mean < .1)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean <= .1)),
    
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)),
    
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Disc_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .9)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Logistic_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .9)),
    sum(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Linear_Settle_True_False, na.rm=TRUE) / 
      nrow(subset(CompareDF, Unbiased_Pred_Mean >= .9)),
    
    sum(CompareDF$Disc_Settle_True_False, na.rm = TRUE) / nrow(CompareDF),
    sum(CompareDF$Logistic_Settle_True_False, na.rm = TRUE) / nrow(CompareDF),
    sum(CompareDF$Linear_Settle_True_False, na.rm = TRUE) / nrow(CompareDF)
  ),
  nrow=3, ncol=11)        

barplot((settlerates_by_case_strength), ylim = c(0,1), main="Settlement Rate Under Each Rule, by Case Strength", 
        col = c("Black", "Green", "Blue"), beside=TRUE,
        xlab = "Predicted Jury Confidence Level", 
        names = c("<.1",".1-.2",".2-.3", ".3-.4", ".4-.5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", ">.9", "All"), cex.names = .9)
legend("topright", inset = 0.05, legend=c("Discontinuous Burden", "Logistic Burden", "Linear Burden"), 
       fill=c("Black", "Green", "Blue"), cex=1)

##### Now let's compare error costs

disc_settle_error_rate = mean(CompareDF$Expected_Disc_Settlement_Error, na.rm = TRUE)
logistic_settle_error_rate = mean(CompareDF$Expected_Logistic_Settlement_Error, na.rm = TRUE)
settlement_error_means = as.table(rbind(disc_settle_error_rate, logistic_settle_error_rate))
settlement_error_means
t.test(CompareDF$Expected_Disc_Settlement_Error, CompareDF$Expected_Logistic_Settlement_Error)

disc_settle_error_over_damages_mean = mean(CompareDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
logistic_settle_error_over_damages_mean = mean(CompareDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
settlement_error_over_damages_means = as.table(rbind(disc_settle_error_over_damages_mean, logistic_settle_error_over_damages_mean))
settlement_error_over_damages_means
t.test(CompareDF$Expected_Disc_Settlement_Error_over_Damages, CompareDF$Expected_Logistic_Settlement_Error_over_Damages)

## So no statistically significant difference in error cost behavior

## Perhaps an interesting difference in some subsets?

expected_errors_over_damages_bydamages = matrix(
  c(
    mean(subset(CompareDF, Damages < 5000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages < 5000)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 1000000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 1000000)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(CompareDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE), 
    mean(CompareDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
  ),
  nrow=2, ncol=7)        


barplot((expected_errors_over_damages_bydamages), ylim = c(0,10), main="Expected Errors Under Each Rule", 
        col = c("Black", "Green"), beside=TRUE,
        xlab = "Amount in Controversy", ylab = "Expected Errors (as proportion of AIC)",
        names = c("<5K", "5K-10K", "10K-25K", "25K-100K", "100K-1M", ">1M","All"), cex.names = .75)
legend("topright", inset = 0.05, legend=c("Continuous (Logistic) Burden", "Discontinuous Burden"), 
       fill=c("green", "black"), cex=1)

small_subsetDF = subset(CompareDF, Damages < 5000)
small_disc_settle_errors_over_damages_mean = mean(small_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
small_logistic_settle_errors_over_damages_mean = mean(small_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
small_settlement_error_over_damages_means = as.table(
  rbind(small_disc_settle_errors_over_damages_mean, small_logistic_settle_errors_over_damages_mean))
small_settlement_error_over_damages_means
t.test(small_subsetDF$Expected_Disc_Settlement_Error_over_Damages, small_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)

medium_subsetDF = subset(CompareDF, Damages > 25000 & Damages <100000)
medium_disc_settle_errors_over_damages_mean = mean(medium_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
medium_logistic_settle_errors_over_damages_mean = mean(medium_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
medium_settlement_error_over_damages_means = as.table(
  rbind(medium_disc_settle_errors_over_damages_mean, medium_logistic_settle_errors_over_damages_mean))
medium_settlement_error_over_damages_means
t.test(medium_subsetDF$Expected_Disc_Settlement_Error_over_Damages, medium_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)

large_subsetDF = subset(CompareDF, Damages > 100000)
large_disc_settle_errors_over_damages_mean = mean(large_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
large_logistic_settle_errors_over_damages_mean = mean(large_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
large_settlement_error_over_damages_means = as.table(
  rbind(large_disc_settle_errors_over_damages_mean, large_logistic_settle_errors_over_damages_mean))
large_settlement_error_over_damages_means
t.test(large_subsetDF$Expected_Disc_Settlement_Error_over_Damages, large_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)

## So the only significant difference is an advantage for the traditional rule in large cases

## How about case strength?

error_over_damages_by_case_strength = matrix(
  c(
    mean(subset(CompareDF, Unbiased_Pred_Mean < .1)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean < .1)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(CompareDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE),
    mean(CompareDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE) 
  ),
  nrow=2, ncol=11)        

barplot((error_over_damages_by_case_strength), ylim = c(0,8), main="Expected Errors Under Each Rule, by Case Strength", 
        col = c("Black", "Green"), beside=TRUE,
        xlab = "Predicted Jury Confidence Level", ylab = "Expected Errors (as proportion of AIC)",
        names = c("<.1",".1-.2",".2-.3", ".3-.4", ".4-.5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", ">.9", "All"), cex.names = .9)
legend("topright", inset = 0.01, legend=c("Continuous (Logistic) Burden", "Discontinuous Burden"), 
       fill=c("Green", "black"), cex=1)

easy_subsetDF = subset(CompareDF, (Unbiased_Pred_Mean < .2 | Unbiased_Pred_Mean > .8))
easy_cases_disc_settle_error_over_damages_mean = mean(
  easy_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
easy_cases_Logistic_settle_error_over_damages_mean = mean(
  easy_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
easy_cases_settlement_error_over_damages_means = as.table(
  rbind(easy_cases_disc_settle_error_over_damages_mean,easy_cases_Logistic_settle_error_over_damages_mean))
easy_cases_settlement_error_over_damages_means
t.test(easy_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       easy_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)

hard_subsetDF = subset(CompareDF, (Unbiased_Pred_Mean > .2 & Unbiased_Pred_Mean < .8))
hard_cases_disc_settle_error_over_damages_mean = mean(
  hard_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
hard_cases_Logistic_settle_error_over_damages_mean = mean(
  hard_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
hard_cases_settlement_error_over_damages_means = as.table(
  rbind(hard_cases_disc_settle_error_over_damages_mean,hard_cases_Logistic_settle_error_over_damages_mean))
hard_cases_settlement_error_over_damages_means
t.test(hard_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       hard_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)


## Still too noisy to see much ... let's subset out the cases with very high trial costs

subsetDF_trialcosts_less_than_100XDamages = subset(CompareDF, Cost_of_Trial < 100*Damages)
nrow(subsetDF_trialcosts_less_than_100XDamages)

error_over_damages_by_case_strength_low_to_moderate_costs3 = matrix(
  c(
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean < .1)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean < .1)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .9)$Expected_Logistic_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subsetDF_trialcosts_less_than_100XDamages$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE),
    mean(subsetDF_trialcosts_less_than_100XDamages$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE) 
  ),
  nrow=2, ncol=11)        

barplot((error_over_damages_by_case_strength_low_to_moderate_costs3), 
        ylim = c(0,1.5), main="Expected Errors Under Each Rule, by Case Strength", sub = "Trial Costs < 100*AIC", 
        col = c("Black", "green"), beside=TRUE,
        xlab = "Predicted Jury Confidence Level", ylab = "Expected Errors (as proportion of AIC)",
        names = c("<.1",".1-.2",".2-.3", ".3-.4", ".4-.5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", ">.9", "All"), cex.names = .9)
legend("topright", inset = 0.01, legend=c("Continuous (Logistic) Burden", "Discontinuous Burden"), 
       fill=c("green", "black"), cex=1)


easy_affordable_subsetDF = subset(CompareDF, Cost_of_Trial < 100*Damages & (Unbiased_Pred_Mean < .2 | Unbiased_Pred_Mean > .8))
easy_affordable_cases_disc_settle_error_over_damages_mean = mean(
  easy_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
easy_affordable_cases_Logistic_settle_error_over_damages_mean = mean(
  easy_affordable_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
easy_affordable_cases_settlement_error_over_damages_means = as.table(
  rbind(easy_affordable_cases_disc_settle_error_over_damages_mean, easy_affordable_cases_Logistic_settle_error_over_damages_mean))
easy_affordable_cases_settlement_error_over_damages_means
t.test(easy_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       easy_affordable_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)

hard_affordable_subsetDF = subset(CompareDF, Cost_of_Trial < 100*Damages & (Unbiased_Pred_Mean > .2 & Unbiased_Pred_Mean < .8))
hard_affordable_cases_disc_settle_error_over_damages_mean = mean(
  hard_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
hard_affordable_cases_Logistic_settle_error_over_damages_mean = mean(
  hard_affordable_subsetDF$Expected_Logistic_Settlement_Error_over_Damages, na.rm = TRUE)
hard_affordable_cases_settlement_error_over_damages_means = as.table(
  rbind(hard_affordable_cases_disc_settle_error_over_damages_mean, hard_affordable_cases_Logistic_settle_error_over_damages_mean))
hard_affordable_cases_settlement_error_over_damages_means
t.test(hard_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       hard_affordable_subsetDF$Expected_Logistic_Settlement_Error_over_Damages)

##### For showing error curves side-by-side

#Plotting the two error curves together along with expected trial error values under each rule

fit_disc2 = lm(Expected_Disc_Settlement_Error_over_Damages ~ Unbiased_Pred_Mean + I(Unbiased_Pred_Mean * Unbiased_Pred_Mean), 
               data=subsetDF_trialcosts_under_100K)
summary(fit_disc2)

fit_linear2 = lm(Expected_Linear_Settlement_Error_over_Damages ~ Unbiased_Pred_Mean + I(Unbiased_Pred_Mean * Unbiased_Pred_Mean), 
               data=subsetDF_trialcosts_under_100K)
summary(fit_linear2)

fit_logistic2 = lm(Expected_Logistic_Settlement_Error_over_Damages ~ Unbiased_Pred_Mean + I(Unbiased_Pred_Mean * Unbiased_Pred_Mean), 
                   data=subsetDF_trialcosts_under_100K)
summary(fit_logistic2)


D=10
r = 8.155
C1 = 1 / ((1 / (1+exp(-r/2))) - (1 / (1+exp(r/2))))
C2= -C1 / (1+exp(r/2))

curve(
  D*((((C1 / (1 + exp(-r*x + (r/2)))) + C2) * (1-x)) +
       (((C1 / (1 + exp(-r*(1-x) + (r/2)))) + C2) * x)),
  lty = 4,lwd=2,
  col="Green",
  main= "Expected Settlement Errors vs. Trial Errors (Trial Costs Under $100K)", cex.main=1.25,
  ylab = "Expected Cost of Error as Fraction of AIC",
  xlab = "Probability of Liability",
  cex.sub=0.8, axes=FALSE, xlim = c(0,1), ylim=c(0,20))
axis(side=1)
axis(side=2, at=c(0,10,20), labels=c(0, "D", "2*D"))
segments(0,0,0.5,D/2, lty = 2, col="Red")
segments(0.5,D/2,1,0, lty = 2, col="Red")
curve(D*2*x*(1-x),lty = 3, lwd=2,col="Blue",add=TRUE)
curve(
  D*
    (coef(fit_logistic2)[1] + 
       (coef(fit_logistic2)[2] * x ) + 
       (coef(fit_logistic2)[3] * x  * x) 
    ),
  add=T, col="Green", lwd=2)

curve(
  D*
    (coef(fit_linear2)[1] + 
       (coef(fit_linear2)[2] * x ) + 
       (coef(fit_linear2)[3] * x  * x) 
    ),
  add=T, col="Blue", lwd=2)

curve(D*
        (coef(fit_disc2)[1] + 
           (coef(fit_disc2)[2] * x ) + 
           (coef(fit_disc2)[3] * x  * x) 
        ),
      add=T, col="Red", lwd=2)

legend("topright", inset=0.01,
       legend=c("Expected Settlement Errors, Discontinuous Burden",
                "Expected Settlement Errors, Logistic Burden",
                "Expected Settlement Errors, Linear Burden",
                "Expected Trial Errors, Linear Burden",
                "Expected Trial Errors, Logistic Burden",
                "Expected Trial Errors, Discontinuous Burden"), 
       col=c("red", "green", "blue", "blue", "green", "red"), 
       lty=c(1,1,1,3,4,2), lwd=c(2,2,2,2,2,2),
       cex=0.7)


## Analyzing Squared Error Data

disc_settle_sqerror_rate = mean(CompareDF$Expected_Disc_Sum_of_Squared_Error , na.rm = TRUE)
linear_settle_sqerror_rate = mean(CompareDF$Expected_Linear_Sum_of_Squared_Error, na.rm = TRUE)
logistic_settle_sqerror_rate = mean(CompareDF$Expected_Logistic_Sum_of_Squared_Error, na.rm = TRUE)
settlement_sqerror_means = as.table(rbind(disc_settle_sqerror_rate, linear_settle_sqerror_rate, logistic_settle_sqerror_rate))
settlement_sqerror_means
t.test(CompareDF$Expected_Disc_Sum_of_Squared_Error, CompareDF$Expected_Linear_Sum_of_Squared_Error)
t.test(CompareDF$Expected_Disc_Sum_of_Squared_Error, CompareDF$Expected_Logistic_Sum_of_Squared_Error)
t.test(CompareDF$Expected_Linear_Sum_of_Squared_Error, CompareDF$Expected_Logistic_Sum_of_Squared_Error)

disc_settle_sqerror_over_sqdamages_mean = mean(CompareDF$Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages, na.rm = TRUE)
linear_settle_sqerror_over_sqdamages_mean = mean(CompareDF$Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages, na.rm = TRUE)
logistic_settle_sqerror_over_sqdamages_mean = mean(CompareDF$Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages, na.rm = TRUE)
settlement_sqerror_over_sqdamages_means = as.table(rbind(disc_settle_sqerror_over_sqdamages_mean, 
                                                         linear_settle_sqerror_over_sqdamages_mean,
                                                         logistic_settle_sqerror_over_sqdamages_mean))
settlement_sqerror_over_sqdamages_means
t.test(CompareDF$Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages, 
       CompareDF$Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages)
t.test(CompareDF$Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages, 
       CompareDF$Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages)
t.test(CompareDF$Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages, 
       CompareDF$Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages)

RMSE_Disc_over_damages = (CompareDF$Expected_Disc_Sum_of_Squared_Error ^ .5) / CompareDF$Damages
RMSE_Linear_over_damages = (CompareDF$Expected_Linear_Sum_of_Squared_Error ^ .5) /  CompareDF$Damages
RMSE_Logistic_over_damages = (CompareDF$Expected_Logistic_Sum_of_Squared_Error ^ .5) /  CompareDF$Damages
t.test(RMSE_Disc_over_damages, RMSE_Linear_over_damages)
t.test(RMSE_Disc_over_damages, RMSE_Logistic_over_damages)
t.test(RMSE_Linear_over_damages, RMSE_Logistic_over_damages)


