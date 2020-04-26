
## Let's visualize the impact on the settlement rate 

disc_settle = c(sum(CompareDF$Disc_Settle_True_False),nrow(CompareDF) - sum(CompareDF$Disc_Settle_True_False))
linear_settle = c(sum(CompareDF$Linear_Settle_True_False),nrow(CompareDF) - sum(CompareDF$Linear_Settle_True_False))
settlementrates = as.table(rbind(disc_settle, linear_settle))
settlementrates
prop.test(settlementrates)


## Showing the interaction of case strength with settlement rates under the two rules


settlerates_by_case_strength = matrix(
        c(
                sum(subset(CompareDF, Unbiased_Pred_Mean < .1)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean <= .1)),
                sum(subset(CompareDF, Unbiased_Pred_Mean < .1)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean <= .1)),
                
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)),
                
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)),
                
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)),
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)),
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)),
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)),
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)),
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)),
                
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .9)),
                sum(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Unbiased_Pred_Mean >= .9)),
                
                sum(CompareDF$Disc_Settle_True_False, na.rm = TRUE) / nrow(CompareDF),
                sum(CompareDF$Linear_Settle_True_False, na.rm = TRUE) / nrow(CompareDF)
        ),
        nrow=2, ncol=11)        

barplot((settlerates_by_case_strength), ylim = c(0,1), main="Settlement Rate Under Each Rule, by Case Strength", 
        col = c("Black", "Blue"), beside=TRUE,
        xlab = "Predicted Jury Confidence Level", 
        names = c("<.1",".1-.2",".2-.3", ".3-.4", ".4-.5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", ">.9", "All"), cex.names = .9)
legend("topright", inset = 0.01, legend=c("Linear Burden", "Discontinuous Burden"), 
       fill=c("blue", "black"), cex=1)

easy_subsetDF = subset(CompareDF, Unbiased_Pred_Mean < .1 | Unbiased_Pred_Mean > .9)
easy_disc_settle = c(sum(easy_subsetDF$Disc_Settle_True_False),nrow(easy_subsetDF) - sum(easy_subsetDF$Disc_Settle_True_False))
easy_linear_settle = c(sum(easy_subsetDF$Linear_Settle_True_False),nrow(easy_subsetDF) - sum(easy_subsetDF$Linear_Settle_True_False))
easysettlementrates = as.table(rbind(easy_disc_settle, easy_linear_settle))
easysettlementrates
prop.test(easysettlementrates)

moderate-to-hard_subsetDF = subset(CompareDF, Unbiased_Pred_Mean > .1 & Unbiased_Pred_Mean < .9)
moderate-to-hard_disc_settle = c(sum(moderate-to-hard_subsetDF$Disc_Settle_True_False),
                                 nrow(moderate-to-hard_subsetDF) - sum(moderate-to-hard_subsetDF$Disc_Settle_True_False))
moderate-to-hard_linear_settle = c(sum(moderate-to-hard_subsetDF$Linear_Settle_True_False),
                                   nrow(moderate-to-hard_subsetDF) - sum(moderate-to-hard_subsetDF$Linear_Settle_True_False))
moderate-to-hardsettlementrates = as.table(rbind(moderate-to-hard_disc_settle, moderate-to-hard_linear_settle))
moderate-to-hardsettlementrates
prop.test(moderate-to-hardsettlementrates)

hard_subsetDF = subset(CompareDF, Unbiased_Pred_Mean > .4 & Unbiased_Pred_Mean < .6)
hard_disc_settle = c(sum(hard_subsetDF$Disc_Settle_True_False),
                                 nrow(hard_subsetDF) - sum(hard_subsetDF$Disc_Settle_True_False))
hard_linear_settle = c(sum(hard_subsetDF$Linear_Settle_True_False),
                                   nrow(hard_subsetDF) - sum(hard_subsetDF$Linear_Settle_True_False))
hardsettlementrates = as.table(rbind(hard_disc_settle, hard_linear_settle))
hardsettlementrates
prop.test(hardsettlementrates)

## By Damages Amounts

settlerates_bydamages = matrix(
        c(
                sum(subset(CompareDF, Damages < 5000)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages < 5000)),
                sum(subset(CompareDF, Damages < 5000)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages < 5000)),
                
                sum(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)),
                sum(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)),
                
                sum(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)),
                sum(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)),
                
                sum(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)),
                sum(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)),
                
                sum(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Disc_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)),
                sum(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Linear_Settle_True_False, na.rm=TRUE) / 
                        nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)),
                
                sum(subset(CompareDF, Damages >= 1000000)$Disc_Settle_True_False, na.rm=TRUE) / nrow(subset(CompareDF, Damages >= 1000000)),
                sum(subset(CompareDF, Damages >= 1000000)$Linear_Settle_True_False, na.rm=TRUE) / nrow(subset(CompareDF, Damages >= 1000000)),
                
                sum(CompareDF$Disc_Settle_True_False, na.rm = TRUE) / nrow(CompareDF),
                sum(CompareDF$Linear_Settle_True_False, na.rm = TRUE) / nrow(CompareDF)
        ),
        nrow=2, ncol=7)        


barplot((settlerates_bydamages), ylim = c(0,1), main="Settlement Rate Under Each Rule", 
        col = c("Black", "Blue"), beside=TRUE,
        xlab = "Amount in Controversy", 
        names = c("<5K", "5K-10K", "10K-25K", "25K-100K", "100K-1M", ">1M","All"), cex.names = .75)
legend("topright", inset = 0.05, legend=c("Linear Burden", "Discontinuous Burden"), 
       fill=c("blue", "black"), cex=1)

nrow(subset(CompareDF, Damages < 100000)) / nrow(CompareDF)

nrow(subset(CompareDF, Damages < 5000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages > 1000000)) / nrow(CompareDF)


high_damages_subsetDF = subset(CompareDF, Damages>100000)
disc_settle_large = c(sum(high_damages_subsetDF$Disc_Settle_True_False),nrow(high_damages_subsetDF) - sum(high_damages_subsetDF$Disc_Settle_True_False))
linear_settle_large = c(sum(high_damages_subsetDF$Linear_Settle_True_False),nrow(high_damages_subsetDF) - sum(high_damages_subsetDF$Linear_Settle_True_False))
settlementrates_large = as.table(rbind(disc_settle_large, linear_settle_large))
settlementrates_large
prop.test(settlementrates_large)

veryhigh_damages_subsetDF = subset(CompareDF, Damages>1000000)
disc_settle_verylarge = c(sum(veryhigh_damages_subsetDF$Disc_Settle_True_False),nrow(veryhigh_damages_subsetDF) - sum(veryhigh_damages_subsetDF$Disc_Settle_True_False))
linear_settle_verylarge = c(sum(veryhigh_damages_subsetDF$Linear_Settle_True_False),nrow(veryhigh_damages_subsetDF) - sum(veryhigh_damages_subsetDF$Linear_Settle_True_False))
settlementrates_verylarge = as.table(rbind(disc_settle_verylarge, linear_settle_verylarge))
settlementrates_verylarge
prop.test(settlementrates_verylarge)

### Showing how Discontinuous Predicted Victory Probability Differences Tend Towards Extreme Values,
### While Mean Predicted Jury Confidence Levels Tend Towards Moderate Values

summary(CompareDF)
summary(CompareDF$Mp_minus_Md)
summary(CompareDF$Pp_minus_Pd)

hist(CompareDF$Mp_minus_Md, breaks=500,
     main = "Linear Rule", 
     xlab = "Plaintiff's Predicted Mean Jury Confidence Level Minus Defendant's Predicted Mean Jury Confidence Level", cex.lab = .85)

hist(CompareDF$Pp_minus_Pd, breaks=500, 
     main = "Discontinuous Rule", 
     xlab = "Plaintiff's Predicted Victory Probability Minus Defendant's Predicted Victory Probability", cex.lab = .85)

length(CompareDF$Mp_minus_Md[CompareDF$Mp_minus_Md < 0.01]) / length(CompareDF$Mp_minus_Md)
length(CompareDF$Pp_minus_Pd[CompareDF$Pp_minus_Pd < 0.01]) / length(CompareDF$Pp_minus_Pd)

largesettleDF = subset(CompareDF, Damages > 100000 & (Disc_Settle_True_False == TRUE | Linear_Settle_True_False == TRUE))
nrow(largesettleDF)
hist(largesettleDF$Unbiased_Pred_Mean, breaks=50, 
     main = "Unbiased Mean Confidence Forecasts for Large Cases That Settle", 
     xlab = "Unbiased Mean Jury Confidence Forecasts")
