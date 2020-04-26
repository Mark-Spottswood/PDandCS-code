## This worksheet includes the code I used to conduct the error rate analyses in Part III of the paper.

## Showing How Settlement Outcomes Track Result Under Each Burden When Trial Costs Are Small

Disc_Settle_Subset1 = CompareDF[CompareDF$Disc_Settle_True_False == TRUE & CompareDF$Cost_of_Trial < .1 * CompareDF$Damages ,]
Sample_Disc_Settle_Subset1 = Disc_Settle_Subset1[sample(nrow(Disc_Settle_Subset1), 1000),]

Disc_Settlement_Amount_Over_Damages1 = Disc_Settle_Subset1$Disc_Settlement_Amount / Disc_Settle_Subset1$Damages
Unbiased_Pred_Means_Disc_Settle_Subset1 = Disc_Settle_Subset1$Unbiased_Pred_Mean

disc_settle_amount_fit1 = lm(Disc_Settlement_Amount_Over_Damages1 ~ Unbiased_Pred_Means_Disc_Settle_Subset1)
summary(disc_settle_amount_fit1)

Sample_Disc_Settlement_Amount_Over_Damages1 = Sample_Disc_Settle_Subset1$Disc_Settlement_Amount / Sample_Disc_Settle_Subset1$Damages
Sample_Unbiased_Pred_Means_Disc_Settle_Subset1 = Sample_Disc_Settle_Subset1$Unbiased_Pred_Mean

Linear_Settle_Subset1 = CompareDF[CompareDF$Linear_Settle_True_False == TRUE & CompareDF$Cost_of_Trial < .1 * CompareDF$Damages ,]
Sample_Linear_Settle_Subset1 = Linear_Settle_Subset1[sample(nrow(Linear_Settle_Subset1), 1000),]

Linear_Settlement_Amount_Over_Damages1 = Linear_Settle_Subset1$Linear_Settlement_Amount / Linear_Settle_Subset1$Damages
Unbiased_Pred_Means_Linear_Settle_Subset1 = Linear_Settle_Subset1$Unbiased_Pred_Mean

Linear_settle_amount_fit1 = lm(Linear_Settlement_Amount_Over_Damages1 ~ Unbiased_Pred_Means_Linear_Settle_Subset1)
summary(Linear_settle_amount_fit1)

Sample_Linear_Settlement_Amount_Over_Damages1 = 
  Sample_Linear_Settle_Subset1$Linear_Settlement_Amount / Sample_Linear_Settle_Subset1$Damages
Sample_Unbiased_Pred_Means_Linear_Settle_Subset1 = 
  Sample_Linear_Settle_Subset1$Unbiased_Pred_Mean

plot(Sample_Disc_Settlement_Amount_Over_Damages1 ~ Sample_Unbiased_Pred_Means_Disc_Settle_Subset1,
     pch=19, cex=0.05, ylim = c(0,1.5), cex.main = 1.1,
     main = "Discontinuous v. Linear Burdens - Predicted Settlements (Trial Costs < 10% of AIC)",
     xlab = "Unbiased Prediction of Mean Jury Confidence Level", ylab = "Settlement Amount as a Proportion of AIC", col="Red")
points(Sample_Linear_Settlement_Amount_Over_Damages1 ~ Sample_Unbiased_Pred_Means_Linear_Settle_Subset1,
       pch=19, cex=0.05, col = "Blue")

segments(0,0,0.5,0, lty = 2, lwd=2)
segments(0.5,0,0.5,1, lty = 2, lwd=2)
segments(0.5,1,1,1, lty = 2, lwd=2)

segments(0,0,1,1, lty = 3, lwd=2)

legend("topleft", inset = .05, 
       legend = c("Discontinuous Rule Settlements (as Fraction of AIC)",
                  "Discontinuous Rule - Predicted Trial Outcome",
                  "Linear Rule Settlements  (as Fraction of AIC)",
                  "Linear Rule - Predicted Trial Outcome"),
       lty = c(NA,2,NA,3), pch = c(19, NA, 19, NA), 
       lwd = c(NA,2,NA,2), pt.cex = c(.25, NA, .25, NA),
       col = c("Red", "Black", "Blue", "Black"), cex = .8)


## Showing How Settlement Outcomes Track Result Under Each Burden When Trial Costs Are Between Half and Double the Stakes

Disc_Settle_Subset2 = CompareDF[CompareDF$Disc_Settle_True_False == TRUE & 
                                  CompareDF$Cost_of_Trial < 2*CompareDF$Damages &
                                  CompareDF$Cost_of_Trial > 0.5*CompareDF$Damages,]
Sample_Disc_Settle_Subset2 = Disc_Settle_Subset2[sample(nrow(Disc_Settle_Subset2), 1000),]

Disc_Settlement_Amount_Over_Damages2 = Disc_Settle_Subset2$Disc_Settlement_Amount / Disc_Settle_Subset2$Damages
Unbiased_Pred_Means_Disc_Settle_Subset2 = Disc_Settle_Subset2$Unbiased_Pred_Mean

disc_settle_amount_fit2 = lm(Disc_Settlement_Amount_Over_Damages2 ~ Unbiased_Pred_Means_Disc_Settle_Subset2)
summary(disc_settle_amount_fit2)

Sample_Disc_Settlement_Amount_Over_Damages2 = Sample_Disc_Settle_Subset2$Disc_Settlement_Amount / Sample_Disc_Settle_Subset2$Damages
Sample_Unbiased_Pred_Means_Disc_Settle_Subset2 = Sample_Disc_Settle_Subset2$Unbiased_Pred_Mean


Linear_Settle_Subset2 = CompareDF[CompareDF$Linear_Settle_True_False == TRUE  & 
                                    CompareDF$Cost_of_Trial < 2*CompareDF$Damages &
                                    CompareDF$Cost_of_Trial > 0.5*CompareDF$Damages,]
Sample_Linear_Settle_Subset2 = Linear_Settle_Subset2[sample(nrow(Linear_Settle_Subset2), 1000),]

Linear_Settlement_Amount_Over_Damages2 = Linear_Settle_Subset2$Linear_Settlement_Amount / Linear_Settle_Subset2$Damages
Unbiased_Pred_Means_Linear_Settle_Subset2 = Linear_Settle_Subset2$Unbiased_Pred_Mean

Linear_settle_amount_fit2 = lm(Linear_Settlement_Amount_Over_Damages2 ~ Unbiased_Pred_Means_Linear_Settle_Subset2)
summary(Linear_settle_amount_fit2)

Sample_Linear_Settlement_Amount_Over_Damages2 = Sample_Linear_Settle_Subset2$Linear_Settlement_Amount / Sample_Linear_Settle_Subset2$Damages
Sample_Unbiased_Pred_Means_Linear_Settle_Subset2 = Sample_Linear_Settle_Subset2$Unbiased_Pred_Mean

plot(Sample_Disc_Settlement_Amount_Over_Damages2 ~ Sample_Unbiased_Pred_Means_Disc_Settle_Subset2,
     pch=19, cex=0.05, ylim = c(0,1.5), cex.main = 1.1,
     main = "Predicted Settlements (Trial Costs Ranging from .5*AIC to 2*AIC)",
     xlab = "Unbiased Prediction of Mean Jury Confidence Level", ylab = "Settlement Amount as a Proportion of AIC", col="Red")
points(Sample_Linear_Settlement_Amount_Over_Damages2 ~ Sample_Unbiased_Pred_Means_Linear_Settle_Subset2,
       pch=19, cex=0.05, col = "Blue")

segments(0,0,0.5,0, lty = 2, lwd=2)
segments(0.5,0,0.5,1, lty = 2, lwd=2)
segments(0.5,1,1,1, lty = 2, lwd=2)

segments(0,0,1,1, lty = 2, lwd=2)

abline(disc_settle_amount_fit2, col="Red")
abline(Linear_settle_amount_fit2, col="Blue")

legend("topleft", inset = .05, 
       legend = c("Discontinuous Rule Settlements (as Fraction of AIC)",
                  "Discontinuous Rule Settlements - Fitted Linear Model",
                  "Discontinuous Rule - Predicted Trial Outcome",
                  "Linear Rule Settlements  (as Fraction of AIC)",
                  "Linear Rule Settlements - Fitted Linear Model",
                  "Linear Rule - Predicted Trial Outcome"),
       lty = c(NA,1,2,NA,1,3), pch = c(19, NA, NA, 19, NA, NA), 
       lwd = c(NA,1,2,NA,1,2), pt.cex = c(.25, NA, NA, .25, NA, NA),
       col = c("Red", "Red","Black", "Blue", "Blue", "Black"), cex = .7)

sd(Sample_Disc_Settlement_Amount_Over_Damages2 )
sd(Sample_Linear_Settlement_Amount_Over_Damages2 )

## Histogram of trial costs for cases close to median

subset_close_to_median_damages = subset(CompareDF, Damages > median(Damages) - 50 & Damages < median(Damages) + 50)
log_Trial_Costs_Moderate_Damages_Cases = log(subset_close_to_median_damages$Cost_of_Trial)
hist(log_Trial_Costs_Moderate_Damages_Cases, axes=FALSE, breaks=1000,
     main = "Expected Trial Costs, AIC Ranging from $1119 to $1219",
     xlab = "Expected Trial Costs (Log Scale)", 
     xlim = log(c(10,1000000000))) 
axis(side=1, at=log(c(10,100,1000,10000,100000,1000000,10000000, 100000000, 1000000000)), 
     labels = c("$10","$100","$1K","$10K", "$100K", "$1M", "$10M", "$100M", "$1B"))
axis(side=2, at=seq(0, 2000, 500), labels = c(0, "", "1000", "", "2000"))
abline(v=quantile(log_Trial_Costs_Moderate_Damages_Cases), 
       lty=c(4,3,2,3,4),
       col=c("Black","Black","Red","Black","Black"))
legend("topright", inset = .01, 
       legend = c("Minimum", "25%", "Median", "75%", "Maximum"),
       lty=c(4,3,2,3,4), col=c("Black","Black","Red","Black","Black")) 

## Expected Settlement Outcomes versus Outcome Under Each Trial Burden (for all cases)

Disc_Settle_Subset = CompareDF[CompareDF$Disc_Settle_True_False == TRUE,]
Sample_Disc_Settle_Subset = Disc_Settle_Subset[sample(nrow(Disc_Settle_Subset), 1000),]

Disc_Settlement_Amount_Over_Damages = Disc_Settle_Subset$Disc_Settlement_Amount / Disc_Settle_Subset$Damages
Unbiased_Pred_Means_Disc_Settle_Subset = Disc_Settle_Subset$Unbiased_Pred_Mean

disc_settle_amount_fit = lm(Disc_Settlement_Amount_Over_Damages ~ Unbiased_Pred_Means_Disc_Settle_Subset)
summary(disc_settle_amount_fit)

Sample_Disc_Settlement_Amount_Over_Damages = Sample_Disc_Settle_Subset$Disc_Settlement_Amount / Sample_Disc_Settle_Subset$Damages
Sample_Unbiased_Pred_Means_Disc_Settle_Subset = Sample_Disc_Settle_Subset$Unbiased_Pred_Mean

Linear_Settle_Subset = CompareDF[CompareDF$Linear_Settle_True_False == TRUE,]
Sample_Linear_Settle_Subset = Linear_Settle_Subset[sample(nrow(Linear_Settle_Subset), 1000),]

Linear_Settlement_Amount_Over_Damages = Linear_Settle_Subset$Linear_Settlement_Amount / Linear_Settle_Subset$Damages
Unbiased_Pred_Means_Linear_Settle_Subset = Linear_Settle_Subset$Unbiased_Pred_Mean

Linear_settle_amount_fit = lm(Linear_Settlement_Amount_Over_Damages ~ Unbiased_Pred_Means_Linear_Settle_Subset)
summary(Linear_settle_amount_fit)

Sample_Linear_Settlement_Amount_Over_Damages = Sample_Linear_Settle_Subset$Linear_Settlement_Amount / Sample_Linear_Settle_Subset$Damages
Sample_Unbiased_Pred_Means_Linear_Settle_Subset = Sample_Linear_Settle_Subset$Unbiased_Pred_Mean

plot(Sample_Disc_Settlement_Amount_Over_Damages ~ Sample_Unbiased_Pred_Means_Disc_Settle_Subset,
     pch=19, cex=0.05, ylim = c(0,8),
     main = "Discontinuous v. Linear Burdens - Predicted Settlements (All Cases)",
     xlab = "Unbiased Prediction of Mean Jury Confidence Level", ylab = "Settlement Amount as a Proportion of AIC", col="Red")
abline(disc_settle_amount_fit, col="Red")
points(Sample_Linear_Settlement_Amount_Over_Damages ~ Sample_Unbiased_Pred_Means_Linear_Settle_Subset,
       pch=19, cex=0.05, ylim = c(0,8), col = "Blue")
abline(Linear_settle_amount_fit, col="Blue")

legend("topleft", inset = .05, 
       legend = c("Discontinuous Rule Settlements - Best Fit Model", 
                  "Linear Rule Settlements - Best Fit Model"),
       lty = c(1,1), col = c("Red", "Blue"), cex = .8)

segments(0,0,0.5,0, lty = 2, lwd=2)
segments(0.5,0,0.5,1, lty = 2, lwd=2)
segments(0.5,1,1,1, lty = 2, lwd=2)

segments(0,0,1,1, lty = 3, lwd=2)

legend("topleft", inset = .05, 
       legend = c("Discontinuous Rule Settlements - Best Fit Model",
                  "Discontinuous Rule - Predicted Trial Outcome",
                  "Linear Rule Settlements - Best Fit Model",
                  "Linear Rule - Predicted Trial Outcome"),
       lty = c(1,2,1,3), col = c("Red", "Black", "Blue", "Black"), lwd = c(2,2,2,2), cex = .8)

## Visualizing expected settlements on a log scale

LargerSample_Disc_Settle_Subset= Disc_Settle_Subset[sample(nrow(Disc_Settle_Subset), 5000),]
LargerSample_Disc_Settlement_Amount_Over_Damages = 
  LargerSample_Disc_Settle_Subset$Disc_Settlement_Amount / LargerSample_Disc_Settle_Subset$Damages
LargerSample_Unbiased_Pred_Means_Disc_Settle_Subset = LargerSample_Disc_Settle_Subset$Unbiased_Pred_Mean

loggedLargerSample_Disc_Settlement_Amount_Over_Damages = 
  log(LargerSample_Disc_Settlement_Amount_Over_Damages)[LargerSample_Disc_Settlement_Amount_Over_Damages != 0]
matchedLargerSample_Unbiased_Pred_Means_Disc_Settle_Subset = 
  LargerSample_Unbiased_Pred_Means_Disc_Settle_Subset[LargerSample_Disc_Settlement_Amount_Over_Damages != 0]

LargerSample_Linear_Settle_Subset= Linear_Settle_Subset[sample(nrow(Linear_Settle_Subset), 5000),]
LargerSample_Linear_Settlement_Amount_Over_Damages = 
  LargerSample_Linear_Settle_Subset$Linear_Settlement_Amount / LargerSample_Linear_Settle_Subset$Damages
LargerSample_Unbiased_Pred_Means_Linear_Settle_Subset = LargerSample_Linear_Settle_Subset$Unbiased_Pred_Mean

loggedLargerSample_Linear_Settlement_Amount_Over_Damages = 
  log(LargerSample_Linear_Settlement_Amount_Over_Damages)[LargerSample_Linear_Settlement_Amount_Over_Damages != 0]
matchedLargerSample_Unbiased_Pred_Means_Linear_Settle_Subset = 
  LargerSample_Unbiased_Pred_Means_Linear_Settle_Subset[LargerSample_Linear_Settlement_Amount_Over_Damages != 0]

length(loggedLargerSample_Linear_Settlement_Amount_Over_Damages)

plot(loggedLargerSample_Disc_Settlement_Amount_Over_Damages ~ matchedLargerSample_Unbiased_Pred_Means_Disc_Settle_Subset,
     pch=19, cex=0.05, ylim = c(log(0.0005),log(5000)), axes=FALSE,
     main = "Discontinuous v. Linear Burdens - Predicted Settlement Ratios (All Cases)",
     xlab = "Unbiased Prediction of Mean Jury Confidence Level", 
     ylab = "Settlement Amount as a Proportion of AIC (log scale)", col="Red")
points(loggedLargerSample_Linear_Settlement_Amount_Over_Damages ~ matchedLargerSample_Unbiased_Pred_Means_Linear_Settle_Subset,
       pch=19, cex=0.05, col = "Blue")

axis(side=2, at=log(c(.001,.01,.1,1,10,100,1000)), 
     labels = c("D/1000","D/100", "D/10","D","10*D", "100*D","1000*D"), gap.axis = 0.05, cex.axis=0.8)

axis(side=1)

#Error rate analysis

summary(CompareDF)

disc_settle_error_rate = mean(CompareDF$Expected_Disc_Settlement_Error, na.rm = TRUE)
linear_settle_error_rate = mean(CompareDF$Expected_Linear_Settlement_Error, na.rm = TRUE)
settlement_error_means = as.table(rbind(disc_settle_error_rate, linear_settle_error_rate))
settlement_error_means
t.test(CompareDF$Expected_Disc_Settlement_Error, CompareDF$Expected_Linear_Settlement_Error)

disc_settle_error_over_damages_mean = mean(CompareDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
linear_settle_error_over_damages_mean = mean(CompareDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
settlement_error_over_damages_means = as.table(rbind(disc_settle_error_over_damages_mean, linear_settle_error_over_damages_mean))
settlement_error_over_damages_means
t.test(CompareDF$Expected_Disc_Settlement_Error_over_Damages, CompareDF$Expected_Linear_Settlement_Error_over_Damages)

## This part was used to investigate whether behavior was very different 
## in the subsets of cases that settled under one rule but not the other

x_axis_max = max(
        c(
                max(CompareDF$Expected_Disc_Settlement_Error, na.rm = TRUE),
                max(CompareDF$Expected_Linear_Settlement_Error, na.rm = TRUE)
        ),
        na.rm = TRUE
)

## First let's look at the cases (a fair number) that only settled using the linear burden

subsetDF = subset(CompareDF, Disc_Settle_True_False == FALSE & Linear_Settle_True_False == TRUE)
nrow(subsetDF)/nrow(CompareDF)
summary(subsetDF)
hist(subsetDF$Expected_Linear_Settlement_Error,
     main="Expected Errors for Cases That Only Settle with Continuous Burden",
     breaks=seq(0,round(max(subsetDF$Expected_Linear_Settlement_Error), -3) + 1000, by=1000),
     xlim = c(0,x_axis_max), xlab = "Expected Error of Settlement",
     sub=paste(
             "Mean Expected Error is $", round(mean(subsetDF$Expected_Linear_Settlement_Error), 0), 
             "; Median Expected Error is $", round(median(subsetDF$Expected_Linear_Settlement_Error), 0)))
abline(v=quantile(subsetDF$Expected_Linear_Settlement_Error), lty=3)
abline(v=median(subsetDF$Expected_Linear_Settlement_Error), lty=2, col="red")
abline(v=mean(subsetDF$Expected_Linear_Settlement_Error), lty=2, col="blue")

hist(subsetDF$Expected_Linear_Settlement_Error,
     main="Expected Errors for Cases That Only Settle with Continuous Burden",
     breaks=seq(0,round(max(subsetDF$Expected_Linear_Settlement_Error), -3) + 1000, by=10),
     xlim = c(0,x_axis_max/1000), xlab = "Expected Error of Settlement",
     sub=paste(
       "Mean Expected Error is $", round(mean(subsetDF$Expected_Linear_Settlement_Error), 0), 
       "; Median Expected Error is $", round(median(subsetDF$Expected_Linear_Settlement_Error), 0)))
abline(v=quantile(subsetDF$Expected_Linear_Settlement_Error), lty=3)
abline(v=median(subsetDF$Expected_Linear_Settlement_Error), lty=2, col="red")
abline(v=mean(subsetDF$Expected_Linear_Settlement_Error), lty=2, col="blue")


## Now let's look at the cases (a smaller number) that only settled using the discontinuous burden

subset2DF = subset(CompareDF, Disc_Settle_True_False == TRUE & Linear_Settle_True_False == FALSE)
nrow(subset2DF)/nrow(CompareDF)
summary(subset2DF)
hist(subset2DF$Expected_Disc_Settlement_Error,
     main="Expected Errors for Cases That Only Settle with Discontinuous Burden",
     breaks=seq(0,round(max(subset2DF$Expected_Disc_Settlement_Error), -3) + 1000, by=1000),
     xlim = c(0,x_axis_max), xlab = "Expected Error of Settlement",
     sub=paste(
       "Mean Expected Error is $", round(mean(subset2DF$Expected_Disc_Settlement_Error), 0), 
       "; Median Expected Error is $", round(median(subset2DF$Expected_Disc_Settlement_Error), 0)))
abline(v=quantile(subset2DF$Expected_Disc_Settlement_Error), lty=3)
abline(v=median(subset2DF$Expected_Disc_Settlement_Error), lty=2, col="red")
abline(v=mean(subset2DF$Expected_Disc_Settlement_Error), lty=2, col="blue")

hist(subset2DF$Expected_Disc_Settlement_Error,
     main="Expected Errors for Cases That Only Settle with Discontinuous Burden",
     breaks=seq(0,round(max(subset2DF$Expected_Disc_Settlement_Error), -3) + 1000, by=10),
     xlim = c(0,x_axis_max/1000),  xlab = "Expected Error of Settlement",
     sub=paste(
       "Mean Expected Error is $", round(mean(subset2DF$Expected_Disc_Settlement_Error), 0), 
       "; Median Expected Error is $", round(median(subset2DF$Expected_Disc_Settlement_Error), 0)))
abline(v=quantile(subset2DF$Expected_Disc_Settlement_Error), lty=3)
abline(v=median(subset2DF$Expected_Disc_Settlement_Error), lty=2, col="red")
abline(v=mean(subset2DF$Expected_Disc_Settlement_Error), lty=2, col="blue")

## Side-by-Side Values

paste("Mean Expected Error for Cases That Only Settle Under Linear Burden is $", 
  round(mean(subsetDF$Expected_Linear_Settlement_Error), 0),sep="")
paste("Median Expected Error for Cases That Only Settle Under Linear Burden is $", 
  round(median(subsetDF$Expected_Linear_Settlement_Error), 0), sep="")

paste("Mean Expected Error for Cases That Only Settle Under Discontinuous Burden is $",
      round(mean(subset2DF$Expected_Disc_Settlement_Error), 0),sep="") 
paste("Median Expected Error for Cases That Only Settle Under Discontinuous Burden is $", 
      round(median(subset2DF$Expected_Disc_Settlement_Error), 0),sep="")


##########################################

## Looking at Overall Errors/Damages Distribution


## By Damages Amounts

expected_errors_over_damages_bydamages = matrix(
  c(
    mean(subset(CompareDF, Damages < 5000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages < 5000)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 5000 & Damages < 10000)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),

    mean(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 10000 & Damages < 25000)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),

    mean(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 25000 & Damages < 100000)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),

    mean(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 100000 & Damages < 1000000)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Damages >= 1000000)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Damages >= 1000000)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(CompareDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE), 
    mean(CompareDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
  ),
  nrow=2, ncol=7)        


barplot((expected_errors_over_damages_bydamages), ylim = c(0,10), main="Expected Errors Under Each Rule", 
        col = c("Black", "Blue"), beside=TRUE,
        xlab = "Amount in Controversy", ylab="Expected Amount of Error (in proportion to AIC)",
        names = c("<5K", "5K-10K", "10K-25K", "25K-100K", "100K-1M", ">1M","All"), cex.names = .75)
legend("topright", inset = 0.05, legend=c("Continuous (Linear) Burden", "Discontinuous Burden"), 
       fill=c("blue", "black"), cex=1)

nrow(subset(CompareDF, Damages > 100000)) / nrow(CompareDF)

nrow(subset(CompareDF, Damages < 5000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 5000 & Damages < 10000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 10000 & Damages < 25000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 25000 & Damages < 100000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages >= 100000 & Damages < 1000000)) / nrow(CompareDF)
nrow(subset(CompareDF, Damages > 1000000)) / nrow(CompareDF)

small_subsetDF = subset(CompareDF, Damages < 5000)
small_disc_settle_errors_over_damages_mean = mean(small_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
small_linear_settle_errors_over_damages_mean = mean(small_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
small_settlement_error_over_damages_means = as.table(
  rbind(small_disc_settle_errors_over_damages_mean, small_linear_settle_errors_over_damages_mean))
small_settlement_error_over_damages_means
t.test(small_subsetDF$Expected_Disc_Settlement_Error_over_Damages, small_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

medium_subsetDF = subset(CompareDF, Damages > 5000 & Damages <100000)
medium_disc_settle_errors_over_damages_mean = mean(medium_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
medium_linear_settle_errors_over_damages_mean = mean(medium_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
medium_settlement_error_over_damages_means = as.table(
  rbind(medium_disc_settle_errors_over_damages_mean, medium_linear_settle_errors_over_damages_mean))
medium_settlement_error_over_damages_means
t.test(medium_subsetDF$Expected_Disc_Settlement_Error_over_Damages, medium_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

medium1_subsetDF = subset(CompareDF, Damages > 5000 & Damages <10000)
medium1_disc_settle_errors_over_damages_mean = mean(medium1_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
medium1_linear_settle_errors_over_damages_mean = mean(medium1_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
medium1_settlement_error_over_damages_means = as.table(
  rbind(medium1_disc_settle_errors_over_damages_mean, medium1_linear_settle_errors_over_damages_mean))
medium1_settlement_error_over_damages_means
t.test(medium1_subsetDF$Expected_Disc_Settlement_Error_over_Damages, medium1_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

medium2_subsetDF = subset(CompareDF, Damages > 10000 & Damages <25000)
medium2_disc_settle_errors_over_damages_mean = mean(medium2_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
medium2_linear_settle_errors_over_damages_mean = mean(medium2_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
medium2_settlement_error_over_damages_means = as.table(
  rbind(medium2_disc_settle_errors_over_damages_mean, medium2_linear_settle_errors_over_damages_mean))
medium2_settlement_error_over_damages_means
t.test(medium2_subsetDF$Expected_Disc_Settlement_Error_over_Damages, medium2_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

medium3_subsetDF = subset(CompareDF, Damages > 25000 & Damages <100000)
medium3_disc_settle_errors_over_damages_mean = mean(medium3_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
medium3_linear_settle_errors_over_damages_mean = mean(medium3_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
medium3_settlement_error_over_damages_means = as.table(
  rbind(medium3_disc_settle_errors_over_damages_mean, medium3_linear_settle_errors_over_damages_mean))
medium3_settlement_error_over_damages_means
t.test(medium3_subsetDF$Expected_Disc_Settlement_Error_over_Damages, medium3_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

large_subsetDF = subset(CompareDF, Damages > 100000)
large_disc_settle_errors_over_damages_mean = mean(large_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
large_linear_settle_errors_over_damages_mean = mean(large_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
large_settlement_error_over_damages_means = as.table(
  rbind(large_disc_settle_errors_over_damages_mean, large_linear_settle_errors_over_damages_mean))
large_settlement_error_over_damages_means
t.test(large_subsetDF$Expected_Disc_Settlement_Error_over_Damages, large_subsetDF$Expected_Linear_Settlement_Error_over_Damages)



### So linear rule is better for small cases, with no difference for moderate cases, 
## and with the discontinuous rule advantaged over 100K

## Now lets check out case strength as a variable

error_over_damages_by_case_strength = matrix(
  c(
    mean(subset(CompareDF, Unbiased_Pred_Mean < .1)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean < .1)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),

    mean(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),

    mean(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),

    mean(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
  
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(CompareDF, Unbiased_Pred_Mean >= .9)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(CompareDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE),
    mean(CompareDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE) 
  ),
  nrow=2, ncol=11)        

barplot((error_over_damages_by_case_strength), ylim = c(0,8), main="Expected Errors Under Each Rule, by Case Strength", 
        col = c("Black", "Blue"), beside=TRUE,
        xlab = "Predicted Jury Confidence Level", ylab="Expected Amount of Error (in proportion to AIC)",
        names = c("<.1",".1-.2",".2-.3", ".3-.4", ".4-.5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", ">.9", "All"), cex.names = .9)
legend("topright", inset = 0.01, legend=c("Continuous (Linear) Burden", "Discontinuous Burden"), 
       fill=c("blue", "black"), cex=1)

easy_subsetDF = subset(CompareDF, (Unbiased_Pred_Mean < .2 | Unbiased_Pred_Mean > .8))
easy_cases_disc_settle_error_over_damages_mean = mean(
 easy_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
easy_cases_linear_settle_error_over_damages_mean = mean(
 easy_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
easy_cases_settlement_error_over_damages_means = as.table(
  rbind(easy_cases_disc_settle_error_over_damages_mean,easy_cases_linear_settle_error_over_damages_mean))
easy_cases_settlement_error_over_damages_means
t.test(easy_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
      easy_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

veryeasy_subsetDF = subset(CompareDF, (Unbiased_Pred_Mean < .1 | Unbiased_Pred_Mean > .9))
veryeasy_cases_disc_settle_error_over_damages_mean = mean(
  veryeasy_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
veryeasy_cases_linear_settle_error_over_damages_mean = mean(
  veryeasy_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
veryeasy_cases_settlement_error_over_damages_means = as.table(
  rbind(veryeasy_cases_disc_settle_error_over_damages_mean,veryeasy_cases_linear_settle_error_over_damages_mean))
veryeasy_cases_settlement_error_over_damages_means
t.test(veryeasy_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       veryeasy_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

hard_subsetDF = subset(CompareDF, (Unbiased_Pred_Mean > .2 & Unbiased_Pred_Mean < .8))
hard_cases_disc_settle_error_over_damages_mean = mean(
  hard_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
hard_cases_linear_settle_error_over_damages_mean = mean(
  hard_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
hard_cases_settlement_error_over_damages_means = as.table(
  rbind(hard_cases_disc_settle_error_over_damages_mean,hard_cases_linear_settle_error_over_damages_mean))
hard_cases_settlement_error_over_damages_means
t.test(hard_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       hard_subsetDF$Expected_Linear_Settlement_Error_over_Damages)



## Might be clearer picture if we subset to Trial Costs < 100AIC

subsetDF_trialcosts_less_than_100XDamages = subset(CompareDF, Cost_of_Trial < 100*Damages)
nrow(subsetDF_trialcosts_less_than_100XDamages)

error_over_damages_by_case_strength_low_to_moderate_costs2 = matrix(
  c(
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean < .1)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean < .1)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .1 & Unbiased_Pred_Mean < .2)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .2 & Unbiased_Pred_Mean < .3)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .3 & Unbiased_Pred_Mean < .4)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .4 & Unbiased_Pred_Mean < .5)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .5 & Unbiased_Pred_Mean < .6)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .6 & Unbiased_Pred_Mean < .7)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .7 & Unbiased_Pred_Mean < .8)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .8 & Unbiased_Pred_Mean < .9)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .9)$Expected_Disc_Settlement_Error_over_Damages, na.rm=TRUE),
    mean(subset(subsetDF_trialcosts_less_than_100XDamages, Unbiased_Pred_Mean >= .9)$Expected_Linear_Settlement_Error_over_Damages, na.rm=TRUE),
    
    mean(subsetDF_trialcosts_less_than_100XDamages$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE),
    mean(subsetDF_trialcosts_less_than_100XDamages$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE) 
  ),
  nrow=2, ncol=11)        

barplot((error_over_damages_by_case_strength_low_to_moderate_costs2), 
        ylim = c(0,1.5), main="Expected Errors Under Each Rule, by Case Strength", sub = "Trial Costs < 100*AIC", 
        col = c("Black", "Blue"), beside=TRUE,
        xlab = "Predicted Jury Confidence Level", ylab="Expected Amount of Error (in proportion to AIC)",
        names = c("<.1",".1-.2",".2-.3", ".3-.4", ".4-.5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", ">.9", "All"), cex.names = .9)
legend("topright", inset = 0.01, legend=c("Continuous (Linear) Burden", "Discontinuous Burden"), 
       fill=c("blue", "black"), cex=1)


easy_affordable_subsetDF = subset(CompareDF, Cost_of_Trial < 100*Damages& (Unbiased_Pred_Mean < .2 | Unbiased_Pred_Mean > .8))
easy_affordable_cases_disc_settle_error_over_damages_mean = mean(
  easy_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
easy_affordable_cases_linear_settle_error_over_damages_mean = mean(
  easy_affordable_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
easy_affordable_cases_settlement_error_over_damages_means = as.table(
  rbind(easy_affordable_cases_disc_settle_error_over_damages_mean, easy_affordable_cases_linear_settle_error_over_damages_mean))
easy_affordable_cases_settlement_error_over_damages_means
t.test(easy_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       easy_affordable_subsetDF$Expected_Linear_Settlement_Error_over_Damages)

hard_affordable_subsetDF = subset(CompareDF, Cost_of_Trial < 100*Damages & (Unbiased_Pred_Mean > .2 & Unbiased_Pred_Mean < .8))
hard_affordable_cases_disc_settle_error_over_damages_mean = mean(
  hard_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, na.rm = TRUE)
hard_affordable_cases_linear_settle_error_over_damages_mean = mean(
  hard_affordable_subsetDF$Expected_Linear_Settlement_Error_over_Damages, na.rm = TRUE)
hard_affordable_cases_settlement_error_over_damages_means = as.table(
  rbind(hard_affordable_cases_disc_settle_error_over_damages_mean, hard_affordable_cases_linear_settle_error_over_damages_mean))
hard_affordable_cases_settlement_error_over_damages_means
t.test(hard_affordable_subsetDF$Expected_Disc_Settlement_Error_over_Damages, 
       hard_affordable_subsetDF$Expected_Linear_Settlement_Error_over_Damages)
