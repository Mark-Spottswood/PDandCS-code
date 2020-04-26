
## Utah Report -- Debt Collection Cases -- 25/50/75% -> 260/2698/14208
## Landscape report lists these as .237 (.37*.64) of all cases
## To avoid too many low values, minimum value will be set to $100 (cost of filing in small claims court)

debt_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(260-100, 2698-100, 14208-100))
debt_cost_draws = rlnorm(500000, meanlog = debt_cost_params[1], sdlog = debt_cost_params[2]) + 100
mean(debt_cost_draws)
sd(debt_cost_draws)
quantile(debt_cost_draws, c(0.25,.5,.75))

## Utah Report -- Auto Tort Cases -- 25/50/75% -> 19888/45375/122163
## Landscape report lists these as .028 (.4 * .07) of all cases

auto_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(19888, 45375, 122163))
auto_cost_draws = rlnorm(500000, meanlog = auto_cost_params[1], sdlog = auto_cost_params[2])
mean(auto_cost_draws)
sd(auto_cost_draws)
quantile(auto_cost_draws, c(0.25,.5,.75))

## Utah Report - Professional Malpractice -- 25/50/75% -> 56880/135950/333275
## These comprise .0035 (.07 * .05) of all cases in Landmark survey

malpractice_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(56880, 135950, 333275))
malpractice_cost_draws = rlnorm(500000, meanlog = malpractice_cost_params[1], sdlog = malpractice_cost_params[2])
mean(malpractice_cost_draws)
sd(malpractice_cost_draws)
quantile(malpractice_cost_draws, c(0.25,.5,.75))

## Renormalizing

## Total Case Space Covered Is .0035 + .028 + .237 = .2685
## So Proportional Share of Cases Is .88 (debt), .1 (auto), and .01 (medmal)

#### Combining into a single distribution, and removing values greater than max litigation costs reported in U.S.C.C. survey) 

debt_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(260-100, 2698-100, 14208-100))
auto_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(19888, 45375, 122163))
malpractice_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(56880, 135950, 333275))

DF_costs = data.frame(
        pick_a_number = numeric(),
        Naive_Costs = numeric()
)

for (i in 1:100000) {
        pick_a_number = runif(1,0,100)
        
        repeat {
                if (pick_a_number < 88) {
                        Naive_Costs = rlnorm(1, meanlog = debt_cost_params[1], sdlog = debt_cost_params[2]) + 100
                }
                
                if ((pick_a_number >= 88) & (pick_a_number < 99)) {
                        Naive_Costs = rlnorm(1, meanlog = auto_cost_params[1], sdlog = auto_cost_params[2])
                }
                
                if (pick_a_number >= 99)  {
                        Naive_Costs = rlnorm(1, meanlog = malpractice_cost_params[1], sdlog = malpractice_cost_params[2])
                }
                
                if (Naive_Costs < 11184989) {break}
        }
        
        
        
        new_row = data.frame(pick_a_number = pick_a_number, Naive_Costs = Naive_Costs)
        DF_costs  = rbind.data.frame(DF_costs , new_row)
}

summary(DF_costs)
quantile(DF_costs $Naive_Costs, seq(0,1,0.05))

log_Naive_Costs = log(DF_costs $Naive_Costs)
hist(log_Naive_Costs, axes=FALSE, breaks=1000,
     main = "Unadjusted Per-Side Litigation Costs If Case Is Tried",
     xlab = "Total Expected Per-Side Litigation Costs (Log Scale)", 
     xlim = log(c(10,1000000000))) 
axis(side=1, at=log(c(10,100,1000,10000,100000,1000000,10000000, 100000000, 1000000000)), 
     labels = c("$10","$100","$1K","$10K", "$100K", "$1M", "$10M", "$100M", "$1B")
)
axis(side=2, at=seq(0, 2000, 500), labels = c(0, "", "1000", "", "2000"))

## Adjusting to Maintain .25 correlation between costs and damages

jury_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(7962-34, 31097-34, 201896-34))
bench_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(679-34, 1131-34, 2028-34))
debt_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(260, 2698, 14208))
auto_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(19888, 45375, 122163))
malpractice_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(56880, 135950, 333275))



DF = data.frame(Damages = numeric(), 
                Naive_Costs=numeric(), 
                Total_Costs=numeric()
)


for (i in 1:10000) {
        
        pick_a_number_Damages = runif(1,1,11481)
        if (pick_a_number_Damages < 195) {
                Damages = rlnorm(1, meanlog = jury_damage_params[1], sdlog = jury_damage_params[2]) +34
        } else {
                Damages = rlnorm(1, meanlog = bench_damage_params[1], sdlog = bench_damage_params[2]) +34
        }

        pick_a_number_Costs = runif(1,0,100)
        
        repeat {
                if (pick_a_number_Costs < 88) {
                        Naive_Costs = rlnorm(1, meanlog = debt_cost_params[1], sdlog = debt_cost_params[2]) + 100
                }
                
                if ((pick_a_number_Costs >= 88) & (pick_a_number_Costs < 99)) {
                        Naive_Costs = rlnorm(1, meanlog = auto_cost_params[1], sdlog = auto_cost_params[2])
                }
                
                if (pick_a_number_Costs >= 99)  {
                        Naive_Costs = rlnorm(1, meanlog = malpractice_cost_params[1], sdlog = malpractice_cost_params[2])
                }
                
                if (Naive_Costs < 11184989) {break}
        }

        if (Damages > Naive_Costs) {Total_Costs = Naive_Costs + (.25 * runif(1, min=0, max=(Damages - Naive_Costs)))}
        if (Damages == Naive_Costs) {Total_Costs = Naive_Costs}
        if (Damages < Naive_Costs) {Total_Costs = Naive_Costs - (.25 * runif(1, min=0, max=(Naive_Costs - Damages)))}
        
        new_row = data.frame(
                Damages = Damages, 
                Naive_Costs=Naive_Costs, 
                Total_Costs=Total_Costs
        )
        
        DF = rbind.data.frame(DF, new_row)
}

summary(DF)

## Checking Fit -- .25 log-log correlation per Lee & Willging (FJC 2010)

fit_log = lm(log(Total_Costs) ~ log(Damages), data=DF)
summary(fit_log)

plot(Total_Costs ~ Damages, data= DF, xlim = c(0,20000), ylim=c(0,200000), pch=19, cex=0.1)
x_values = data.frame(Damages = exp(seq(0,100,by=0.1)))
pred_y = predict(fit_log, x_values)
lines(exp(pred_y), x_values$Damages, col="Red")

subsetDF = subset(DF, DF$Damages<1000000 & DF$Total_Costs<1000000)
fit_log_subset = lm(log(Total_Costs) ~ log(Damages), data=subsetDF)
summary(fit_log_subset)

subset2DF = subset(DF, DF$Damages<100000 & DF$Total_Costs<100000)
fit_log_subset2 = lm(log(Total_Costs) ~ log(Damages), data=subset2DF)
summary(fit_log_subset2)

log_Total_Costs = log(CompareDF$Total_Costs)
hist(log_Total_Costs, axes=FALSE, breaks=1000,
     main = "Expected Overall Litigation Costs If Case Is Tried",
     xlab = "Expected Overall Litigation Costs (Log Scale)", 
     xlim = log(c(10,1000000000))) 
axis(side=1, at=log(c(10,100,1000,10000,100000,1000000,10000000, 100000000, 1000000000)), 
     labels = c("$10","$100","$1K","$10K", "$100K", "$1M", "$10M", "$100M", "$1B")
)
axis(side=2, at=seq(0, 4000, 500), labels = c(0, "", "1000", "", "2000", "", "3000", "", "4000"))

fit_log = lm(log(Total_Costs) ~ log(Damages), data=CompareDF)
summary(fit_log)


################

## Defining settlement and going to trial costs
## including their reasonableness beyond the sample space

## Utah Report Includes Most Small Cases
## Code Below Averages Settlement Effort Fractions from Utah Report Across Relevant Case Types, 
## yielding average settle effort proportion of .065 of total costs,
## with noise added in with sd of .1 of the average settling cost

auto_settle_effort = c(.055, .077, .094)
premises_settle_effort = c(.096, .068, .092)
malpractice_settle_effort = c(.044, .056, .043)
business_settle_effort = c(.044, .059, .059)
employment_settle_effort = c(.082, .06, .044)
debt_settle_effort = c(.038, .069, .096)
combined_settle_effort = c(auto_settle_effort, premises_settle_effort, malpractice_settle_effort, business_settle_effort,
                           employment_settle_effort, debt_settle_effort)

average_settle_proportion = mean(combined_settle_effort)
settle_costs = total_costs*average_settle_proportion + rnorm(1,0,total_costs*average_settle_proportion/5)

log_Settle_Costs = log(CompareDF$Cost_of_Settlement)
hist(log_Settle_Costs, axes=FALSE, breaks=1000, ylim = c(0,4100),
     main = "Expected Settlement Costs",
     xlab = "Expected Settlement Costs (Log Scale)", 
     xlim = log(c(10,1000000000))) 
axis(side=1, at=log(c(10,100,1000,10000,100000,1000000,10000000, 100000000, 1000000000)), 
     labels = c("$10","$100","$1K","$10K", "$100K", "$1M", "$10M", "$100M", "$1B")
)
axis(side=2, at=seq(0, 4000, 500), labels = c(0, "", "1000", "", "2000", "", "3000", "", "4000"))

######Finding proprotion of overall effort devoted to trying cases, 
## using attorney time fractions from Utah Report, with mean of .375 of total effort

auto_trial_effort = c(.519, .428, .341)
premises_trial_effort = c(.708, .496, .38)
malpractice_trial_effort = c(.397, .4, .043)
business_trial_effort = c(.6, .413, .367)
employment_trial_effort = c(.431, .395, .422)
debt_trial_effort = c(0, .138, .275)
combined_trial_effort = c(auto_trial_effort, premises_trial_effort, malpractice_trial_effort, business_trial_effort,
                           employment_trial_effort, debt_trial_effort)

average_trial_proportion = mean(combined_trial_effort)
trial_costs = total_costs*average_trial_proportion + rnorm(1,0,total_costs*average_trial_proportion/5)

log_Trial_Costs = log(CompareDF$Cost_of_Trial)
hist(log_Trial_Costs, axes=FALSE, breaks=1000, ylim = c(0,4100),
     main = "Expected Trial Costs",
     xlab = "Expected Trial Costs (Log Scale)", 
     xlim = log(c(10,1000000000))) 
axis(side=1, at=log(c(10,100,1000,10000,100000,1000000,10000000, 100000000, 1000000000)), 
     labels = c("$10","$100","$1K","$10K", "$100K", "$1M", "$10M", "$100M", "$1B")
)
axis(side=2, at=seq(0, 4000, 500), labels = c(0, "", "1000", "", "2000", "", "3000", "", "4000"))


## Illustrating relationship between simulated trial and settlement costs

total_costs = seq(100,10000,1)

av_settle_costs = numeric()
for (i in 1:length(total_costs)) {av_settle_costs[i] = total_costs[i]*average_settle_proportion}
settle_costs = numeric()
for (i in 1:length(total_costs)) {settle_costs[i] = (total_costs[i]*average_settle_proportion) + 
        (rnorm(1, sd= (total_costs[i]*average_settle_proportion/5)))}

av_trial_costs = numeric()
for (i in 1:length(total_costs)) {av_trial_costs[i] = total_costs[i]*average_trial_proportion}
trial_costs = numeric()
for (i in 1:length(total_costs)) {trial_costs[i] = (total_costs[i]*average_trial_proportion) + 
        (rnorm(1, sd= (total_costs[i]*average_trial_proportion/5)))}


plot(av_settle_costs ~ total_costs, cex=.1, pch=19, col="Red", ylim = c(0,6000),
     main="Simulated Settlement and Trial Costs, by Total Costs", 
     ylab="Trial and Settlements Costs", xlab="Total Costs If Case Is Tried")
points(settle_costs ~ total_costs, cex=.01, pch=19, col="Red")
points(av_trial_costs ~ total_costs, cex=.1, pch=19, col="Blue")
points(trial_costs ~ total_costs, cex=.01, pch=19, col="Blue")

legend("topleft", inset=0.05,
       legend=c("Average Trial Cost", "Trial Costs with Error", "Average Settlement Cost", "Settlement Cost with Error"),
       col=c("Blue", "Blue", "Red", "Red"),
       lty=c(1,NA,1,NA), lwd = c(2,NA,2,NA),
       pch=c(NA, 19, NA, 19), pt.cex=c(NA, 0.1, NA, 0.1), 
       cex=0.8)

## Showing relationship between trial costs and damages across the whole dataset

log_Trial_Costs_over_Damages = log(CompareDF$Cost_of_Trial / CompareDF$Damages)
hist(log_Trial_Costs_over_Damages, axes=FALSE, breaks=1000, ylim = c(0,5000),
     main = "Expected Trial Costs Over Damages",
     xlab = "Expected Trial Costs Over Damages (Log Scale)", 
     xlim = log(c(.0001,100000))) 
axis(side=1, at=log(c(.0001,.001,.01,.1,1,10,100,1000,10000,100000)), 
     labels = c(".0001",".001",".01",".1","1","10","100","1000","10,000", "100,000"))
axis(side=2, at=seq(0, 5000, 500), labels = c(0, "", "1000", "", "2000", "", "3000", "", "4000","" ,"5000"))
abline(v=mean(log_Trial_Costs_over_Damages), col="Red", lty=2)
abline(v=mean(log_Trial_Costs_over_Damages) + sd(log_Trial_Costs_over_Damages), col="Red", lty=3)
abline(v=mean(log_Trial_Costs_over_Damages) - sd(log_Trial_Costs_over_Damages), col="Red", lty=3)
legend("topright", inset=0.05,
       legend=c("Mean", "+/- 1SD"),
       col="Red",
       lty=c(2,3))