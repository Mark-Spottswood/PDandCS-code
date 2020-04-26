## Figures and Analysis for Part I-B

log_Damages = log(CompareDF$Damages)
hist(log_Damages, axes=FALSE, breaks=1000,ylim=c(0,10000),
     main = "Expected Damages If Case Is Tried",
     xlab = "Expected Damages (Log Scale)") 
axis(side=1, at=log(c(10,100,1000,10000,100000,1000000,10000000, 100000000, 1000000000)), 
     labels = c("$10","$100","$1K","$10K", "$100K", "$1M", "$10M", "$100M", "$1B")
)
axis(side=2, at=seq(0, 10000, 2500), labels = c(0, "2500", "5000", "7500", "10000"))


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

max(CompareDF$Total_Costs)
