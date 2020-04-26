## Loading Settlement Comparison Simulation Function and its Associated Parameters

source('~/Dropbox/Continuous Burdens of Proof Project/DataSim of Settlements Under Different Burden Rules - Final 2020.R')

## Creating dataset of simulated cases for settlement behavior analysis

set.seed(20200424)

DF1 = settlement_comparison_simulation(number_of_rows = 50000)
DF2 = settlement_comparison_simulation(number_of_rows = 50000)
DF3 = settlement_comparison_simulation(number_of_rows = 50000)
DF4 = settlement_comparison_simulation(number_of_rows = 50000)
DF5 = settlement_comparison_simulation(number_of_rows = 50000)
DF6 = settlement_comparison_simulation(number_of_rows = 50000)
DF7 = settlement_comparison_simulation(number_of_rows = 50000)
DF8 = settlement_comparison_simulation(number_of_rows = 50000)
DF9 = settlement_comparison_simulation(number_of_rows = 50000)
DF10 = settlement_comparison_simulation(number_of_rows = 50000)
DF11 = settlement_comparison_simulation(number_of_rows = 50000)
DF12 = settlement_comparison_simulation(number_of_rows = 50000)
DF13 = settlement_comparison_simulation(number_of_rows = 50000)
DF14 = settlement_comparison_simulation(number_of_rows = 50000)
DF15 = settlement_comparison_simulation(number_of_rows = 50000)
DF16 = settlement_comparison_simulation(number_of_rows = 50000)
DF17 = settlement_comparison_simulation(number_of_rows = 50000)
DF18 = settlement_comparison_simulation(number_of_rows = 50000)
DF19 = settlement_comparison_simulation(number_of_rows = 50000)
DF20 = settlement_comparison_simulation(number_of_rows = 50000)

CompareDF = rbind.data.frame(DF1,DF2,DF3,DF4,DF5,
                             DF6,DF7,DF8,DF9,DF10,
                             DF11,DF12,DF13,DF14,DF15,
                             DF16,DF17,DF18,DF19,DF20)

write.csv(CompareDF, "CompareDF.csv")

summary(CompareDF)
