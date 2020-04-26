## This code was used to conduct the analysis reported in Part B of the paper's Appendix

## See Landscape Report, p. 24 for underlying distribution values for bench and jury trial outcomes

##### Combined Distribuiton Using get.lnorm.par

## Jury Trial Params

jury_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(7962-34, 31097-34, 201896-34))
set.seed(4242020)
jury_damages_raw_draws = rlnorm(500000, meanlog = jury_damage_params[1], sdlog = jury_damage_params[2]) + 34
length(jury_damages_raw_draws[jury_damages_raw_draws < 34]) / length(jury_damages_raw_draws)
summary(jury_damages_raw_draws)
quantile(jury_damages_raw_draws, seq(0,1,.05))
hist(jury_damages_raw_draws, breaks=100000, xlim = c(0,1000000))
hist(jury_damages_raw_draws, breaks=100000, xlim = c(0,100000))
mean(jury_damages_raw_draws)
sd(jury_damages_raw_draws)

## Bench Trial Params

bench_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(679-34, 1131-34, 2028-34))
set.seed(4242020)
bench_damages_raw_draws = rlnorm(500000, meanlog = bench_damage_params[1], sdlog = bench_damage_params[2]) + 34
length(bench_damages_raw_draws[bench_damages_raw_draws < 34]) / length(bench_damages_raw_draws)
summary(bench_damages_raw_draws)
quantile(bench_damages_raw_draws, seq(0,1,.05))
hist(bench_damages_raw_draws, breaks=10000, xlim = c(0,100000))
hist(bench_damages_raw_draws, breaks=10000, xlim = c(0,10000))
mean(bench_damages_raw_draws)
sd(bench_damages_raw_draws)

## Combining into a single distribution


jury_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(7962-34, 31097-34, 201896-34))
bench_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(679-34, 1131-34, 2028-34))

DF_damages = data.frame(
        damages = numeric()
)

for (i in 1:100) {
        pick_a_number = runif(1,1,11481)
        if (pick_a_number < 195) {
                damages_jury = rlnorm(1, meanlog = jury_damage_params[1], sdlog = jury_damage_params[2]) +34
                new_row = data.frame(damages = damages_jury)
                
                
        } else {
                damages_bench = rlnorm(1, meanlog = bench_damage_params[1], sdlog = bench_damage_params[2]) +34
                new_row = data.frame(damages = damages_bench)
        }
        
        DF_damages = rbind.data.frame(DF_damages, new_row)
}

summary(DF_damages)
quantile(DF_damages$damages, seq(0,1,0.05))
hist(DF_damages$damages, breaks=1000)
hist(DF_damages$damages, breaks=100000, xlim = c(0,100000))
hist(DF_damages$damages, breaks=100000, xlim = c(0,10000))

## Summarizing paper's Damages dataset


