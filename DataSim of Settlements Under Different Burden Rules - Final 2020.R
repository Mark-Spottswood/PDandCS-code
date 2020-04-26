

## Defining settlement cost, trial cost and filing cost parameters -- see Paper's Appendix for more details

library(rriskDistributions)

jury_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(7962-34, 31097-34, 201896-34))
bench_damage_params = get.lnorm.par(p = c(.25, .5, .75), q = c(679-34, 1131-34, 2028-34))

debt_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(260-100, 2698-100, 14208-100))
auto_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(19888, 45375, 122163))
malpractice_cost_params = get.lnorm.par(p = c(.25, .5, .75), q = c(56880, 135950, 333275))

auto_settle_effort = c(.055, .077, .094)
premises_settle_effort = c(.096, .068, .092)
malpractice_settle_effort = c(.044, .056, .043)
business_settle_effort = c(.044, .059, .059)
employment_settle_effort = c(.082, .06, .044)
debt_settle_effort = c(.038, .069, .096)
combined_settle_effort = c(auto_settle_effort, premises_settle_effort, malpractice_settle_effort, business_settle_effort,
                           employment_settle_effort, debt_settle_effort)
average_settle_proportion = mean(combined_settle_effort)

auto_trial_effort = c(.519, .428, .341)
premises_trial_effort = c(.708, .496, .38)
malpractice_trial_effort = c(.397, .4, .043)
business_trial_effort = c(.6, .413, .367)
employment_trial_effort = c(.431, .395, .422)
debt_trial_effort = c(0, .138, .275)
combined_trial_effort = c(auto_trial_effort, premises_trial_effort, malpractice_trial_effort, business_trial_effort,
                          employment_trial_effort, debt_trial_effort)
average_trial_proportion = mean(combined_trial_effort)

## Define function to create a forecast of settlement behavior in simulated cases
## optimism_level must take a value between 0 and 1

settlement_comparison_simulation = function(forecast_precision = 25, optimism_level = .75, number_of_rows = 10000) {
  
        ## Keeping track of time
        start_time <- Sys.time()
        pb <- txtProgressBar(min = 0, max = number_of_rows, style = 3)

        ## Define logistic transform function
        
        r = 8.155
        A = 1 / ((1 / (1+exp(-r/2))) - (1 / (1+exp(r/2))))
        B = -A / (1+exp(r/2))
        
        logistic_outcome = function (p) {A / (1 + exp((-r*p) + (r/2))) + B}
        
        
        ## Define beta function with specified paramters to simulate estimated judgments of probabilistic likelihood of liability
        ## uses the mean and the variance (square of the SD) as arguments
        
          estBetaParams <- function(mu, var) {
          alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
          beta <- alpha * (1 / mu - 1)
          return(params = c(alpha,beta))
        }
        
              
        ## Make empty data frame
        
        Settlement_Data_Frame = data.frame(Unbiased_Pred_Mean = numeric(),
                                           Unbiased_Pred_Precision = numeric(),
                                           Unbiased_ProbV = numeric(),
                                           Pl_Pred_Mean = numeric(),
                                           PProbV = numeric(),
                                           Pl_Logit_Expectation = numeric(),
                                           Df_Pred_Mean = numeric(),
                                           DProbV = numeric(),
                                           Df_Logit_Expectation = numeric(),
                                           Pp_minus_Pd = numeric(),
                                           Mp_minus_Md = numeric(),
                                           LOGITp_minus_LOGITd = numeric(),
                                           Damages = numeric(), 
                                           Total_Costs=numeric(), 
                                           Cost_of_Trial=numeric(), 
                                           Cost_of_Settlement=numeric(), 
                                           Twice_Trial_minus_Settle_over_Damages = numeric(),
                                           Bargaining_Power = numeric(),
                                           Disc_Settle_True_False = logical(),
                                           Linear_Settle_True_False = logical(),
                                           Logistic_Settle_True_False = logical(),
                                           Disc_P_Settle_Bound = numeric(),
                                           Disc_D_Settle_Bound = numeric(),
                                           Linear_P_Settle_Bound = numeric(),
                                           Linear_D_Settle_Bound = numeric(),
                                           Logit_P_Settle_Bound = numeric(),
                                           Logit_D_Settle_Bound = numeric(),
                                           Disc_Settlement_Amount = numeric(),
                                           Linear_Settlement_Amount = numeric(),
                                           Logit_Settlement_Amount = numeric(),
                                           Expected_Disc_Settlement_Error = numeric(),
                                           Expected_Linear_Settlement_Error = numeric(),
                                           Expected_Logistic_Settlement_Error = numeric(),
                                           Expected_Disc_Sum_of_Squared_Error = numeric(),
                                           Expected_Linear_Sum_of_Squared_Error = numeric(),
                                           Expected_Logistic_Sum_of_Squared_Error = numeric(),
                                           Expected_Disc_Settlement_Error_over_Damages = numeric(),
                                           Expected_Linear_Settlement_Error_over_Damages = numeric(),
                                           Expected_Logistic_Settlement_Error_over_Damages = numeric(),
                                           Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages = numeric(),
                                           Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages = numeric(),
                                           Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages = numeric()
                                           
                                           
        )
        
        ## Fill up data frame
        
        for (i in 1:number_of_rows) {
                
                # update progress bar
                setTxtProgressBar(pb, i)
                
                ## Determine Value of Unbiased Prediction of Jury Confidence Distribution
                
                repeat {
                        Unbiased_Pred_Mean = runif(1,0.01,.99)
                        if (Unbiased_Pred_Mean > 1 - Unbiased_Pred_Mean) {
                                Precision_Max = (1-Unbiased_Pred_Mean) / 10
                        } else {
                                Precision_Max = (Unbiased_Pred_Mean) / 10
                        }
                        Unbiased_Pred_Precision = runif(1,0.0001,Precision_Max)
                        alpha = estBetaParams(Unbiased_Pred_Mean,Unbiased_Pred_Precision)[1]
                        beta =  estBetaParams(Unbiased_Pred_Mean,Unbiased_Pred_Precision)[2]
                        
                        if ((alpha >=1 | beta >= 1) & (alpha <= forecast_precision & beta <=forecast_precision)) {break}
                }
                
                ## Determine Values of Biased Estimates of that Distribution
          
                pl_pred = c(alpha + runif(1, 0, forecast_precision-alpha+0.001)*optimism_level, 
                            beta - runif(1,0, beta - 0.001)*optimism_level)
                df_pred = c(alpha - runif(1,0,alpha - 0.001)*optimism_level, 
                            beta + runif(1, 0, forecast_precision-beta+0.001)*optimism_level)
                Pl_Pred_Mean = pl_pred[1] / (pl_pred[1] + pl_pred[2])
                Df_Pred_Mean = df_pred[1] / (df_pred[1] + df_pred[2])
               
                ## Determine predicted victory probabilities under discontinuous 0.5 threshold rule
                
                unbiased_integrand <- function(x) {dbeta(x,alpha, beta)}
                Unbiased_ProbV = integrate(unbiased_integrand, lower = 0.5, upper = 1)
                PL_integrand <- function(x) {dbeta(x,pl_pred[1], pl_pred[2])}
                PL_PV = integrate(PL_integrand, lower = 0.5, upper = 1)
                PProbV = as.numeric(PL_PV[1])
                DF_integrand <- function(x) {dbeta(x,df_pred[1], df_pred[2])}
                DF_PV = integrate(DF_integrand, lower = 0.5, upper = 1)
                DProbV= as.numeric(DF_PV[1])
                Pp_minus_Pd = PProbV - DProbV
                Mp_minus_Md = Pl_Pred_Mean - Df_Pred_Mean
                
                ## Determine Expected Outcome Under Logistic Burden Rule
                
                PL_Logit_Integrand = function(x) {dbeta(x,pl_pred[1], pl_pred[2])*logistic_outcome(x)}
                PL_Logit_Outcome = integrate(PL_Logit_Integrand, lower = 0, upper = 1)
                Pl_Logit_Expectation = as.numeric(PL_Logit_Outcome[1])
                
                Df_Logit_Integrand = function(x) {dbeta(x,df_pred[1], df_pred[2])*logistic_outcome(x)}
                Df_Logit_Outcome = integrate(Df_Logit_Integrand, lower = 0, upper = 1)
                Df_Logit_Expectation = as.numeric(Df_Logit_Outcome[1])
                
                LOGITp_minus_LOGITd = Pl_Logit_Expectation - Df_Logit_Expectation
                
                ## Generate value for total amount in controversy
                
                pick_a_number_Damages = runif(1,1,11675)
                if (pick_a_number_Damages < 195) {
                  Damages = rlnorm(1, meanlog = jury_damage_params[1], sdlog = jury_damage_params[2]) +34
                } else {
                  Damages = rlnorm(1, meanlog = bench_damage_params[1], sdlog = bench_damage_params[2]) +34
                }
                
                ## Generate value for total litigation costs
                
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
                
                ## Generate Values for Cost of Settlement and Cost of Trial
                
                Cost_of_Settlement = Total_Costs*average_settle_proportion + rnorm(1,0,Total_Costs*average_settle_proportion/5)
                Cost_of_Trial = Total_Costs*average_trial_proportion + rnorm(1,0,Total_Costs*average_trial_proportion/5)
                
                ## Determine Whether Case Will Settle Under Different Rules
                
                Twice_Trial_minus_Settle_over_Damages = (2*(Cost_of_Trial - Cost_of_Settlement)) / Damages
                
                Disc_Settle_True_False =  as.logical( (as.numeric(PL_PV[1]) - as.numeric(DF_PV[1]))*Damages 
                                                 < 2*(Cost_of_Trial - Cost_of_Settlement) )
                Linear_Settle_True_False = as.logical((Pl_Pred_Mean*Damages - Df_Pred_Mean*Damages) 
                                                  < 2*(Cost_of_Trial - Cost_of_Settlement))
                Logistic_Settle_True_False = as.logical((Pl_Logit_Expectation*Damages - Df_Logit_Expectation*Damages) 
                                                        < 2*(Cost_of_Trial - Cost_of_Settlement))
                
                ## Calculate a Settlement Amount for Cases That Settle Under Each Rule
                
                Bargaining_Power = rbeta(1,12,12)
                
                Disc_P_Settle_Bound = PProbV*Damages  - Cost_of_Trial + Cost_of_Settlement
                Disc_D_Settle_Bound = DProbV*Damages + Cost_of_Trial - Cost_of_Settlement
                if (Disc_P_Settle_Bound > Disc_D_Settle_Bound) {
                        Disc_Settlement_Amount = NA
                } else {
                        Disc_Settlement_Amount = Disc_P_Settle_Bound + 
                                (Disc_D_Settle_Bound - Disc_P_Settle_Bound) * Bargaining_Power
                        if (Disc_Settlement_Amount < 0) {Disc_Settlement_Amount = 0}
                }
                
                Linear_P_Settle_Bound = Pl_Pred_Mean*Damages  - Cost_of_Trial + Cost_of_Settlement
                Linear_D_Settle_Bound = Df_Pred_Mean*Damages + Cost_of_Trial - Cost_of_Settlement
                if (Linear_P_Settle_Bound > Linear_D_Settle_Bound) {
                        Linear_Settlement_Amount = NA
                } else {
                        Linear_Settlement_Amount = Linear_P_Settle_Bound + 
                                (Linear_D_Settle_Bound - Linear_P_Settle_Bound) * Bargaining_Power
                        if (Linear_Settlement_Amount < 0) {Linear_Settlement_Amount = 0}
                }
                
                Logit_P_Settle_Bound = Pl_Logit_Expectation*Damages  - Cost_of_Trial + Cost_of_Settlement
                Logit_D_Settle_Bound = Df_Logit_Expectation*Damages + Cost_of_Trial - Cost_of_Settlement
                if (Logit_P_Settle_Bound > Logit_D_Settle_Bound) {
                  Logit_Settlement_Amount = NA
                } else {
                  Logit_Settlement_Amount = Logit_P_Settle_Bound + 
                    (Logit_D_Settle_Bound - Logit_P_Settle_Bound) * Bargaining_Power
                  if (Logit_Settlement_Amount < 0) {Logit_Settlement_Amount = 0}
                }
                
                ## Compute Error and Squared Error Terms for Discontinuous Settlements
                
                if (is.na(Disc_Settlement_Amount) == FALSE) {
                  
                  if (Disc_Settlement_Amount <= Damages) {
                    Disc_Error_Harming_Plaintiff = (Damages - Disc_Settlement_Amount) * Unbiased_Pred_Mean
                  } else {Disc_Error_Harming_Plaintiff = 0}
                  
                  Disc_Error_Harming_Plaintif_Squared = Disc_Error_Harming_Plaintiff ^ 2
                  
                  if (Disc_Settlement_Amount <= Damages) {
                    Disc_Error_Harming_Defendant = Disc_Settlement_Amount * (1 - Unbiased_Pred_Mean)
                  } else {Disc_Error_Harming_Defendant =  
                    (Disc_Settlement_Amount - Damages) * Unbiased_Pred_Mean + Disc_Settlement_Amount * (1 - Unbiased_Pred_Mean)}
                  
                  Disc_Error_Harming_Defendant_Squared = Disc_Error_Harming_Defendant ^ 2
                  Expected_Disc_Settlement_Error = Disc_Error_Harming_Plaintiff + Disc_Error_Harming_Defendant
                  Expected_Disc_Sum_of_Squared_Error = Disc_Error_Harming_Plaintif_Squared + Disc_Error_Harming_Defendant_Squared
                  
                  Expected_Disc_Settlement_Error_over_Damages = Expected_Disc_Settlement_Error / Damages
                  Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages = Expected_Disc_Sum_of_Squared_Error / (Damages^2)
                  
                  
                } else {
                  Disc_Error_Harming_Plaintiff = NA
                  Disc_Error_Harming_Plaintif_Squared = NA
                  Disc_Error_Harming_Defendant = NA
                  Disc_Error_Harming_Defendant_Squared = NA
                  Expected_Disc_Settlement_Error = NA
                  Expected_Disc_Sum_of_Squared_Error = NA
                  Expected_Disc_Settlement_Error_over_Damages = NA
                  Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages = NA
                }
                
                ## Compute Squared Error Terms for Linear Settlements
                
                if (is.na(Linear_Settlement_Amount) == FALSE) {
                  
                  if (Linear_Settlement_Amount <= Damages) {
                    Linear_Error_Harming_Plaintiff = (Damages - Linear_Settlement_Amount) * Unbiased_Pred_Mean
                  } else {Linear_Error_Harming_Plaintiff = 0}
                  
                  Linear_Error_Harming_Plaintif_Squared = Linear_Error_Harming_Plaintiff ^ 2
                  
                  if (Linear_Settlement_Amount <= Damages) {
                    Linear_Error_Harming_Defendant = Linear_Settlement_Amount * (1 - Unbiased_Pred_Mean)
                  } else {Linear_Error_Harming_Defendant =  
                    (Linear_Settlement_Amount - Damages) * Unbiased_Pred_Mean + Linear_Settlement_Amount * (1 - Unbiased_Pred_Mean)}
                  
                  Linear_Error_Harming_Defendant_Squared = Linear_Error_Harming_Defendant ^ 2
                  Expected_Linear_Settlement_Error = Linear_Error_Harming_Plaintiff + Linear_Error_Harming_Defendant
                  Expected_Linear_Sum_of_Squared_Error = Linear_Error_Harming_Plaintif_Squared + Linear_Error_Harming_Defendant_Squared
                  
                  Expected_Linear_Settlement_Error_over_Damages = Expected_Linear_Settlement_Error / Damages
                  Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages = Expected_Linear_Sum_of_Squared_Error / (Damages^2)
                  
                  
                } else {
                  Linear_Error_Harming_Plaintiff = NA
                  Linear_Error_Harming_Plaintif_Squared = NA
                  Linear_Error_Harming_Defendant = NA
                  Linear_Error_Harming_Defendant_Squared = NA
                  Expected_Linear_Settlement_Error = NA
                  Expected_Linear_Sum_of_Squared_Error = NA
                  Expected_Linear_Settlement_Error_over_Damages = NA
                  Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages = NA
                }
                
                ## Compute Squared Error Terms for Logistic Settlements
                
                if (is.na(Logit_Settlement_Amount) == FALSE) {
                  
                  if (Logit_Settlement_Amount <= Damages) {
                    Logit_Error_Harming_Plaintiff = (Damages - Logit_Settlement_Amount) * Unbiased_Pred_Mean
                  } else {Logit_Error_Harming_Plaintiff = 0}
                  
                  Logit_Error_Harming_Plaintif_Squared = Logit_Error_Harming_Plaintiff ^ 2
                  
                  if (Logit_Settlement_Amount <= Damages) {
                    Logit_Error_Harming_Defendant = Logit_Settlement_Amount * (1 - Unbiased_Pred_Mean)
                  } else {Logit_Error_Harming_Defendant =  
                    (Logit_Settlement_Amount - Damages) * Unbiased_Pred_Mean + Logit_Settlement_Amount * (1 - Unbiased_Pred_Mean)}
                  
                  Logit_Error_Harming_Defendant_Squared = Logit_Error_Harming_Defendant ^ 2
                  Expected_Logistic_Settlement_Error = Logit_Error_Harming_Plaintiff + Logit_Error_Harming_Defendant
                  Expected_Logistic_Sum_of_Squared_Error = Logit_Error_Harming_Plaintif_Squared + Logit_Error_Harming_Defendant_Squared
                  
                  Expected_Logistic_Settlement_Error_over_Damages = Expected_Logistic_Settlement_Error / Damages
                  Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages = Expected_Logistic_Sum_of_Squared_Error / (Damages^2)
                  
                } else {
                  Logit_Error_Harming_Plaintiff = NA
                  Logit_Error_Harming_Plaintif_Squared = NA
                  Logit_Error_Harming_Defendant = NA
                  Logit_Error_Harming_Defendant_Squared = NA
                  Expected_Logistic_Settlement_Error = NA
                  Expected_Logistic_Sum_of_Squared_Error = NA
                  Expected_Logistic_Settlement_Error_over_Damages = NA
                  Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages = NA
                }
                
                
                new_row_SDF <- data.frame(
                  Unbiased_Pred_Mean = Unbiased_Pred_Mean,
                  Unbiased_Pred_Precision = (Unbiased_Pred_Precision),
                  Unbiased_ProbV = as.numeric(Unbiased_ProbV[1]),
                  Pl_Pred_Mean = Pl_Pred_Mean,
                  PProbV = PProbV,
                  Pl_Logit_Expectation = Pl_Logit_Expectation,
                  Df_Pred_Mean = Df_Pred_Mean,
                  DProbV = DProbV,
                  Df_Logit_Expectation = Df_Logit_Expectation,
                  Pp_minus_Pd = Pp_minus_Pd,
                  Mp_minus_Md = Mp_minus_Md,
                  LOGITp_minus_LOGITd = LOGITp_minus_LOGITd,
                  Damages = as.numeric(Damages),
                  Total_Costs = as.numeric(Total_Costs),
                  Cost_of_Trial = as.numeric(Cost_of_Trial),
                  Cost_of_Settlement = as.numeric(Cost_of_Settlement),
                  Twice_Trial_minus_Settle_over_Damages = Twice_Trial_minus_Settle_over_Damages,
                  Bargaining_Power = Bargaining_Power,
                  Disc_Settle_True_False = Disc_Settle_True_False,
                  Linear_Settle_True_False = Linear_Settle_True_False,
                  Logistic_Settle_True_False =  Logistic_Settle_True_False,
                  Disc_P_Settle_Bound = as.numeric(Disc_P_Settle_Bound),
                  Disc_D_Settle_Bound = as.numeric(Disc_D_Settle_Bound),
                  Linear_P_Settle_Bound = Linear_P_Settle_Bound,
                  Linear_D_Settle_Bound = Linear_D_Settle_Bound,
                  Logit_P_Settle_Bound = Logit_P_Settle_Bound,
                  Logit_D_Settle_Bound = Logit_D_Settle_Bound,
                  Disc_Settlement_Amount = Disc_Settlement_Amount,
                  Linear_Settlement_Amount = Linear_Settlement_Amount,
                  Logit_Settlement_Amount = Logit_Settlement_Amount,
                  Expected_Disc_Settlement_Error = Expected_Disc_Settlement_Error,
                  Expected_Linear_Settlement_Error = Expected_Linear_Settlement_Error,
                  Expected_Logistic_Settlement_Error = Expected_Logistic_Settlement_Error,
                  Expected_Disc_Sum_of_Squared_Error = Expected_Disc_Sum_of_Squared_Error,
                  Expected_Linear_Sum_of_Squared_Error = Expected_Linear_Sum_of_Squared_Error,
                  Expected_Logistic_Sum_of_Squared_Error = Expected_Logistic_Sum_of_Squared_Error,
                  Expected_Disc_Settlement_Error_over_Damages = Expected_Disc_Settlement_Error_over_Damages,
                  Expected_Linear_Settlement_Error_over_Damages = Expected_Linear_Settlement_Error_over_Damages,
                  Expected_Logistic_Settlement_Error_over_Damages = Expected_Logistic_Settlement_Error_over_Damages,
                  Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages = Expected_Disc_Sum_of_Squared_Error_over_Squared_Damages,
                  Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages = Expected_Linear_Sum_of_Squared_Error_over_Squared_Damages,
                  Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages = Expected_Logistic_Sum_of_Squared_Error_over_Squared_Damages
                )
                        
                Settlement_Data_Frame = rbind.data.frame(Settlement_Data_Frame, new_row_SDF)
        }
        
        ## Wrapping up
        close(pb)
        end_time <- Sys.time()
        print(end_time - start_time)
        Settlement_Data_Frame
}          
        
######################################################################


