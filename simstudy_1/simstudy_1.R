# 3.Design a simulation study 1

# 3.1 Fit a regression model to the simulated data while assuming that data are i.i.d and normally distributed

# 3.2 Conduct a simulation study that varies sample sizes, correlation levels, or degree of non-normality to assess bias in parameter estimates, type I error in hypothesis testing and coverage in conference intervals.

# 3.3 Demonstrate how inference changes as the severity of assumption deviation increases (more skew, heavier tail, stronger correlation)


# -------------------------------------------------------------------------------------------------------
# Questions from what other ppl have coded:
  # why are there FOUR variables in the Sigma correlation matrix??
  # why wouldn't it just be two variables that are correlated?? confused about why there are FOUR going here??
  # IT IS MUCH HARDER TO **VISUALIZE** once the number of vars in the multivar dist exceeds 2!! less opportunities for us to plot how we successfully generated correlated data with the copula transformation, etc...
# "# Generate Independent Heavy-Tailed t-Distributions"
  # what is happening here?? I don't understand why this is being done (ex: why + & * ##??)
# generation/inducing correlation with copula acc seems okay... nice -- although it's worth noting that, based on the corr matrix, each of the two var pairs is only correlated w each other--so then why wouldn't we just be using 2 vars to show proof of concept/do the simulation study...
# the plot in "visualization_hist_fit" is just the marginal distributions, no? this doesn't demonstrate that they are correlated... what is the point
# for final chunk: should have calculated for generated_data rather than data_copula_t, no?
  # also: why are the values so consistent for the empirical estimates--shouldn't the ones with true value 0 be hovering around 0, not staying near these values?
# -------------------------------------------------------------------------------------------------------

# 3.1 Fit a regression model to the simulated data 
# while assuming that data are i.i.d and normally distributed

# source in the Rmd to have the generating fxn
rm(list=ls())
library(here)
# need to incorporate "here", but for now it's hard-coded...
# ideally: have a setup file for ppl to set the working directory to the project lol
source(here("dist_and_cor/distributions_and_correlation_analyses.Rmd"))
# oh lol can't source Rmd
# well pretend I did... just run the whole thing (will add this script to that Rmd later)

# now: have access to all the scripts/fxns in that Rmd

# generate 100 realizations of the multivariate distribution (100 samples each)
# should generate 100 rows, two columns -- yes?

# Generate 4 vars -- 100 samples -- from 4 vars with distributions (? are they all t dist originally?-- yes: they are 4 diff Generate Independent Heavy-Tailed t-Distributions, each only correlated with one other (2 pairs of 2))
nn = 100 # number of samples
nun = 5 # df; lower df indicates increased skewness, since it's a t-dist [i.e. further deviation from normality]
test_3_1 <- generate_t_copula_data(n=nn, nu=nun)

# apply linear regression to the simulated multivariate data (assumes iid, normally distributed)
# should be pretty straightforward
# visualize using plots -- 5 examples: x against y, then the linear regression through them

lm(test_3_1$Glucose ~ test_3_1$Cholesterol) # true corr 0.5
lm(test_3_1$Glucose ~ test_3_1$BMI)
lm(test_3_1$Glucose ~ test_3_1$BloodPressure)

lm(test_3_1$Cholesterol ~ test_3_1$BMI)
lm(test_3_1$Cholesterol ~ test_3_1$BloodPressure)

lm(test_3_1$BMI ~ test_3_1$BloodPressure) # true corr 0.4

# note: if we just had TWO variables, all of these pairwise comparisons would be SO MUCH CLEANER LOL
# & it would be easy to plot all of this, even in a loop!!

test <- lm(test_3_1$Glucose ~ test_3_1$Cholesterol)
summary(test) # nice--the R^2 is correct...

plot(test_3_1$Cholesterol, test_3_1$Glucose)
lines(seq(0,260,1), test$coefficients[1] + test$coefficients[2]*seq(0,260,1))

cor(test_3_1) # note: these numbers are all over the place...
cor(generate_t_copula_data(n=100000, nu=4)) # majorlyyyy improves as sample size increases

cor(generate_t_copula_data(n=100000, nu=5)) # not affected by df




# -------------------------------------------------------------------------------------------------------

# 3.2 Conduct a simulation study that varies sample sizes, ---> vary nn in a loop
# correlation levels, 
# or degree of non-normality ---> vary df [for heavy-tailed t, higher df is closer to normality]

# to assess bias in parameter estimates, --- ??? how do I assess bias?? what is the TRUE correlation between them?? is it linear? I HAVE NO IDEA based on the copula transform?! ---> calculate true value... I think it is the same independent of the df? since we forced the correlation matrix/it's fixed? but can check that
  # output a df with a) the true correlation & b) the estimated correlation
  # should it also have a) the true mean of the dist & b) the estimated mean of the dist?
  # is there a true values for beta & the intercept?? how do I find that for a multivariate dist....
# type I error in hypothesis testing --- ??? how is this different from coverage in confidence intervals? my impression is that this would just testing whether there IS a correlation, but then that should be the same @ hyp testing--whether there is a corr should be the same as whether it is contianed in the CI, no?
  # (note: doesn't this...depend on the CI lol. p-value & CI are equivalent, since both depend on specified distribution & true SE)
# and coverage in conference intervals. 
  # --> what percent of CIs contain the true, theoretical values


# -------------------------------------------------------------------------------------------------------

# 3.3 Demonstrate how inference changes as the severity of assumption deviation increases 
# (more skew, heavier tail, stronger correlation) 
  # -- what? how is this different from the second part--i think it's just adding tail & skew to the "more correlation" part in 3.2...

# -------------------------------------------------------------------------------------------------------







# -------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------

# keeping some other code that I put in "distributionsand_correlation_analysis.Rmd" but is messy::











# Linear regression
lm_out_3_1 <- list()
for (ii in colnames(generated_data_3_1)){
  for (jj in colnames(generated_data_3_1)){
    # same variable
    if (ii == jj){ # pass
    }
    # 6 unique pairwise regressions for four variables (3 + 2 + 1)
    else{
      linreg <- lm(generated_data_3_1[[ii]] ~ generated_data_3_1[[jj]]) # ii = y ~ jj = x
      # store output, named
      lm_out_3_1[[paste0(ii, "_", jj)]] <- linreg  # stored as y_x
    }
  }
}

# function for storing all of these pairwise regressions -- for each multivar generated dataset
pairwise_linreg_store <- function(gen_data){
  lm_out <- list()
  for (ii in colnames(gen_data)){
    for (jj in colnames(gen_data)){
      # same variable
      if (ii == jj){ # pass
      }
      # 6 unique pairwise regressions for four variables (3 + 2 + 1)
      else{
        linreg <- lm(gen_data[[ii]] ~ gen_data[[jj]]) # ii = y ~ jj = x
        # store output, named
        lm_out[[paste0(ii, "_", jj)]] <- linreg  # stored as y_x
      }
    }
  }
  return(lm_out)
}











# loop through sample sizes & extract all pairwise linear regression estimates for each -- DEPRECATED
# for (ii in sampsize_3_2){
#   generated_data_loop <- generate_t_copula_data(n = ii, nu = df_3_2)
#   lm_varysampsize_store[[paste0("sampsize", ii)]] <- pairwise_linreg_store(generated_data_loop)
# }

# test that these nested loops work: 
# lm_varysampsize_store$sampsize10$Glucose_Cholesterol$coefficients
# nice

# for this scenario (df fixed), there is a single true relationship for each of these pairwise correlations
Target_Correlation = c(0.5, 0, 0, 0, 0, 0.4)  # From defined Sigma -- Glucose & Cholesterol, BMI & Blood Pressure
# convert these to expected beta values:
Target_linregcoeff_NOTDONE = c(0.5, 0, 0, 0, 0, 0.4)  # r / (S_x/S_y) <<- those should have theoretical values for each dist*** -- & NOTE: these are not transitive, direction of magnitude changes based on which is x & which is y even though correlation does not












# plot how they vary with sample size -- include a) estimated beta and b) CI
# there will be 6 plots here (each pairwise correlation [combinations] although NOTE -- 12 permutations since they are not the same when flipped

# test: plot ONE of these
# test <- as.data.frame(t(lm_varysampsize_store$sampsize10$Glucose_Cholesterol)) # note: the beta columna var is sort of wigging & I'm not quite sure why
# colnames(test)[3] <- "beta"
# 
# # plot works... now do this for all 6 (!) unique pairwise regressions and see how they look (note: not doing the double for reciprocals)
# ggplot(data=test, aes(x = samp_size, y = beta)) +
# 
#   geom_point() +
# 
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.9))


