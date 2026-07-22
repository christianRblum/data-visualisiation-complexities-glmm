# Summary: this is an example script for statistical analysis using generalized linear mixed models (GLMM)
# within the null hypothesis significance testing (NHST) framework. it includes post hoc and stability testing.
# this script is a simplified example of one possible approach.
# it serves as starting point, not as replacement for a statistics course.
# some steps in this script might not apply to your analysis, e.g. diagnostics for different distributions
# make sure you understand what the code does if you decide to run (parts of) it for your analysis.
# 
# Author: Christian Blum
# last updated: 2026-04-24



# set up workspace ####

# load packages
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)
library(ggplot2)

# get example data
data <- as.data.frame(glmmTMB::Salamanders)

# explore data
names(data) # get column headers
nrow(data) # get number rows
ncol(data) # get number columns
str(data) # get structure of data (data type per column)
dplyr::glimpse(data) # alternative to str(), requires the package "dplyr"
head(data, 20) # show first 20 rows 
#View(data) # show all of data in a nother tab


# it seems data consists of the following:
# Column | Description
# site   | Sampling site identifier (factor); used as a grouping variable for random effects.
# mined  | Habitat disturbance status (factor), typically with levels "no" (unmined) and "yes" (mined).
# cover  | Habitat cover at the sampling location (numeric; e.g., percent/amount of suitable substrate or cover).
# sample | Sampling replicate/occasion within site (identifier; distinguishes repeated samples).
# DOP    | Dissolved oxygen (percent saturation) at sampling (numeric).
# Wtemp  | Water temperature in degrees Celsius at sampling (numeric).
# DOY    | Day of year of the sampling event (numeric; 1–366).
# spp    | Salamander species (factor; species codes).
# count  | Number of salamanders observed for the given species at that site/sample (integer response).


# sometimes it is required to clean, filter, transform data etc. here we use it as is.
# refer to data exploration script for more info on this




# data preparation ####

# we choose the following for our analysis:

# response: count
# fixed effects: mined, spp, cover, DOP, Wtemp, DOY
# random effects: site, sample

# we expect different species to be affected differently by mining, so we include an interaction here:
# mined * spp (this is identical to mined + spp + mined:spp)

# sample is nested in site, so we include it as such in the random effects:
# (site:sample)

# time may have a linear effect, seasonal effect etc.
# linear: DOY
# seasonal: different implementations are possible. e.g. convert to radian and take sine and cosine
# here we use a quadratic term: poly(DOY, 2)

# before we assemble the model, we want to do a few more things:
# check if we can include fixed effects as random slopes in random effects
# check if our data is complete (remove incomplete cases)
# dummy code and center categorical predictors (factors)
# z-transform continuous predictors (covariates)
# these dummy coded and z-transformed predictors can be then included as random slopes. this helps convergence of the model
# there is a custom function created by Roger Mundry that helps us with this:
# https://doi.org/10.5281/zenodo.7670523
# i have not included it in this repository on purpose, because i don't want to use outdated versions.

# there are two ways to get this custom function into your R instance:

# option 1 (possibly safer):
  # download it manually, then source it using
  # source("diagnostic_fcns.r")

# option 2:
  # download it using R code. this is simpler, but it downloads whatever is located under the following link,
  # whether it is the custom function or something else. in this instance it should be fine, but use at your own risk.
  
  # load custom function for random slopes:
  tmp <- tempfile(fileext = ".R")
  download.file(
    "https://zenodo.org/records/7670524/files/diagnostic_fcns.r?download=1",
    destfile = tmp, mode = "wb", quiet = TRUE
  )
  source(tmp) 

# now that we sourced the custom function, we can use it
# specify our model in the custom function and run it on our data
xx.fe.re=fe.re.tab(fe.model="count ~
                      mined * spp + cover + DOP + Wtemp + DOY",
                   re="(1|site)+(1|sample)",
                   data=data)

# we get an error, because one of our predictors is not correctly specified.
# Inspect which columns have multi-class (length > 1)
sapply(data, class)
# here we see that "site" has two classes "ordered" and "factor"
# we remove ordered from it
data$site <- factor(data$site, ordered = FALSE)
# and rerun the custom function
xx.fe.re=fe.re.tab(fe.model="count ~
                      mined * spp + cover + DOP + Wtemp + DOY",
                   re="(1|site)+(1|sample)",
                   data=data)

# it worked! we investigate the output
nrow(data)==nrow(xx.fe.re$data) # TRUE: no cases were dropped. If FALSE: cases were dropped.
nrow(data)-nrow(xx.fe.re$data) # this tells you how many cases were dropped
nrow(data) # out of this many
# you then have to figure out why. it is caused by missing entries for one or more predictors (columns)
# is the inclusion of this predictor worth the loss in data?
# let us save this data for our use in the model
model_data = xx.fe.re$data


# show variation of levels of fixed effects within random effects to identify possible random slopes
xx.fe.re$summary 

# $`mined_within_site (factor)`
# 1 tot 
# 23  23 

# out of a total 23 sites, 23 have only one level of "mined". meaning every site is either yes or no exclusively.
# random slope is not identifiable due to limited variation.
# sufficient variation for factors would be at least 50% of total cases having at least 2 levels.
# sufficient variation for covariates would be at least 50% of total cases having at least 3 levels.
table(data$site, data$mined) # this is a different way of looking at it, but more complex


# $`mined_within_sample (factor)`
# 2 tot 
# 4   4 

# out of 4 total samples, 4 have 2 levels of mined. 
# this is an issue, because "sample" is nested in "site", but this nested structure is not compatible with the custom function
# for this purpose, it is recommended to manually nest the random effects beforehand
# in this case we continue to see what happens
# based on this output, we would have sufficient vairation to include a random slope of mined in sample
# in reality, we do not, because of the nested structure
table(data$sample, data$mined)

# $`spp_within_site (factor)`
# 7 tot 
# 23  23 

# 100% of cases have 7 unique values. this means all sites have 7 species present

table(data$site, data$spp)
# we can include this random slope

# for interactions, such as "mined:spp" it only shows larger than one >1 or not larger than one !>1, but same rule applies

# following this logic, we work our way through the output and include random slopes wherever the variation in the data allows.
# these are our theoretically identifiable random slopes. including them all leads to the maximal model

# this leads to the following maximal model

# maximal.model <- glmmTMB(
#   count ~ mined * spp + cover + DOP + Wtemp + poly(DOY, 2) +
#     (1 + spp + DOP + Wtemp + DOY |site) + 
#     (1 + mined + spp + cover + DOP + Wtemp + DOY + mined:spp |site:sample),
#   data = model_data
# )

# HOWEVER, we remember that sample is nested in site:

table(data$site, data$sample) # sample 1 for site R-1 is different from sample 1 for R-2
# due to the nested structure, random slopes that are not identifiable for site are (most likely) also 
# not identifiable for site:sample

# updated formula:

# maximal.model <- glmmTMB(
#   count ~ mined * spp + cover + DOP + Wtemp + poly(DOY, 2) +
#     (1 + spp + DOP + Wtemp + DOY  |site) + 
#     (1 + spp + DOP + Wtemp + DOY  |site:sample),
#   data = model_data
# )

# this looks better, but we still have to dummy code and center factors, z-transform covariates for use in random slopes
# fortunately, the function already took care of the dummy coding for us!

# factors (center):
# we center by subtracting the mean
model_data$spp.DES.L = model_data$spp.DES.L - mean(model_data$spp.DES.L)


# covariates (z-transform):
model_data$z.DOP = as.vector(scale(model_data$DOP))
mean(model_data$z.DOP) # mean of approx 0
sd(model_data$z.DOP) # SD of 1

model_data$z.Wtemp = as.vector(scale(model_data$Wtemp))
model_data$z.DOY = as.vector(scale(model_data$DOY))


# we update the maximal model with dummy coded and centered factor random slopes, and z-transformed covariate random slopes

# maximal.model <- glmmTMB(
#   count ~ mined * spp + cover + DOP + Wtemp + poly(DOY, 2) +
#     (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  |site) + 
#     (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  |site:sample),
#   data = model_data
# )

# now we have to add a model family
# this is based on the response
# i heavily suggest you do your own research on this, as this is just a script to showcase practical application of 
# GLMMs in the NHST framework, not a statistics class, but here an incomplete and superficial starting point:
# 
# covariate -> continuous -> unbound (-inf, +inf) : gaussian
# covariate -> continuous -> lower bound (>0, +inf) -> not latency: gaussian / gamma
# covariate -> continuous -> lower bound (0, +inf) -> not latency: tweedie
# covariate -> continuous -> lower bound (0, +inf) -> latency (waiting time): gamma
# covariate -> discrete -> upper and lower bound (0,1; probabilities of failure / success): binomial
# covariate -> discrete -> lower bound (0, +inf) -> independent counts: poisson
# covariate -> discrete -> lower bound (0, +inf) -> clustered counts: negative binomial
# covariate -> proportions (upper and lower bound; 0, 1) -> continuous: beta
# factor -> ordered levels (distance between consecutive levels is unequal): cumulative logistic
# factor -> unordered levels: multinomial (bayesian approach)


# model formulation ####

# we choose negative binomial, now our model is complete and we can run it
maximal.model <- glmmTMB(
  count ~ mined * spp + cover + DOP + Wtemp + poly(DOY, 2) +
    (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  |site) + 
    (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  |site:sample),
  family = nbinom2,
  data = model_data
)

# model does not converge. it is simply too complex for our limited sample size.
# we reduce complexity by changing | to || in the random effects structure to remove correlations of 
# random slopes and random intercepts.
# after this step, it is no longer the maximal model. we are now in search of a simpler version of the maximal model,
# that converges and still represents our working hypothesis. a full model:


full.model <- glmmTMB(
  count ~ mined * spp + cover + DOP + Wtemp + poly(DOY, 2) +
    (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  ||site) + 
    (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  ||site:sample),
  family = nbinom2,
  data = model_data
)

# this model converges. 
# if not, we would have to simplify it further by removing some terms, e.g. random slopes.
# decide what terms to remove first using your biological and experimental understand of the data.
# the least relevant terms get removed first




# model diagnostics ####

# we now need to run diagnostics, to see if this model is acceptable

# colinearity test via variance inflation factors
# we use a linear model with only fixed main effects (no interactions, no random effects)
xx=lm(count ~ mined + spp + cover + DOP + Wtemp + poly(DOY, 2), data=model_data) 
vif(xx)[, 3]^2 # good. anything above 2 or 3 could indicate problems

# check residuals
hist(resid(full.model)) 

# best linear unbiased predictors (BLUPs): deviations from common intercept and slopes
ranef.diagn.plot(full.model) # good! we want approximately normal distributions, absolute x-axis ranges of less than 3

# basic dharma diagnostics plot
diagnostics_full <- simulateResiduals(fittedModel = full.model, plot = F)
plot(diagnostics_full) # looks good. anything significant or red would indicate a problem, meaning we would have to 
# "fix" the model before we continue

# we can also run specific tests using dharma:

    # dispersion tests
        # Dharma model diagnostics  
        testDispersion(full.model) # standard approach
        
        # use pearson to check for overdispersion instead
        testDispersion(full.model, type = "PearsonChisq", alternative = "greater") # good.
        # dispersion parameter of 1 is ideal. 
        # above 1 is overdispersion (increased false positive error), meaning we cannot trust significant findings 
        # (very problematic)
        # below 1 is underdispersion (increased false negative error), meaning we might miss a significant finding, but 
        # can trust those
        # we actually get (so less problematic, but still not good)
        # generally, a range of 0.8 to 1.2 is fine
        
        # check for overdispersion using Roger Mundry's custom function
        overdisp.test(full.model)
    
    # zero inflation test
    testZeroInflation(diagnostics_full) # looks good


# this is a quick run down of model diagnostics. there are other tests you can run. 
# not all tests are appropriate for all model families.

# now that we have a working full model, we formulate the null model
# this null model represents our null hypothesis (the test predictors do not matter)





# null model ####


# let's remind ourselves of our full model:

# full.model <- glmmTMB(
#   count ~ mined * spp + cover + DOP + Wtemp + poly(DOY, 2) +
#     (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  ||site) + 
#     (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  ||site:sample),
#   family = nbinom2,
#   data = model_data
# )


# in this case, our test predictors are the interaction of mined * spp and their main effects, so we remove those
# we also consider cover a test predictor, so we remove that as well
# the rest we consider control predictors, so they stay

null.model <- glmmTMB(
  count ~  DOP + Wtemp + poly(DOY, 2) +
    (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  ||site) + 
    (1 + spp.DES.L + z.DOP + z.Wtemp + z.DOY  ||site:sample),
  family = nbinom2,
  data = model_data
)

# this model also converges. sometimes the full model converges, but the null model doesn't.
# in that case you have to simplify the null model until it converges, then adjust the full model to have the same formula, 
# besides the inclusion of the test predictors of course.
# in the end, you want the presence / absence of test predictors to be the only difference between full and null model.





# model comparison ####

# now we can run the model comparison between full and null model.
# this is the main point of the NHST approach. we run one test to check if the inclusion of the sum of test predictors
# has a significant effect on the response.
# this avoids cryptic multiple testing.

# model comparison
model.comparison=as.data.frame(anova(full.model, null.model, test="Chisq")) 
model.comparison 
# we get a significant model comparison (p<0.05)
# and our full model has a better fit (lower AIC than null model)
# this means the full model representing our working hypothesis is significantly better than the 
# null model representing the null hypothesis. we can therefore continue with the analysis
# if this test would have been non-significant, we would have to stop out analysis here.
# though, some people prefer to continue if the comparison shows a trend and interpret findings with care.

# time to get some p-values for our predictors. we do that by dropping one predictor at a time from the model and comparing
# against the full model using drop1()





# model results ####

# get p value for entire predictors, not just between levels per predictor
p_values <- as.data.frame(drop1(full.model, test = "Chisq"))
round(p_values, 3)
# all sub models converged. sometimes some of them do not converge, then you have to simplify the full model until they do.
# we get a significant effect for the interaction term mined:spp
# we get no siginificant effect for cover
# sometimes the model comparison (full vs null) is significant, but the test-predictor-interaction is not significant in the 
# drop1 results. depending on the focus of your study, you could consider the interaction only (a:b) as test predictor, but
# not their main effects (a+b). in that case, the full model is a*b and the null model a+b. if drop1 is not significant for 
# a:b you end the analysis. If it is, but drop1 is not significant for a:b, you end the analysis.
# however, in many cases you consider the interaction (a:b) AND their main effects (a+b) as test predictors. this is what we
# did here. if in such cases the model comparison is significant, but the interacton (a:b) in the drop1 is not, you can 
# formulate a reduced full model lacking the interaction term and containing the main effects only (a+b) to then investigate
# those in absence of an interaction. main effects results in presence of an interaction have different, more limited 
# interpretation, hence the need for a reduced full model.
# here drop1 showed a significant effect for a:b, or in our case mined:spp, so we continue with the analysis.

# the significant drop1 effect for an interaction term might already be your result. if your interaction consists of 
# categorical predictors (factors) only, you could follow up with a post hoc test. if it includes at least one covariate, i 
# suggest simply plotting the effect instead.

# let us look at the model summary
summary(full.model)
# the conditional model output shows us estimates for the levels of the predictors.
# here we see the reference level of spp (so the alphabetically first species) compared to the six other species.
# the p-values are always reference vs another level. this is different from the drop1 p-value, which is for the entire 
# predictor species. it could be possible that we find no difference between A and B, or A and C. then summary would show
# non-significant p-values. But B and C might be significantly different. we would then miss that, unless we specifically
# run this comparison. drop1 looks at "model with species" vs "model without species". that is a more reliable way to get 
# the relevance of species overall.
# here of course the interpretation of species alone is limited, because it is also included in an interaction.

# what we can see in this summary, is that 
# cover is non significat (as drop1 showed)
# DOP is non non significant (only a control predictor)
# Wtemp is non significant (also a control predictor)
# time (as poly(DOY,2) included in the model) is non significant
# some of the interaction groups show significance, but this will get clearer in the post hoc test.

confint(full.model) # this is how you can get quick, but less precice confitdence intervals.
# for better results, i recommend a boot-strapping solution, e.g. via above zenodo link





# post hoc test ####
post_hoc=emmeans(full.model, specs=pairwise~mined:spp)
# here we get the warning message:
#
# Warning message:
#   You may have generated more contrasts than you really wanted. In the future,
# we suggest you avoid things like 'pairwise ~ fac1*fac2' when you have
# more than one factor. Instead, call emmeans() with just '~ fac1*fac2' and do the
# contrasts you need in a later step.
#
# we'll get back to that

# check the results of the post hoc
post_hoc$contrasts 
# we see there are many
# let us plot them
plot(post_hoc, CIs = F, comparisons = TRUE)
# overlapping arrows means NO significant difference between them

# getting this many comparisons is problematic for two reasons:
# it is difficult to interpret the outcome
# because of the large number of comparisons, multiple testing adjustment will be harsh

# for those reasons, it is a good idea to limit the levels within a predictor of an interaction term.
# maybe grouping some species could have worked?

# let us assume that we are only interested in two species. we do this only to showcase how we could limit the number 
# of comparisons on a technical level.


# here we follow the steps recommended in above warning message.
# first we define the emmeans grid without the pairwise argument
emci_grid = emmeans(full.model, ~ mined * spp)
# this looks like that:
emci_grid
# this returns all the possible levels in order 
# first order is mined yes : spp GP, so the mined GP group
# second order is non mined GP 
# third is mined PR etc.

# next we specifiy the comparison we are actually interested in:
contrast_selection = list(
  
  # include mined GP and non mined GP, by setting first order 1 and second order -1. all others to 0 
  "mined GP - non mined GP" = c(1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
  
  # include mined PR and non mined PR
  "mined PR - non mined PR" = c(0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  
  # include mined DF and non mined F
  "mined DM - non mined DM" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1)
)

# run comparisons (contrasts)
post_hoc_targeted = contrast(emci_grid, contrast_selection, adjust = "holm") 
# tukey method for entire set (all comparisons)
# holm method when selecting only some of the comparisons

# look at the more targeted comparison results
post_hoc_targeted # we now see that they all differ significantly.

# if you look back up at the results of all possible comparisons, some of them were non significant, becasue of the 
# multiple testing corrections for all of those tests (many of which we didn't even need)

# yes PR - no PR had an adjusted p-value of 0.2886, here it has 0.0073

confint(post_hoc_targeted)# again we get the CIs



# this concludes the statistical analysis. but it is always a good idea to plot the results

# this package can help create some simple, basic plots of the main findings
# note that it does not show what is or is not significant
# it also does not always show the nicest plots
library(effects)
plot(allEffects(full.model))


# we can create nicer plots ourself.


# quick emmeans plot of interaction with CIs on the response scale
emmip(full.model, ~ mined | spp, type = "response", CIs = TRUE)

# a nicer plot using ggplot2 
emm <- emmeans(full.model, ~ mined * spp, type = "response")  # this time on response scale, averages over covariates
df  <- as.data.frame(emm)  # columns: mined, spp, response, SE, lower.CL, upper.CL

ggplot(df, aes(x = mined, y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~ spp, nrow = 2) +
  labs(x = "Mined status", y = "Predicted count (response scale)",
       title = "Mining effects per species") +
  theme_classic()

# alternative plot layout
ggplot(df, aes(x = mined, y = response, group = spp, color = spp)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  labs(x = "Mined status", y = "Predicted count (response scale)",
       title = "Mining effects per species",
       color = "species") +
  theme_classic()


# Take home message: 
# We see that all species have higher counts when not mined, but the strenght of this effect varies across species.




# model stability ####
# it is recommended to also test model stability, though many don't do it
# you get model stability by dropping one level from your random effects grouping terms at a time and running the 
# full model on this reduced dataset
# in this case, it we drop one site at a time.
# in another projects, you might have subjectID as random effect and remove all data from one subject at a time
# for every level you remove, you get a new estimate.
# all of these estimates give you a range (per fixed effects predictor)
# the larger the range, the less stability you have
# if the range goes across 0, directionality of the effect is not very clear either
# this is additional information on your model and the underlying data

# see https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf

# source the stability function out of the glmmTMB package
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))


# rerun the model, leaving out one site at a time (this takes a while)
model_stability = influence_mixed(full.model, groups="site")

# display result
car::infIndexPlot(model_stability)

# display another way (i prefer this one):
inf <- as.data.frame(model_stability[["fixed.effects[-site]"]])
inf <- transform(inf,
                 nest=rownames(inf),
                 cooks=cooks.distance(model_stability))
inf$ord <- rank(inf$cooks)
if (require(reshape2)) {
  inf_long <- melt(inf, id.vars=c("ord","nest"))
  gg_infl <- (ggplot(inf_long,aes(ord,value))
              + geom_point()
              + facet_wrap(~variable, scale="free_y")
              ## n.b. may need expand_scale() in older ggplot versions ?
              + scale_x_reverse(expand=expansion(mult=0.15))
              + scale_y_continuous(expand=expansion(mult=0.15))
              + geom_text(data=subset(inf_long,ord>24),
                          aes(label=nest),vjust=-1.05)
  )
  print(gg_infl)
}

# we now see how removing every station at a time changes the estimate
# in most cases, the estimate seems to not change a lot, indicating high stability
# in some cases, only one stations seems to be very different from the rest
# in the last panel we see the cooks distance, this gives the leverage for each station (how different it is from the others)


# display table of cook's distances, sorted descending
inf_table = inf %>%
  select(site = nest, cooks) %>%
  arrange(desc(cooks))

inf_table 
# in this case site "VF-2" seems to be very different from the others
# there are different cutoff points to determine when a cooks distance is "large", but take those with a grain of salt
# inform yourself on how these relate to your data specifically


# we can plot three common thresholds
# define cutoff threshold (e.g., 4 / number of levels, or a constant of 1)
cutoff_1 = 4 / length(unique(inf_table$site))
cutoff_2 = 1

# cutoff_3 from f distribution 
# Define parameters for your model
p <- length(coef(full.model))  # Number of predictors (including the intercept)
n <- nrow(model.frame(full.model))  # Number of observations in the model
alpha <- 0.05  # Significance level

# Calculate degrees of freedom
df1 <- p  # Numerator degrees of freedom (number of predictors)
df2 <- n - p  # Denominator degrees of freedom (remaining observations)

# Calculate the critical F value
cutoff_3 <- qf(1 - alpha, df1, df2)

# Display the results
cutoff_1
cutoff_2
cutoff_3

# sort sites by cook’s distance for better readability
inf_table_sorted = inf_table[order(inf_table$cooks, decreasing = TRUE), ]
inf_table_sorted$site = factor(inf_table_sorted$site, levels = inf_table_sorted$site)

# plot
ggplot(inf_table_sorted, aes(x = site, y = cooks)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = cutoff_1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = cutoff_2, linetype = "dotted", color = "blue", linewidth = 1) +
  geom_hline(yintercept = cutoff_3, linetype = "dotdash", color = "green", linewidth = 1) +
  geom_text(aes(label = round(cooks, 2)), vjust = -0.5, size = 3) +
  labs(title = "Cook's Distances by Site",
       subtitle = paste("Red dashed line = 4/n (", round(cutoff_1, 4), "),",
                        "Blue dotted line = 1,",
                        "Green dot-dash line = F-distribution cutoff (", round(cutoff_3, 4), ")"),
       x = "Site", y = "Cook's Distance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# conclusion on the stability:
# stability overall is good, with the exclusion of station VF-2

# at this point you can just report the stability as yet another result.
# if you have very good reason for it, you can also remove an extreme level of your grouping factor (in this case site)
# from the data and re-run your model on the reduced data, presenting both results.
# however, you need to have a good biological, experimental design reason etc.
# e.g. VF-2 was the only site inside a city, while all other sites were on the countryside (or vice versa)
# although in this case the decision to remove VF-2 should have probably occurred before starting the analysis
# but there might be other reasons that are less obvious and only pup up after you get a hint.

# this is the end of the script. 
# the purpose of this script was to show the general statistical approach, it is of course not complete and only a very short
# introduction. be careful when generalising from this script to other analyses, as every dataset and every hypothesis has to 
# be evaluated by themselves and with care. every analysis is different.
# some of the information here might be outdated or wrong, as always, do your own research.










