# Name: data exploration and analysis at different levels of complexity
# Summary: this script uses plots and tables to explore data, check for outliers, data presence in interaction groups, 
# relationships between co-variates and shows some best practices to do before you start your statistical analysis.


library(dplyr)
library(ggplot2)
library(janitor)
library(skimr)
library(GGally)
library(performance)
library(car)
library(DHARMa)
library(lme4)
library(nlme)
library(patchwork)
library(tidyr)
library(broom)


# simulate some data ####

# we create a dataset of students' stress level across time, while listening to christian talk about stats.
# this is stressfull. and confusing. let's simulate that into the data.

# set random seed for reproducibility
set.seed(123)

# parameters for simulation ####
num_participants = 8       # number of participants
time_points = 120           # number of time points (2 hours, every minute)
stress_start_mean = 100      # baseline stress level at start (arbitrary value)
stress_start_sd = 5          # standard deviation for initial stress level

# simulate participant-level data with categories ####
participants = data.frame(
  participant_id = 1:num_participants,
  # create experienced and inexperienced students
  experience = sample(c("experienced", "inexperienced"), num_participants, replace = TRUE),
  # create students at the levels of bachelor, master an phd
  level = sample(c("PhD", "master", "bachelor"), num_participants, replace = TRUE),
  # some drink coffee, some tea, some water
  beverage = sample(c("coffee", "tea", "water"), num_participants, replace = TRUE),
  
  # randomly assign tiredness start level and slope
  tiredness_start = runif(num_participants, min = 0, max = 100),
  tiredness_slope = runif(num_participants, min = -0.5, max = 0.5) # slope for tiredness change per minute
)

# adjust starting stress levels based on participant level 
participants = participants %>%
  mutate(
    # adjust baseline based on level of study
    stress_baseline2 = case_when(
      level == "PhD" ~ stress_start_mean * 0.6,      # PhD starts at 60% of master
      level == "bachelor" ~ stress_start_mean * 1.02, # bachelor starts 2% higher than master
      TRUE ~ stress_start_mean                       # master as reference
    ),
    
    # further adjust based on beverage choice
    stress_baseline = case_when(
      beverage == "coffee" ~ stress_baseline2 * 1.05, # coffee increases stress by 5%
      beverage == "water" ~ stress_baseline2 * 0.8,  # water increases stress by *0.8 (decrease of 20%!)
      TRUE ~ stress_baseline2                         # tea as reference
    )
  ) 

# simulate stress levels and tiredness over time for each participant 
data = data.frame()
for (i in 1:num_participants) {
  # get participant-specific parameters
  start_stress = participants$stress_baseline[i]
  start_tiredness = participants$tiredness_start[i]
  tiredness_slope = participants$tiredness_slope[i]
  exp_status = participants$experience[i]
  
  # simulate stress and tiredness trajectory over time
  for (t in 1:time_points) {
    # calculate stress adjustment based on experience
    if (exp_status == "experienced") {
      stress = start_stress - (0.31 * t)  # decrease by 0.1 unit per minute
    } else {
      stress = start_stress + (0.37 * t)  # increase by 0.2 units per minute
    }
    
    # calculate tiredness based on starting value, slope, and time, keeping it within bounds [0, 100]
    tiredness = start_tiredness + tiredness_slope * t
    tiredness = max(0, min(100, tiredness))  # bound tiredness between 0 and 100
    
    # save the simulated data for this time point
    data = rbind(data, data.frame(
      participant_id = participants$participant_id[i],
      time = t,
      stress = stress,
      tiredness = tiredness,
      experience = exp_status,
      level = participants$level[i],
      beverage = participants$beverage[i]
    ))
  }
}

# add some noise to the stress measurement 
# set parameters for the noise
mean_noise = 0       # mean of the noise
sd_noise = 5         # standard deviation of the noise

# add noise to the stress column
data$stress = data$stress + rnorm(nrow(data), mean = mean_noise, sd = sd_noise)


# low complexity approach ####

# let's start with a simple non parametric test (Spearman spearman's rank correlation test)
cor_test = cor.test(data$stress, data$time, method = "spearman")
cor_test # not significant

# it found nothing, so we go home. right?
# WRONG! it found nothing, but we know theres something! after all, we simulated effects into the data ourselves


# let's fit a linear model
model = lm(stress ~ time, data = data)
summary(model)

# again nothing? how come?
# let's plot what the model did: stress across time
ggplot(data, aes(x = time, y = stress)) +
  geom_point(alpha = 0.3, color = "blue") +   # scatter plot of stress over time
  geom_smooth(method = "lm", color = "red") +  # regression line
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme

# this shows us basically a horizontal line. this is the estimate. it estimates no effect of time on stress.
# this is because we have neglected to respect other effects in our analysis


# moderate complexity approach ####

# fit a linear model 2, but include experience this time!
model2 = lm(stress ~ time * experience, data = data)
summary(model2) # significant result!

# heureka! it lives!
# let us plot what this new and improved model did: the effect of experience AND time on stress.
# plot stress over time, separated by experience (color-coded)
ggplot(data, aes(x = time, y = stress, color = experience)) +
  geom_point(alpha = 0.3) +                    # scatter plot with color by experience
  geom_smooth(method = "lm", aes(color = experience)) +  # regression line per experience level
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme

# we see that by including experience as predictor, we now see two opposing effects that cancelled each other out before
# we reduced noise in the data, by accounting for additional effects not present in the first model (and the non parametric test)




# but we have other effects, let's plot those too:

# plot stress over time, separated by level (color-coded)
ggplot(data, aes(x = time, y = stress, color = level)) +
  geom_point(alpha = 0.3) +                    
  geom_smooth(method = "lm", aes(color = level)) +  
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme


# plot stress over time, separated by beverage (color-coded)
ggplot(data, aes(x = time, y = stress, color = beverage)) +
  geom_point(alpha = 0.3) +                    
  geom_smooth(method = "lm", aes(color = beverage)) +  
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme


# plot stress over time, separated by tiredness (color-coded)
ggplot(data, aes(x = time, y = stress, color = tiredness)) +
  geom_point(alpha = 0.3) +                    
  geom_smooth(method = "lm", aes(color = tiredness)) +  
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme

# hang on, this looks weird. why is that?
# tiredness is a covariate, not a factor (category) so plotting it like this makes little sense...
# let's try some alternative plot ideas
    # bin tiredness into groups and use as category
      data$tiredness_bin <- cut(data$tiredness, breaks = 3, labels = c("low", "medium", "high"))
      
      ggplot(data, aes(x = time, y = stress, color = tiredness_bin)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm") +
        labs(title = "Stress over time by tiredness level",
             x = "Time (minutes)", y = "Stress Level", color = "Tiredness") +
        theme_minimal()  

    # scatterplot with 
      ggplot(data, aes(x = time, y = stress, color = tiredness)) +
        geom_point(alpha = 0.5) +
        scale_color_viridis_c() +
        labs(title = "Stress over time, colored by tiredness",
             x = "Time (minutes)", y = "Stress Level", color = "Tiredness") +
        theme_minimal()
  
    # hex plot with binned tiredness
      ggplot(data, aes(x = time, y = stress)) +
        geom_hex(bins = 30) +
        scale_fill_viridis_c() +
        labs(title = "Stress over time", x = "Time (minutes)", y = "Stress Level") +
        theme_minimal()
      
      
      # contour/density overlay: shows where tiredness concentrates across time and space (the final frontier...)
      ggplot(data, aes(x = time, y = stress)) +
        geom_point(aes(color = tiredness), alpha = 0.3) +
        scale_color_viridis_c() +
        geom_density_2d(color = "white", alpha = 0.5) +
        labs(title = "Stress over time with density contours",
             x = "Time (minutes)", y = "Stress Level", color = "Tiredness") +
        theme_dark()
      
      
      # facet by participant: shows individual trajectories and tiredness patterns
      ggplot(data, aes(x = time, y = stress, color = tiredness)) +
        geom_point(alpha = 0.5, size = 0.8) +
        scale_color_viridis_c() +
        facet_wrap(~ participant_id, nrow = 2) +
        labs(title = "Individual stress trajectories colored by tiredness",
             x = "Time (minutes)", y = "Stress Level", color = "Tiredness") +
        theme_minimal()
      
      # i think we are getting carried away. let's refocus


      
      
# we can also plot additional effects into the same plot:
      
# plot stress over time, separated by experience (color-coded)
ggplot(data, aes(x = time, y = stress, color = experience, shape=level)) +
  geom_point(alpha = 0.3) +                    # scatter plot with color by experience
  geom_smooth(method = "lm", aes(color = experience)) +  # regression line per experience level
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme


# more effects!

# stress over time, separated by experience (color-coded)
ggplot(data, aes(x = time, y = stress, color = experience, shape=level, fill=beverage)) +
  geom_point(alpha = 0.3) +                    # scatter plot with color by experience
  geom_smooth(method = "lm", aes(color = experience)) +  # regression line per experience level
  labs(
    title = "Stress when listening to a stats lecture",
    x = "Time (minutes)",
    y = "Stress Level"
  ) +
  theme_minimal()  # cleaner theme
# ok, this is now a very clear an easily understood plot. it clearly shows that... 
# ehm... 
# fine, i have no idea how to interpret this. but if you spend half an hour you might figure it out.
# anyway...
# I SAID MORE EFFECTS!!!

# but we quickly hit the limit in what we can do. we already have y-axis and x-axis as two dimensions.
# colour as third dimesion. shape as fourth and fill as fifth dimension... 
# what can we still do? move into the third dimension! no, i don't mean colour again, but inclusion of another axis (z-axis)
# load the necessary library
library(plotly)
# Create a 3D scatter plot
plot6a = plot_ly(data, 
                 x = ~time, 
                 y = ~stress, 
                 z = ~tiredness, 
                 color = ~experience,        # color based on experience
                 symbol = ~level,            # shape based on level
                 symbols = c("circle", "square", "diamond"), # specify shapes for levels
                 marker = list(size = ~ifelse(beverage == "coffee", 10,
                                              ifelse(beverage == "tea", 7, 5))),
                 opacity = ifelse(data$beverage == "coffee", 1, 
                                  ifelse(data$beverage == "tea", 0.75, 0.5)), # Set transparency based on beverage
                 colors = c("red", "blue")) %>%
  add_markers() %>%
  layout(
    title = "larger means more caffeine, transparent ",
    scene = list(
      xaxis = list(title = "Time (minutes)"),
      yaxis = list(title = "Stress Level"),
      zaxis = list(title = "Tiredness Level")
    ),
    legend = list(title = list(text = "Legend"))
  )
# Display the plot
plot6a  # i originally saved the earlier plots, this was the sixth plot, variation a. this is no longer correct. i don't care.



# I WANT ALL THE EFFECTs!!!!!!!!

# set colors for each participant
participant_colors = c("red", "blue", "green", "orange", "purple", "brown", "pink", "gray")

# create a 3D scatter plot with unique colors for each participant
plot6b = plot_ly(
  data,
  x = ~time,
  y = ~stress,
  z = ~tiredness,
  color = ~factor(participant_id),  # factor to ensure discrete coloring
  colors = participant_colors,      # use predefined colors for each participant
  symbol = ~level,                  # shape based on level
  symbols = c("circle", "square", "diamond"), # specify shapes for levels
  marker = list(
    size = ~ifelse(beverage == "coffee", 10, ifelse(beverage == "tea", 7, 5))
  ),
  opacity = ~ifelse(experience == "experienced", 1, 0.5) # transparency based on experience
) %>%
  add_markers() %>%
  layout(
    title = "transparent means inexpienced, larger means more caffeine",
    scene = list(
      xaxis = list(title = "Time (minutes)"),
      yaxis = list(title = "Stress Level"),
      zaxis = list(title = "Tiredness Level")
    ),
    legend = list(title = list(text = "Participant and Level"))
  )

# display the plot
plot6b

# you see how we quickly hit the limit with what we can show in a plot?
# not that any of this makes any actual sense anymore, no human could derive patterns from this for all effects
# and their possible interactions
length(names(data)) # and we only have a measly 8 dimensions in the data

# that's why we need statistical testing. specifically models. specifcally glmms 
# (in this case, there are other tests that might be more appropriate for other situations)
# the point is: you can't ever get a good understanding of a complex dataset by simply looking at a single plot
# you need to understand your data by running multiple plots. be precise in what you want to investigate
# how you create the plots (ggplots in R or pivot charts in excel etc.) doesn't matter
# and finally, you need to understand that even a simple question can require complex analysis, because you need to 
# control for other effects present in your data. if you don't a lot of the "noise" remains unexplained.
# and this will negatively impact how reliable your results are regarding your test predictor.




# high complexity approach #####
## modelling #####
# in reality, stats can be much more complex than this, but you get the idea.
# consult my glmm-example script for a more complete approach to glmms (including random slopes, diagnostics, post hocs etc.)

# Full Model
# For Gamma distribution with log link function
full.model = glmer(stress ~ experience * level + time + beverage + tiredness +
                     (1 | participant_id),
                   family = Gamma(link = "log"), data = data) # rank deficient: some levels have missing data

# what does "rank deficiency" mean? let's look at the data
names(data)

ggplot(data, aes(x = experience, y = stress)) +
  geom_boxplot()

ggplot(data, aes(x = level, y = stress)) +
  geom_boxplot()


ggplot(data, aes(x = level, y = stress, color=experience)) +
  geom_boxplot()

ggplot(data, aes(x = level, y = stress, fill=experience)) +
  geom_boxplot()+
  facet_wrap(~beverage)

# we can also use tables
table(data$experience, data$beverage, data$level)
# we see that tea is only drunk by inexperienced PhDs, nobody else
# we see that bachelors only drink water, that masters only drink coffe
# in other words, there is not a lot of variation across beverages, experience and level, creating some sub groups without data
# this makes it difficult for the model to differentiate effects of e.g. beverage vs experience for PhDs
# this is what rank-deficiency refers to.
# solution: either collect more balanced data (researchers love this feedback), or remove the problematic predictor from the model.
# here we chose to remove beverage.

# updated model to remove beverage
full.model = glmer(stress ~ experience * level + time + tiredness +
                     (1 | participant_id),
                   family = Gamma(link = "log"), data = data) # does not converge


# there are some approaches to help identify issues with your model. one of them is a collinearity check:
xx = lm(stress ~ experience + level + time + tiredness, data=data)
vif(xx)[, 3]^2 # above 3 might indicate problems, as we see for tiredness here

# to make this short: you remove the least important model terms one at a time until the model converges,
# in this case it converged after removing:
# beverage, tiredness, time, and the interaction between experience and level
# this is mainly because of our small sample size and imbalanced data
# but removing the interaction between experience and level allowed us to include beverage again
# why is that?

full.model = glmer(stress ~ experience + level + beverage + 
                     (1 | participant_id),
                   family = Gamma(link = "log"), data = data) # converges!

# turns out if we remove the interaction between experience and level,
# the model no longer needs to estimate separate effects for every experience:level combination.
# with the interaction, the model needed data for all 6 cells (2 experience × 3 level).
# without it, it only needs to estimate the main effects independently.
# this means beverage no longer creates rank-deficiency, because the model is not trying
# to distinguish beverage effects within specific experience:level subgroups.

# this is a very important point to consider before your data collection already!
# the more interactions you include, the more balanced your data needs to be.
# with limited data, an interaction is sometimes just not possible.
# ideally you plan your sample size and data collection around the model you want to run, not the other way around
# unfortunately most scientists first collect the data and then think about statistics, which is wrong, and creates 
# entirely avoidable limitations in their projects, oftentimes massively limiting scope and scale of conclusions.

# now that we have a working model, let's look at the results:
summary(full.model)

# if the interaction is the point of the study and NEEDS to be kept, we have to throw out the rest:
full.model2 = glmer(stress ~ experience * level + 
                      (1 | participant_id),
                    family = Gamma(link = "log"), data = data)
summary(full.model2)

# had we planned beforehand to ensure all beverages are present in all categories (within interaction) we could have done both

# we could also have collected more data to ensure model convergence with time / tiredness included
# maybe instead of measuring tiredness we could have used a different value, e.g. number of yawns as proxy?
# simulating data and running the model on that beforehand informs us what to watch out for when collecting actual data
# it might also inform us how much data we would need to run a specific model (power analysis)

## data exploration ####
# Data exploration thus far was rather visual, but you can also get quick info via tabular checks to support visual data exploration:

# skimr to check entire dataset real quick 
skimr::skim(data)

# Balance of categorical variables
data %>%
  count(experience, level, beverage) %>%
  arrange(experience, level, beverage) %>%
  adorn_totals("row")

# also show what we do not have by including all possible combinations based on their presence in the data:
data %>%
  count(experience, level, beverage, name = "n") %>%
  complete(experience, level, beverage, fill = list(n = 0)) %>%
  arrange(experience, level, beverage)

# Counts per participant
data %>%
  count(participant_id) %>%
  adorn_totals("row")

# Descriptive stats by groups (overall stress, tiredness)
data %>%
  group_by(experience, level, beverage) %>%
  summarise(
    n = n(),
    stress_mean = mean(stress),
    stress_sd   = sd(stress),
    tired_mean  = mean(tiredness),
    tired_sd    = sd(tiredness),
    .groups = "drop"
  )

# starting point (first minute, t=1) descriptive stats
data %>%
  filter(time == 1) %>%
  group_by(experience, level, beverage) %>%
  summarise(
    n = n(),
    stress_mean = mean(stress),
    stress_sd   = sd(stress), # doesn't work, can't calculate standard deviation of one datapoint
    tired_mean  = mean(tiredness),
    tired_sd    = sd(tiredness), #  doesn't work, can't calculate standard deviation of one datapoint
    .groups = "drop"
  )

## some more plot ideas ####
# histograms and densities for stress and tiredness
p_hist_stress <- ggplot(data, aes(stress)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  labs(title = "Stress distribution", x = "Stress")

p_density_stress <- ggplot(data, aes(stress)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Stress density", x = "Stress")

p_hist_tired <- ggplot(data, aes(tiredness)) +
  geom_histogram(bins = 40, fill = "darkorange", alpha = 0.7) +
  labs(title = "Tiredness distribution", x = "Tiredness")

p_density_tired <- ggplot(data, aes(tiredness)) +
  geom_density(fill = "darkorange", alpha = 0.4) +
  labs(title = "Tiredness density", x = "Tiredness")

# combine multiple plots into one
(p_hist_stress | p_density_stress) / (p_hist_tired | p_density_tired)

# distribution by interaction groups (faceted)
ggplot(data, aes(stress)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_grid(experience ~ level) +
  labs(title = "Stress by experience x level", x = "Stress")


## covariate relationships ####

# Numeric subset for pairwise relationships
num_df <- data %>%
  select(stress, time, tiredness)

# Pair plots with correlations
GGally::ggpairs(num_df)

# relationships by experience plot
ggplot(data, aes(time, stress, color = experience)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~experience) +
  theme_minimal() +
  labs(title = "Stress vs Time by Experience")


# Take home messages ####

# 1. get to know your data. 
# explore visually (ggplot() in r, pivotcharts in excel) or via tables (table() in r, or pivot-tables in excel)
# to help you understand structure, spot outliers, and identify missing groups.

# 2. a low complexity approach might miss effects.
# the more complex your data, the more you need to account for in your analysis.
# ignoring other effects doesn't make them go away, it turns them into unexplained noise.
# this makes it harder to find what you're actually looking for.

# 4. no single plot can show you everything.
# be intentional about what you investigate and  match the plot to the question.

# 5. plan your analysis before you collect your data. not after.
# your sample size, balance across groups, and choice of predictors determine what models you can run and what conclusions you can draw.
# if you think about statistics only after data collection, you risk collecting data that cannot answer your research question.

# 6. while exploring your data is important, be careful that you don't let it influece the questions you want to ask
# your hypothesis are created before data collection, not after looking at a promising plot
# if you test for effects that you think you saw in a plot it quickly becomes cyclical and you risk p-fishing
# explore your data to see if you can do what you want to do and to maybe see what you have to control for
# don't explore your data to come up with a question. unless it is an exploratory projects, but then be up-front about it.

# for more details on GLMMs, see my glmm-example script


# general tip for working on R:
# report your sessionInfo for reproducability (different package versions might give different results):
sessionInfo()
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
# 
# time zone: Europe/Vienna
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] plotly_4.10.4      broom_1.0.5        tidyr_1.3.1        patchwork_1.2.0    nlme_3.1-162      
# [6] lme4_1.1-35.1      Matrix_1.6-5       DHARMa_0.4.7       car_3.1-2          carData_3.0-5     
# [11] performance_0.15.2 GGally_2.2.1       skimr_2.2.1        janitor_2.2.1      ggplot2_4.0.0     
# [16] dplyr_1.1.4       
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.6       xfun_0.42          htmlwidgets_1.6.4  insight_1.4.2      lattice_0.21-8    
# [6] crosstalk_1.2.1    vctrs_0.6.5        tools_4.3.1        generics_0.1.3     parallel_4.3.1    
# [11] tibble_3.2.1       fansi_1.0.4        pkgconfig_2.0.3    data.table_1.15.2  RColorBrewer_1.1-3
# [16] S7_0.2.0           lifecycle_1.0.4    compiler_4.3.1     farver_2.1.1       stringr_1.5.1     
# [21] repr_1.1.7         snakecase_0.11.1   htmltools_0.5.7    yaml_2.3.8         lazyeval_0.2.2    
# [26] crayon_1.5.2       hexbin_1.28.5      pillar_1.9.0       nloptr_2.0.3       ellipsis_0.3.2    
# [31] MASS_7.3-60        boot_1.3-28.1      abind_1.4-5        ggstats_0.9.0      tidyselect_1.2.1  
# [36] digest_0.6.35      stringi_1.8.3      purrr_1.0.4        labeling_0.4.3     splines_4.3.1     
# [41] fastmap_1.1.1      grid_4.3.1         cli_3.6.1          magrittr_2.0.3     base64enc_0.1-3   
# [46] utf8_1.2.3         withr_3.0.0        scales_1.4.0       backports_1.4.1    lubridate_1.9.3   
# [51] timechange_0.3.0   httr_1.4.7         ggtext_0.1.2       knitr_1.45         viridisLite_0.4.2 
# [56] mgcv_1.9-1         rlang_1.1.1        isoband_0.2.7      gridtext_0.1.6     Rcpp_1.0.11       
# [61] glue_1.6.2         xml2_1.3.8         rstudioapi_0.15.0  minqa_1.2.6        jsonlite_2.0.0    
# [66] R6_2.5.1           plyr_1.8.9 

# and store all the work you've done, so you don't need to re-run all calculations from scratch next time you open the script:
# save.image("folder/R_session.RData") # uncomment and adjust to your path


# this is the end of this script
# while this is a brief summary of the topics mentioned in the description at the beginning, it is NOT a complete approach suitable for every dataset / analysis. do your own research and use your best judgement for every one of your analyses, they all differ in one way or another.

