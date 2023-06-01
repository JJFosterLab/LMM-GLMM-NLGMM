# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 06 27
#     MODIFIED:	James Foster              DATE: 2023 06 01
#
#  DESCRIPTION: An example generalised mixed-effect model, using "glmer" from
#               the "lme4" package.
#
#       USAGE:  First install the packages :'lme4', 'report', 'emmeans' 
#               and 'pbkrtest'  as suggested by Rstudio, or by running
#               install.packages('report'); install.packages('emmeans) etc. ...
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: -
#
#   REFERENCES: Bates et al., (2022) Fitting Linear Mixed-Effects Models Using lme4,
#               https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
#               
#               Russell V. Lenth (2021). emmeans: Estimated Marginal Means, 
#               AKA Least-Squares Means. R package version 1.5.4. 
#               https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
#
#               Šidák, Z. K. (1967). "Rectangular Confidence Regions for the 
#               Means of Multivariate Normal Distributions". J. Am. Statist. Assoc. 
#               62 (318): 626–633. doi:10.1080/01621459.1967.10482935.
#
#TODO
#- Add comments +
#- Plot predictions

# Simuluate some data -----------------------------------------------------
set.seed(17070523)#seed the random number generator with Linnaeus' birthday


# . Set up simulation parameters ------------------------------------------

#Experimental variables
n_animals = 10# number of individuals
n_levels = 7# number of stimulus levels per individual
n_types = 2# number of stimulus types
replicates = 20#number of replicates of each condition

#starting parameters
pop_mean = 5.00#population mean
pop_sd = 1.00#population standard deviation
slope_mean = 3.00#population slope
slope_sd = 1.03#population slope standard deviation
type_mean = 2.50# effect of stimulus type 2 on average response
type_slope = 3.5# effect of type 2 on response to stimulus intensity
type_animal_sd = 0.55 # sd of effect of animal on effect of type 2
extra_error_sd = 0.10#small source of unnaccounted error


# . Set up experimental design --------------------------------------------

#Experiment design
animal = rep(x = 1:n_animals, # each animal has a number and experiences each level
             times = n_levels)
animal = sort(animal)# vector of animal numbers organised in order
animal = rep(x = animal, # each animal and level also includes each type
             times = n_types)
stimulus = rep(x = 1:n_levels - 1,# each stimulus has a number and repeats for each animal
               times = n_animals)
stimulus = rep(x = stimulus, # repeat stimulus for each type
               times = n_types)
stype = rep(x = 1:n_types, # each stimulus type for each animal and number
            times = n_animals * n_levels)
stype = sort(stype) # arrange in order

#replicate each observation
animal = rep(x = animal,
             times = replicates)
stimulus = rep(x = stimulus,
               times = replicates)
stype = rep(x = stype,
            times = replicates)

#baseline bias (response when stimulus = 0 and type = 1)
base_bias_animal = rnorm(n = n_animals,
                         mean  = pop_mean,
                         sd = pop_sd)
#response increase with each stimulus level
slope_animal = rnorm(n = n_animals,
                     mean = slope_mean,
                     sd = slope_sd)
#response to type 2
type_animal = rnorm(n = n_animals,
                    mean = 1.0, # on average, no random effect
                    sd = type_animal_sd)

# . Generate simulated data -----------------------------------------------

#Make an empty vector
response_y = rep(x = NA,
                 length = n_animals * n_levels * n_types * replicates)
#Loop through experiment design and generate theoretical response
for (ii in 1:length(response_y))
{
  #each data point is generated from the animal's intercept
  #plus the effect of stimulus type
  #plus the animal's response (slope) to changing stimulus level
  #scaled by the effect of different stimlus types
  #plus a small additional source or error
  response_y[ii] = base_bias_animal[animal[ii]] +
    (stype[ii] - 1) * type_mean +
    (slope_animal[animal[ii]] +
       (stype[ii] - 1) * type_slope * type_animal[animal[ii]]) * stimulus[ii] +
    rnorm(n = 1,
          mean = 0,
          sd = extra_error_sd)
}
#convert to binomial
correct_incorrect = rbinom(n = length(response_y),
                           size = 1,
                           prob = plogis(q = response_y - mean(response_y)) )


# Combine this into a single data.frame format object,
# with the experimental design and the measured response data
sim_data = data.frame(
  correct_incorrect = correct_incorrect,
  animal = LETTERS[animal],
  #animal names will now be capital letters
  stimulus = stimulus,
  type = factor(stype,
                labels = c('alpha',
                           'beta'))
)
View(sim_data)


# Write/Read data ---------------------------------------------------------
# Find a good save location
if(Sys.info()[['sysname']] == 'Windows')
  {
  #get rid of all the backslashes
  ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
}else
{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  ltp = Sys.getenv('HOME')#Easier on Mac
}
root_dir = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                       {file.path(dirname(sys.frame(1)$ofile))},
                     error = function(e)
                     {#if that fails, try to find the "Documents" folder
                       file.path(ltp,'Documents')
                     }
                    )
file_name = 'simulated_binomial_data.csv'
#write this data to an Excel-compatible "comma separated values" file
write.csv(
  file = file.path(root_dir,
                   file_name),
  x = sim_data,
  row.names = F
)
#Now read in this file, or another file in the same format
dta = read.csv(file = file.path(root_dir,
                                file_name),
               header = T)
#format categorical variables as "factors"
dta = within(dta,
             {
               #Run code within this data frame, using it as an environment
               animal = factor(animal)
               type = factor(type)
             })


# Plot data ---------------------------------------------------------------
#Set up plot colours
#on Rstudio these are distinct
clz = sapply(length(unique(dta$animal)),
             colorRampPalette(c(
               'red', 'blue', 'cyan4', 'magenta4', 'orange'
             )))
clz = sample(clz)#Randomise order?

#general overview
#aggragate the proportion correct for each animal
agg = aggregate(x = correct_incorrect~
                   stimulus * type * animal,
                 data = dta,
                FUN = mean)
#inspect the aggregate values
head(agg)
## stimulus  type animal correct_incorrect
## 1        0 alpha      A              0.00
## 2        1 alpha      A              0.00
## 3        2 alpha      A              0.00
## 4        3 alpha      A              0.35
## 5        4 alpha      A              1.00
## 6        5 alpha      A              1.00

# open an empty plot
plot(
  NULL,
  xlab = 'Stimulus intensity',
  ylab = 'Proportion correct',
  xlim = range(stimulus) * c(0.7, 1.3),
  ylim = c(0,1),
)
abline(h = c(0,1))
for (aa in agg$animal)
  with(subset(agg, animal == aa),
       #find the subset of the data with this animal
       {
         #Run code using this data as an environment
         points(
           x = stimulus,
           y = correct_incorrect,
           col = clz[which(LETTERS %in% aa)],
           #colour for this animal
           pch = c(20, 21)[1 + type %in% levels(type)[2]]
           # a dot, filled or open
         )
       })
legend(
  x = 'topright',
  legend = unique(agg$animal),
  col = clz,
  pch = 20
)
legend(x = 'bottomright',
       legend = unique(agg$type),
       pch = c(20, 21))

# Load packages required for mixed-effects modelling ----------------------
require(lme4)
# Fit the GENERALISED linear model ----------------------------------------

#set some useful optimiser settings for models with many paramters
ctrl_opt = glmerControl(optimizer = 'bobyqa') #efficient optimiser for models with many parameters

#Maximal model with random intercepts for individuals
glmm.max = glmer(formula = correct_incorrect~
                  stimulus * type +
                  (1 + stimulus * type | animal),
                 data = dta,
                 control = ctrl_opt,
                 family = binomial(link = 'logit')
)
#boundary (singular) fit: see ?isSingular
#Some random effects are too small to estimate properly, common warning
#Null model, with only random effects
glmm.null = glmer(formula = correct_incorrect~
                   1 + (1|animal),
                   data = dta,
                   family = binomial(link = 'logit')
)
# Model comparison --------------------------------------------------------
#To prove any fixed effects, the mixed-effects model has to
#describe the data better than the random effects model alone.
extractAIC(glmm.max)[2]#2nd component is the AIC, 1st is the d.f.
extractAIC(glmm.null)[2]
#If AIC is lower for the maximal model, then the maximal model fits
# 685.9584 < 3587.245
#We can also perform a likelihood ratio test, confusingly called "anova"
anova(glmm.max, glmm.null, test = 'Chisq')
#This gives the same answer as the AIC, but the difference can be reported with
#a test statistic and p-value
##           npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
## glmm.null    2 3587.2 3599.1 -1791.62   3583.2                         
## glmm.max    14  686.0  769.1  -328.98    658.0 2925.3 12  < 2.2e-16 ***

#report:
#change in deviance = 2925.3 
#change in degrees of freedom = 12
# p < 2.2e-16 (the smallest number the computer can think of)


# Check model reduction options -------------------------------------------
#If we are not sure that we need all of our parameters, we might consider model reduction.
#This is not always necessary, and without a good rationale it may be preferable
#to use the maximal model, which controls for all possible combinations of parameters.

#can we remove one or more fixed effect to improve the model fit?

#effect of stimulus-type interaction
no_int = update(object = glmm.max, 
                .~. - stimulus:type - #remove the interaction
                  (1 + stimulus * type | animal) + #remove random effects that include it
                  (1 + stimulus + type | animal) #replace them with random effect that exclude it
)
anova(glmm.max,
      no_int)
## no_int: correct_incorrect ~ stimulus + type + (1 + stimulus + type | animal)
## glmm.max: correct_incorrect ~ stimulus * type + (1 + stimulus * type | animal)
##            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## no_int      9 719.80 773.24 -350.90   701.80                         
## glmm.max   14 685.96 769.08 -328.98   657.96 43.844  5  2.492e-08 ***
#larger AIC following the parameter's removal indicates 
#that the model without an interaction is a significantly poorer fit
#therefore, there is a significant interaction.

#We can also use this method to report the strength of our fixed effects
#we would report this as:
#likelihood ratio test, change in deviance  = 43.844, d.f. = 5, p < 0.001

#effect of stimulus
no_stimulus = update(object = no_int, 
                     .~. - stimulus - #remove the effect of stimulus
                       (1 + stimulus + type | animal) + #remove random effects that include it
                       (1 + type | animal) #replace them with random effect that exclude it
)
anova(no_int,
      no_stimulus)
##              npar   AIC   BIC   logLik deviance  Chisq Df Pr(>Chisq)    
## no_stimulus    5 2867.2 2896.89 -1428.6   2857.2                         
## no_int         9  719.8  773.24  -350.9    701.8 2155.4  4  < 2.2e-16 ***

#larger AIC following the parameter's removal indicates 
#that the model without an effect of stimulus is a significantly poorer fit
#therefore, there is a significant effect of stimulus
#we would report this as:
#likelihood ratio test, change in deviance  = 2155.4, d.f. = 4, p < 0.001

#effect of type
no_type = update(object = no_stimulus, 
                 .~. - type - #remove the effect of type
                   (1 + type | animal) + #remove random effects that include it
                   (1 | animal) #replace them with random effect that exclude it
)
anova(no_stimulus,
      no_type)
##              npar   AIC   BIC   logLik deviance  Chisq Df Pr(>Chisq)    
## no_type        2 3587.2 3599.1 -1791.6   3583.2                         
## no_stimulus    5 2867.2 2896.9 -1428.6   2857.2 726.04  3  < 2.2e-16 ***

#larger AIC following the parameter's removal indicates 
#that the model without an effect of type is a significantly poorer fit
#therefore, there is a significant effect of type
#we would report this as:
#likelihood ratio test, change in deviance  = 726.04, d.f. = 3, p < 0.001

# Post-hoc comparisons ----------------------------------------------------
#Load the package for post-hoc comparisons
require(emmeans)
require(pbkrtest)# Used for calculating DF in mixed-effects model

# Calculate overall discrete effects
emm_intercepts = emmeans(glmm.max,
                         specs = list(pairwise~type),
                         #compare the overall effects of different types
                         adjust = 'sidak')
summary(emm_intercepts)
## $`pairwise differences of type`
## 1            estimate   SE  df z.ratio p.value
## alpha - beta    -16.3 3.84 Inf  -4.259  <.0001
#As for the likelihood ratio test, we see that there is a large effect of 
#type on probability of a correct response (as there should be, see type_mean to see the different values we started with).
#The estimated difference is that for alpha the odds of a correct response are 
#16 log units lower than for beta for the same stimulus intensity.
#That is an odds ratio of exp(16.3) = 11,994,995 to 1 (beta vs alpha)

# Calculate post-hoc comparisons for continuous effects (slope)
emm_slopes_interact = emtrends(glmm.max,
                               specs = list(pairwise~type),
                               var = 'stimulus',
                               #compare the effects of different types on response/stimulus slopes
                               adjust = 'sidak')
summary(emm_slopes_interact)
## $`pairwise differences of type`
## 1            estimate   SE df t.ratio p.value
## alpha - beta    -7.71 3.7 Inf  -2.083  0.0372
#there is a significant difference in the stimulus-response relationship 
#between the two types. This is as specified in our input variables (type_slope) 
#for the simulation. The estimated difference is that alpha has an average 
#stimulus-response slope that is weaker by "-7.71". That is for every increase 
#in stimulus intensity of 1, the odds of a correct response increase 7.71x faster
#for beta than alpha.



