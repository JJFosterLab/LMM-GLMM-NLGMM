# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 06 27
#     MODIFIED:	James Foster              DATE: 2023 05 28
#
#  DESCRIPTION: An example mixed-effect model, using "lmer" from the "lme4" package.
#
#       USAGE:  First install the packages :'lme4', 'report', 'emmeans', 'lmeTest'
#               and 'pbkrtest'  as suggested by Rstudio, or by running
#               install.packages('report'); install.packages('emmeans) etc. ...
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: - plots
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
#- Plot predictions +

# Basic Mixed Model Script ------------------------------------------------

#  Welcome to R!
#	Statements preceded by "#" are comments,
#	"<-" commands assign the variable to the left with the value to the right
#	"()" contain the targets of functions
#	"{}" contain conditional statements or loops
#	"~" specifies a relationship in a model formula


# Simuluate some data -----------------------------------------------------
set.seed(17070523)#seed the random number generator with Linnaeus' birthday

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

#Experiment design
animal = rep(x = 1:n_animals,
             times = n_levels)
animal = sort(animal)# vector of animal numbers organised in order
animal = rep(x = animal,
             times = n_types)
stimulus = rep(x = 1:n_levels - 1,
               times = n_animals)
stimulus = rep(x = stimulus,
               times = n_types)
stype = rep(x = 1:n_types,
            times = n_animals * n_levels)
stype = sort(stype)

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

#Generate data
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

# Combine this into a single data.frame format object,
# with the experimental design and the measured response data
sim_data = data.frame(
  response_y = response_y,
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
file_name = 'simulated_data.csv'
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

#open an empty plot
plot(
  NULL,
  xlab = 'Stimulus intensity',
  ylab = 'Response strength',
  xlim = range(stimulus) * c(0.7, 1.3),
  ylim = range(response_y) * c(0.7, 1.3),
)
for (aa in dta$animal)
  with(subset(dta, animal == aa),
       #find the subset of the data with this animal
       {
         #Run code using this data as an environment
         points(
           x = stimulus,
           y = response_y,
           col = clz[which(LETTERS %in% aa)],
           #colour for this animal
           pch = c(20, 21)[1 + type %in% levels(type)[2]]
           # a dot, filled or open
         )
       })
legend(
  x = 'topright',
  legend = unique(dta$animal),
  col = clz,
  pch = 20,
  cex = 0.7
)
legend(x = 'bottomright',
       legend = unique(dta$type),
       pch = c(20, 21),
       cex = 0.7
       )



# Fit the maximal model ---------------------------------------------------
#	Fit the most complicated possible model							#

# You know a lot of variables that might have affected your data
# and it is a good idea to check if all (or any) did.
# You do this by fitting a model that has an effect of all variables
# and their interactions (the effect of specific combinations).
# The thing that you measured in your experiment is a
# "response" variable (it responds to the other factors),
# whereas the other factors are "predictor" variables
# (they might help you predict the response variable).
# These are "fixed" effects and their model is:
# response ~ predictor
# , which tells us that we can do something to the predictor
# to guess the response.

# Because individual identity is a proxy for many sources of variation
# they are considered a "random" factor and are entered
# into the model in a different way:
# response ~ (1|random_factor)
# for the intercept and
# response ~ (predictor|random_factor)
# for the slope, and
# response ~ (1+predictor|random_factor)
# for both.

# Mixing fixed and random effects gives us the model including
# everything that might have an effect.

# Load packages required for mixed-effects modelling ----------------------
require(lme4)

# Fit the model -----------------------------------------------------------
#set some useful optimiser settings for models with many parameters
ctrl_opt = lmerControl(optimizer = 'bobyqa')
#Maximal model with random intercepts for individuals
mixmod.max = lmer(formula = response_y ~
                    stimulus * type +
                    (1 + stimulus * type | animal),
                  data = dta,
                  control = ctrl_opt
                  )
#boundary (singular) fit: see ?isSingular
#Some random effects are too small to estimate properly, common warning
#Null model, with only random effects
mixmod.null <- lmer(formula = response_y ~
                      1 + (1 | animal),
                    data = dta)

# Model comparison --------------------------------------------------------
#To prove any fixed effects, the mixed-effects model has to
#describe the data better than the random effects model alone.
extractAIC(mixmod.max)[2]#2nd component is the AIC, 1st is the d.f.
extractAIC(mixmod.null)[2]
#If AIC is lower for the maximal model, then the maximal model fits
# -4706.033 < 22160.38

#We can also perform a likelihood ratio test, confusingly called "anova"
anova(mixmod.max, mixmod.null,test = 'Chisq')
#This gives the same answer as the AIC, but the difference can be reported with
#a test statistic and p-value
##              npar     AIC     BIC  logLik deviance Chisq Df Pr(>Chisq)
## mixmod.null      3 22160.4 22178 -11077  22154.4
## mixmod.max      15 -4706.1 -4617   2368  -4736.1 26890 12  < 2.2e-16 ***

#report:
#change in deviance = 26890
#change in degrees of freedom = 12
# p < 2.2e-16 (the smallest number the computer can think of)

#inspect maximal model
anova(mixmod.max) 
## Analysis of Variance Table
## npar  Sum Sq Mean Sq    F value
## stimulus         1   83.27   83.27   8700.789
## type             1 1225.14 1225.14 128016.524
## stimulus:type    1    0.40    0.40     42.314

#the large F values give a general indication of effect sizes
#but for a mixed effects model we need to use the post-hoc methods 
#below to determine their significance

# Check model reduction options -------------------------------------------
#If we are not sure that we need all of our parameters, we might consider model reduction.
#This is not always necessary, and without a good rationale it may be preferable
#to use the maximal model, which controls for all possible combinations of parameters.

#can we remove one or more fixed effect to improve the model fit?

#effect of stimulus-type interaction
no_int = update(object = mixmod.max, 
                .~. - stimulus:type - #remove the interaction
                    (1 + stimulus * type | animal) + #remove random effects that include it
                    (1 + stimulus + type | animal) #replace them with random effect that exclude it
                  )
anova(mixmod.max,
      no_int)
##              npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)    
## no_int       10 15687 15746 -7833.4    15667                        
## mixmod.max   15 -4706 -4617  2368.0    -4736 20403  5  < 2.2e-16 ***
#larger AIC following the parameter's removal indicates 
#that the model without an interaction is a significantly poorer fit
#therefore, there is a significant interaction.

#We can also use this method to report the strength of our fixed effects
#we would report this as:
#likelihood ratio test, change in deviance  = 20403, d.f. = 5, p < 0.001

#effect of stimulus
no_stimulus = update(object = no_int, 
                      .~. - stimulus - #remove the effect of stimulus
                        (1 + stimulus + type | animal) + #remove random effects that include it
                        (1 + type | animal) #replace them with random effect that exclude it
                    )
anova(no_int,
      no_stimulus)
##              npar   AIC   BIC   logLik deviance  Chisq Df Pr(>Chisq)    
## no_stimulus    6 21175 21211 -10581.7    21163                         
## no_int        10 15687 15746  -7833.4    15667 5496.6  4  < 2.2e-16 ***

#larger AIC following the parameter's removal indicates 
#that the model without an effect of stimulus is a significantly poorer fit
#therefore, there is a significant effect of stimulus
#we would report this as:
#likelihood ratio test, change in deviance  = 5496.6, d.f. = 4, p < 0.001

#effect of type
no_type = update(object = no_stimulus, 
                      .~. - type - #remove the effect of type
                        (1 + type | animal) + #remove random effects that include it
                        (1 | animal) #replace them with random effect that exclude it
                    )
anova(no_stimulus,
      no_type)
##              npar   AIC   BIC   logLik deviance  Chisq Df Pr(>Chisq)    
## no_type        3 22160 22178 -11077    22154                         
## no_stimulus    6 21175 21211 -10582    21163 991.04  3  < 2.2e-16 ***

#larger AIC following the parameter's removal indicates 
#that the model without an effect of type is a significantly poorer fit
#therefore, there is a significant effect of type
#we would report this as:
#likelihood ratio test, change in deviance  = 991.04, d.f. = 3, p < 0.001

# Post-hoc comparisons ----------------------------------------------------
#Load the package for post-hoc comparisons
require(emmeans)
require(pbkrtest)# Used for calculating DF in mixed-effects model

# Calculate overall discrete effects
emm_intercepts = emmeans(mixmod.max,
                         specs = list(pairwise~type),
                         #compare the overall effects of different types
                         adjust = 'sidak')
summary(emm_intercepts)
## $`pairwise differences of type`
## 1            estimate   SE df t.ratio p.value
## alpha - beta      -13 1.62  9 -8.043  <.0001 
#As for the likelihood ratio test, we see that there is a large effect of 
#type on average response (as there should be, see type_mean to see the different values we started with).
#The estimated difference is that alpha has an average response strength that is
#weaker by "13" response strength units.

# Calculate post-hoc comparisons for continuous effects (slope)
emm_slopes_interact = emtrends(mixmod.max,
                     specs = list(pairwise~type),
                     var = 'stimulus',
                     #compare the effects of different types on response/stimulus slopes
                     adjust = 'sidak')
summary(emm_slopes_interact)
## $`pairwise differences of type`
## 1            estimate   SE df t.ratio p.value
## alpha - beta    -3.51 0.54  9 -6.505  0.0001 
#there is a significant difference in the stimulus-response relationship 
#between the two types. This is as specified in our input variables (type_slope) 
#for the simulation. The estimated difference is that alpha has an average 
#stimulus-response slope that is weaker by "3.51" response strength units per stimulus unit.


# Plot predictions --------------------------------------------------------

# . Extract predictions ---------------------------------------------------
#check all relevant variables for predictions
formula(mixmod.max)
## lag ~ 1 + temperature + treatment + (1 + temperature | Animal.number)
newdta = with(dta,
              expand.grid(stimulus = unique(stimulus),
                          type = unique(type),
                          animal = unique(animal)
              ) 
)
#predictions (mean estimate)
prd = predict(mixmod.max,
              newdata = newdta,
)
#bootstrap the confidence intervals (can take a while...)
pfun = function(x)
{
  predict(x,
          newdata = newdta)
}
# library(snow)#parallel processing requires "snow" on Windows
#Really benefits from some parallel processing
avail.cores = parallel::detectCores() - 1
clt = parallel::makeCluster(spec = avail.cores, 
                            type="SOCK")
parallel::clusterExport(clt,
                        list('mixmod.max',
                             'dta',
                             'newdta'
                        )
)
#this takes a long time to simulate
system.time({
  bt = bootMer(mixmod.max,
               FUN = pfun,
               nsim = 100,#100 takes ≈60 seconds. Minimum of 20 to be able to calculate 95%CI. Increase number for greater detail.
               re.form = NULL,#NA for fixed effects, NULL to include random effects
               parallel = ifelse(test = Sys.info()[['sysname']] == 'Windows',
                                 yes =  "snow",
                                 no =  "multicore"),
               ncpus = parallel::detectCores()-1, #leave one processor available for user
               cl = clt #the parallel cluster prepared above
  )
})
#now it has been used, close the cluster
parallel::stopCluster(clt)
#For reference, confint gives confidence intervals for each datapoint
param_ci = confint(bt) #fast method
#To get CI for only fixed effects, align and aggregate bootstrap estimates
pred_q = aggregate(pred ~ stimulus*type, #aggregate predictions by fixed effect
                   FUN = quantile, #calculate as quantiles of bootstrap predictions
                   data = with(bt, 
                               data.frame(newdta, 
                                          pred = c(t(t)) #convert from rows to columns and align 
                               ) 
                   ),
                   probs = c(0,1) + c(1,-1)*0.05/2) #using alpha = 0.05, make two-tailed confidence intervals
# find mean prediction across 
mod_mean = aggregate(prd ~ stimulus*type, 
                     data = cbind(newdta, prd), 
                     FUN = mean)
#merge together
param_data = merge(merge(newdta, pred_q, all = TRUE), 
                   mod_mean)
#rename for plotting
mod_pred = within(param_data,
                  {
                    mod_mean = prd
                    CI_02.5 = pred[,'2.5%']
                    CI_97.5 = pred[,'97.5%']
                    rm(list = c('pred','prd'))
                  }
)

# . Plot predictions ------------------------------------------------------

#plot all data together
with(dta,
     {
     plot(x = stimulus,
          y = response_y,
          bg = adjustcolor(col = c('orange2', 'darkblue')[ 1 + type %in% 'alpha'],
                           alpha.f = 0.5), # 50% opacity
          col = 'black',
          pch = 21) # dots
     }
)
#add model and shaded confidence intervals
#stimulus type alpha
with(subset(mod_pred, type %in% 'alpha'),
     {
       polygon(x = c(sort(stimulus), sort(stimulus,decreasing = TRUE)),
               y = 
                 c(lowerCI = CI_02.5[order(stimulus)],
                   upperCI = CI_97.5[order(stimulus,decreasing = TRUE)]
                 ),
               col = adjustcolor(col = 'darkblue',
                                 alpha.f = 0.2),
               border = NA
       )
       lines(sort(stimulus),
             mod_mean[order(stimulus)],#
             col = 'darkblue',
             lwd = 3
       )
     }
)
#stimulus type beta
with(subset(mod_pred, type %in% 'beta'),
     {
       polygon(x = c(sort(stimulus), sort(stimulus,decreasing = TRUE)),
               y = 
                 c(lowerCI = CI_02.5[order(stimulus)],
                   upperCI = CI_97.5[order(stimulus,decreasing = TRUE)]
                 ),
               col = adjustcolor(col = 'orange2',
                                 alpha.f = 0.2),
               border = NA
       )
       lines(sort(stimulus),
             mod_mean[order(stimulus)],
             col = 'orange2',
             lwd = 3
       )
     }
)

legend(x = 'topleft',
       legend = c('alpha', 'beta'),
       col = c('darkblue', 'orange2'),
       pch = 15)
     
