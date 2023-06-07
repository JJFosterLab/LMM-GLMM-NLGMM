# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2023 06 02
#     MODIFIED:	James Foster              DATE: 2023 06 06
#
#  DESCRIPTION: Fit a nonlinear logistic mixed-effects model
#               
#       INPUTS: 
#               
#      OUTPUTS: Test result
#
#	   CHANGES: - 
#
#   REFERENCES: Olsson P, Johnsson RD, Foster JJ, Kirwan JD, Lind O & Kelber A (2020)
#               Chicken colour discrimination depends on background colour. 
#               J. Exp. Biol. 223 jeb.209429 doi: 10.1242/jeb.209429
#               
#               Gabry J, Češnovar R, Johnson A (2022). 
#               cmdstanr: R Interface to 'CmdStan'.
#               https://mc-stan.org/cmdstanr/
# 
#               Bürkner, P.-C. (2018). 
#               Advanced Bayesian Multilevel Modeling with the R Package brms. 
#               The R Journal 10, 395–411.
# 
#               Carpenter, B., Gelman, A., Hoffman, M. D., Lee, D., Goodrich, B., 
#               Betancourt, M., Brubaker, M., Guo, J., Li, P. and Riddell, A. (2017). 
#               Stan: A Probabilistic Programming Language. 
#               Journal of Statistical Software 76 doi: 10.18637/jss.v076.i01
# 
#       USAGE:  
#TODO   ---------------------------------------------
 # - Simulate data
 # - Fit model
 # - Choose priors
 # - Post-hoc interpretations
 # - Simulate individual lapse rates


# Simuluate some data -----------------------------------------------------
set.seed(17070523)#seed the random number generator with Linnaeus' birthday


# . Set up simulation parameters ------------------------------------------

#Experimental variables
n_animals = 10# number of individuals
n_levels = 7# number of stimulus levels per individual
n_types = 2# number of stimulus types
replicates = 20#number of replicates of each condition

#A sigmoid curve can be described in terms of its inflection (turning) point, 
#where the slope stops increasing and begins decreasing, and the width of the 
#x-axis region where the curve completes most (e.g. 80%) of its rise.

#starting parameters
width_alpha = 0.8 # proportion of the rise region that we will call the "width"
width_coef = 2*log(2/(1-width_alpha)-1) # coefficient to rescale curve to width
inflex = 15 # inflection point of first process
width = -10 # 80% width of rise region with negative slope (x*-1)
lapse = 0.12 # rate of incorrect ("lapses") at maximum performance
base = 0.5 # baseline rate of correct choices: for two alternatives, expect at least 50% correct

pop_inflex = 2.00#population mean
pop_sd = 1.00#population standard deviation
pop_width = 3.00#population slope
width_sd = 1.03#population slope standard deviation
type_inflex = 2.50# effect of stimulus type 2 on average response
type_width = 3.5# effect of type 2 on response to stimulus intensity
type_animal_sd = 0.55 # sd of effect of animal on effect of type 2
lapse_animal_sd = 0.50 # sd of lapse rates for different animals on log(odds) scale
extra_error_sd = 0.10#small source of unaccounted error

#Plot ideal curve for stimulus type 1
xx = seq(from = 0,
         to  = n_levels-1,
         length.out = 1e3)
plot(x = xx,
    y = base + (1 - lapse - base) * 
              plogis(q = 
                width_coef* (xx - pop_inflex) / pop_width
                ),
    xlim = c(0,n_levels-1),
    ylim = c(0,1),
    type = 'l',
    col = 'blue',
    xlab = 'Stimulus',
    ylab = 'Proportion correct'
     )
abline(h = c(0,1,base, 1- lapse), 
       lty = c(1,1,3,3))
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
                         mean  = pop_inflex,
                         sd = pop_sd)
#response increase with each stimulus level
width_animal = rnorm(n = n_animals,
                     mean = pop_width,
                     sd = width_sd)
#response to type 2
type_animal = rnorm(n = n_animals,
                    mean = 1.0, # on average, no random effect
                    sd = type_animal_sd)
#lapse rates for each animal
lapse_animal = plogis(q = #convert to probability scale
  rnorm(n = n_animals,
                    mean = qlogis(lapse), #lapse rate in log(odds)
                    sd = lapse_animal_sd)
)

# . Generate simulated data -----------------------------------------------

# the sigmoid bounded between a baseline and the maximum response (1- rate of lapsing)
# takes the form:
# y = baseline + (1 - lapse_rate - baseline) * sigmoid
# where the sigmoid is the logistic curve:
# p  = 1/ ( 1 + exp(-log(odds_ratio)))

#Make an empty vector
response_y = rep(x = NA,
                 length = n_animals * n_levels * n_types * replicates)
#Loop through experiment design and generate theoretical response
for (ii in 1:length(response_y))
{
  #each data point is generated between the base rate
  #and the animal's maximum response (1-lapse rate)
  #responses scale relative to the inflection point (stimulus - inflection)
  #and the rise rate of the curve is inverse to its width: (stim - inflect) / width
  #inflection and width differ for each stimulus type and animal
  #plus a small additional source or error
  #simulate choice average
  response_y[ii] = base + (1 - lapse_animal[animal[ii]] - base)* #nonlinear parameters
                          #linear component
                          plogis(q = 
                                   width_coef * (stimulus[ii] - 
                                # effects on inflection point
                                   (base_bias_animal[animal[ii]] +
                                      (stype[ii] - 1) * type_inflex)
                                 )/ 
                                #effects on width
                                   (width_animal[animal[ii]] +
                                      (stype[ii] - 1) * type_width * 
                                      type_animal[animal[ii]]) +
                                #residual error
                                rnorm(n = 1,
                                      mean = 0,
                                      sd = extra_error_sd)
                          )
}
#convert to binomial
correct_incorrect = rbinom(n = length(response_y),
                           size = 1,
                           prob = response_y )


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
file_name = 'simulated_nonlinear_data.csv'
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
agg = aggregate(correct_incorrect~
                  stimulus * type * animal,
                data = dta,
                FUN = mean)
#inspect the aggregate values
head(agg)
## stimulus  type animal correct_incorrect
#1        0 alpha      A              0.50
#2        1 alpha      A              0.70
#3        2 alpha      A              0.85
#4        3 alpha      A              0.60
#5        4 alpha      A              0.70
#6        5 alpha      A              0.75

# open an empty plot
plot(
  NULL,
  xlab = 'Stimulus intensity',
  ylab = 'Proportion correct',
  xlim = range(stimulus) * c(0.7, 1.3),
  ylim = c(0,1),
)
abline(h = c(0,1,base, 1- lapse), 
       lty = c(1,1,3,3))
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


# Install and set up Bayesian modelling -----------------------------------

#make sure modelling packages are installed

#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS = function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
}

#first download and install Rtools4.3 
shell.exec.OS('https://cran.r-project.org/bin/windows/Rtools/')

#install the package for Bayesian modelling on Windows
if(!any(installed.packages() %in% 'remotes'))
{install.packages('remotes')}#install the package for installing remote packages

if(!any(installed.packages() %in% 'cmdstanr'))
{remotes::install_github("stan-dev/cmdstanr")} #follow all instructions

#install accompanying executable program
require(cmdstanr)
install_cmdstan(cores = parallel::detectCores()-1, overwrite = FALSE)# may take a while & print a lot!
#if this fails, try https://github.com/stan-dev/cmdstan/archive/refs/tags/v2.30.1.zip

#install package for writing Bayesian models
if(!any(installed.packages() %in% 'brms'))
{install.packages('brms')}

#test cmdstanr
cmdstanr::check_cmdstan_toolchain()
cmdstanr::set_cmdstan_path(path = cmdstanr::cmdstan_path())

#load packages
require(cmdstanr)
require(brms)


# Set up non-linear model -------------------------------------------------


# . Define model formula --------------------------------------------------

#for our model, we would like to define width as 80% of the curve rise
print(round( 
  2*log(2/
          (1- 0.8) -1),
  digits = 2 )) # coeficient to rescale curve to width))
#this is approximately 4.39

#set up model fit
formula_nl = bf(
  #set up a formula for the curve as a whole,
  #including parameters found in the data (correct_incorrect, stimulus)
  #and parameters that we wish to estimate (baseline, lapse rate, inflection point, width).
  #Most of these are subject to further fixed (type) and random (animal) effects,
  #these need to be defined for each parameter.
  
  #Two parameters need special transformations
  #To keep the output between 0 and 1, additional effects of lapse rate
  #will be added on the "logit" scale (Lapse = inv_logit(LogitLapse)).
  #To avoid curve widths of 0, we can assume a positive slope (≥0)
  #and add additional effects to width on a log scale (Width = exp(LogWidth))
  formula = correct_incorrect ~ 
    Base + (1 - inv_logit(LogitLapse) - Base) *#curve region
    inv_logit( 4.39*(stimulus - Inflex) / exp(LogWidth) ) , #inflection-width curve
  # for each of these parameters, we can set up a separate formula 
  # that describes how to predict them from the data 
  #Base rate of correct choices: "
  Base ~ 1, #Base rate of correct choices: "~ 1" gives the instruction "estimate the mean across all data"
  #Lapse rate on a log(odds) scale:
  LogitLapse ~ type + (1 + type|animal), #this is similar to the formula in our LMM example
  #inflection point of the initial curve:
  Inflex ~ type + (1 + type|animal), #N.B. this is similar to the intercept, so it does not include effects of stimulus level
  #log 80% width of the curve:
  LogWidth ~ type + (1 + type|animal), #N.B. this is similar to the slope, so all of its effects depend on stimulus level
  family = bernoulli("identity"),
  nl = TRUE)#the joint distribution for these parameters is undefined, and therefore the parameters themselves are "nonlinear"

#set up a null model
formulaNull_nl = bf(
  #set up a formula for the curve as a whole,
  #including parameters found in the data (correct_incorrect, stimulus)
  #and parameters that we wish to estimate (baseline, lapse rate, inflection point, width).
  #Most of these are subject to futher fixed (type) and random (animal) effects,
  #these need to be defined for each parameter.
  
  #Two parameters need special transformations
  #To keep the output between 0 and 1, additional effects of lapse rate
  #will be added on the "logit" scale (Lapse = inv_logit(LogitLapse)).
  #To avoid curve widths of 0, we can assume a positive slope (≥0)
  #and add additional effects to width on a log scale (Width = exp(LogWidth))
  formula = correct_incorrect ~ 
    Base + (1 - inv_logit(LogitLapse) - Base) *#curve region
    inv_logit( LogitMean ) , #inflection-width curve
  Base ~ 1, #Base rate of correct choices
  LogitLapse ~ 1 + (1|animal), #Lapse rate on a log(odds) scale
  LogitMean ~ 1 + (1|animal), #mean log(odds) of a correct choice (intercept of an LMM)
  family = bernoulli("identity"),
  nl = TRUE)#the joint distribution for these parameters is undefined, and therefore the parameters themselves are "nonlinear"


# . Define model priors ---------------------------------------------------

# For many types of model with multiple parameters, the search for the correct
# parameter values needs to focus on the right range of potential values.
# This is especially true for non-linear models, where the wrong value of one
# parameter can exclude the possiblity of the right value for other parameters.
# In Bayesian modelling, we define prior (expected) distributions for each parameter.
# This informs the modelling algorithm when parameter estimates become too different
# from the values we expect, which prevents it from returning implausible estimates
# (or failing to converge on any estimates).

# By default, expected distributions (prior) for non-linear parameters (nlpar) 
# for fixed effects coefficients (class: b) are uniform (flat): 
# any value is equally plausible!
# For random effects standard deviations (class: sd) the expected distribution
# is a very wide student t distribution: (3 degrees of freedom, centre 0, st. dev. 2.5):
# the differences in parameter values can vary by any amount, 
# but small variations are more plausible.

# Because we have random effects of animal on both Intercept and stimulus type
# brms adds a correlation parameter, with a Lewandowski-Kurowicka-Joe distributed 
# prior, for potential correlation between these two effects within each animal. 

prior_nl = get_prior(formula = formula_nl,
                     data = dta)
##  prior                  class  coef  group resp dpar      nlpar lb ub       source
## lkj(1)                  cor                                                  default
## lkj(1)                  cor           animal                            (vectorized)
## (flat)                    b                                  Base            default
## (flat)                    b Intercept                        Base       (vectorized)
## (flat)                    b                                Inflex            default
## (flat)                    b Intercept                      Inflex       (vectorized)
## (flat)                    b  typebeta                      Inflex       (vectorized)
## student_t(3, 0, 2.5)    sd                                Inflex  0         default
## student_t(3, 0, 2.5)    sd           animal               Inflex  0    (vectorized)
## student_t(3, 0, 2.5)    sd Intercept animal               Inflex  0    (vectorized)
## (flat)                    b                            LogitLapse            default
## (flat)                    b Intercept                  LogitLapse       (vectorized)
## (flat)                    b  typebeta                  LogitLapse       (vectorized)
## student_t(3, 0, 2.5)    sd                            LogitLapse  0         default
## student_t(3, 0, 2.5)    sd           animal           LogitLapse  0    (vectorized)
## student_t(3, 0, 2.5)    sd Intercept animal           LogitLapse  0    (vectorized)
## (flat)                    b                            LogWidth            default
## (flat)                    b Intercept                  LogWidth       (vectorized)
## (flat)                    b  typebeta                  LogWidth       (vectorized)
## student_t(3, 0, 2.5)    sd                             LogWidth  0         default
## student_t(3, 0, 2.5)    sd           animal            LogWidth  0    (vectorized)
## student_t(3, 0, 2.5)    sd Intercept animal            LogWidth  0    (vectorized)

# To make sure the model converges, and returns plausible parameter estimates
# it would be a good idea to define some expected distributions for our parameters.
# A normal distribution with the mean around the centre of possible values
# and a moderate standard deviation (1-3) indicates a range of values not more than
# 2-6 units of the parameter 
#set priors to ensure convergence


# . . Base rate prior -----------------------------------------------------
#for the baseline, we will use a beta distribution with a bias towards 0.5

#inspect the prior distribution
#beta(20,20) has 95% probability of values between 0.35 and 0.65
round(qbeta(p = c(0,1) + #lower & upper
              (c(1,-1)/2) * #add and subtract half the interval
              (1 - 0.95), #proportion of probability density outside interval
            shape1 = 20, 
            shape2 = 20),
      digits = 2)
xseq = seq(from  = 0, to = 1, by  = 0.01)
plot(x = xseq, 
     y = dbeta(x = xseq,
               shape1 = 20, 
               shape2 = 20), 
     ylab = 'probability density',
     xlab = 'expected value: baseline rate',
     type = 'l', 
     col = 4)
abline(v = c(0.5,
             qbeta(p = c(0,1) + #lower & upper
                     (c(1,-1)/2) * #add and subtract half the interval
                     (1 - 0.95), #proportion of probability density outside interval
                   shape1 = 20, 
                   shape2 = 20)),
       lty = c(1,3,3)
       )

#set the prior distribution
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'Base' &
                      coef %in% 'Intercept' 
                      ] = 'beta(20,20)' #a beta distribution centred on 0.5
              })
#this prior is automatically bounded between 0 and 1, 
# but we may want to set additional upper and lower bounds 
#lower bound
prior_nl = within(prior_nl, 
              { lb[
                nlpar %in% 'Base' #&
                ] = 0 #baseline should not be less than 0
              })
#upper bound
prior_nl = within(prior_nl, 
              { ub[
                class %in% 'b' & #just the fixed effects
                nlpar %in% 'Base' #&
                ] = 0.75 #if we expect lapse rates of 5-10%, baseline should not reach this range
              })
# . . Lapse rate priors ---------------------------------------------------
#for the lapse rates, we will use a normal distribution with a strong bias 
# towards log(odds) = -3, prob ≈0.05
# (see qlogis(0.05) for conversion from prob to log odds)
# this expectation is still very broad, 95% probability between 0.0001 and 0.9468
round(plogis(qnorm(p = c(0,1) + #lower & upper
              (c(1,-1)/2) * #add and subtract half the interval
              (1 - 0.95), #proportion of probability density outside interval
            mean = -3, 
            sd = 3)
            ),
      digits = 4)
xseq = seq(from  = 0, to = 1, by  = 0.01)
plot(x = xseq, 
     y = dnorm(x = qlogis(xseq),
               mean = -3, 
               sd = 3), 
     ylab = 'probability density',
     xlab = 'expected value: lapse rate',
     type = 'l', 
     col = 4)
abline(v = c(0.05,
             plogis(
               qnorm(p = c(0,1) + #lower & upper
                       (c(1,-1)/2) * #add and subtract half the interval
                       (1 - 0.95), #proportion of probability density outside interval
                     mean = -3, 
                     sd = 3)
               )
             ),
       lty = c(1,3,3)
)

# set the prior distribution
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitLapse' &
                      coef %in% 'Intercept' 
                      ] = 'normal(-3,3)' #a normal distribution centred on -3
              })

#for all other fixed effects on lapse rate, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitLapse' &
                      coef %in% 'typebeta' 
                      ] = 'normal(0,3)' #a normal distribution:mean 0, sd 3
              })

# . . Inflection point priors ---------------------------------------------
#for the population level inflection point, we expect it to be somewhere
#in the middle of our stimulus range (0-6) = 3
# this expectation is broad, 95% probability of inflection points between -2.88 and 8.88
round(qnorm(p = c(0,1) + #lower & upper
                     (c(1,-1)/2) * #add and subtract half the interval
                     (1 - 0.95), #proportion of probability density outside interval
                   mean = 3, 
                   sd = 3),
digits = 2)

#inspect the prior distribution
xseq = seq(from  = -5, to = 10, by  = 0.01)
plot(x = xseq, 
     y = dnorm(x = xseq,
               mean = 3, 
               sd = 3), 
     ylab = 'probability density',
     xlab = 'expected value: inflection point',
     type = 'l', 
     col = 4)
abline(v = c(3,
             qnorm(p = c(0,1) + #lower & upper
                       (c(1,-1)/2) * #add and subtract half the interval
                       (1 - 0.95), #proportion of probability density outside interval
                     mean = 3, 
                     sd = 3)
            ),
        lty = c(1,3,3)
)

#set the priors
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'Inflex' &
                      coef %in% 'Intercept'
                      ] = 'normal(3,3)' #a normal distribution:mean 3, sd 3
              })
#for all coefficients, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'Inflex' &
                      coef %in% 'typebeta' 
                      ] = 'normal(0,3)' #a normal distribution:mean 0, sd 3
              })

# . . Rise region width priors --------------------------------------------
#for the population level width, we expect it to be a positive value (>exp(-Inf))
#somewhat smaller than the range of stimulus values
#it might be reasonable to also expect an 80% rise region of around 3 ≈ exp(1)
#this expectation is broad, 95% probability of widths between 0.01 and 972.52
#but with the strong assumption the curves are either positive (increase with stimulus level)
#or nearly flat (requiring many times the range of available stimuli to rise).
#This would not be the case in experiments where correct response rates _decrease_ 
#as a function of increasing stimulus (estimate Width instead of LogWidth).
round(exp(qnorm(p = c(0,1) + #lower & upper
              (c(1,-1)/2) * #add and subtract half the interval
              (1 - 0.95), #proportion of probability density outside interval
            mean = 1, 
            sd = 3)
          ),
      digits = 2)

#inspect the prior distribution
xseq = seq(from  = 0.001, to = 7, by  = 0.01)
plot(x = xseq, 
     y = dnorm(x = log(xseq),
               mean = 1, 
               sd = 3), 
     ylab = 'probability density',
     xlab = 'expected value: rise region width',
     type = 'l', 
     col = 4)
abline(v = c(3,
             exp(
               qnorm(p = c(0,1) + #lower & upper
                       (c(1,-1)/2) * #add and subtract half the interval
                       (1 - 0.95), #proportion of probability density outside interval
                     mean = 3, 
                     sd = 3)
                 )
            ),
      lty = c(1,3,3)
)

#set prior distribution
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogWidth' &
                      coef %in% 'Intercept' 
                      ] = 'normal(1,3)' #a normal distribution:mean 3, sd 3
              })
#for all coefficients, we'll suggest values around 0 (no effect)
prior_nl = within(prior_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogWidth' &
                      coef %in% 'typebeta'
                      ] = 'normal(0,3)' #a normal distribution:mean 0, sd 3
              })

print(prior_nl)
## prior                class  coef  group resp dpar      nlpar lb ub  source
## (flat)               b                                  Base            default
## beta(20,20)          b Intercept                        Base 0 0.75     default
## (flat)               b                                Inflex            default
## normal(3,3)          b Intercept                      Inflex            default
## normal(0,3)          b  typebeta                      Inflex            default
## student_t(3, 0, 2.5) sd                                Inflex  0         default
## student_t(3, 0, 2.5) sd           animal               Inflex  0    (vectorized)
## student_t(3, 0, 2.5) sd Intercept animal               Inflex  0    (vectorized)
## (flat)               b                            LogitLapse            default
## normal(-3,3)         b Intercept                  LogitLapse            default
## normal(0,3)          b  typebeta                  LogitLapse            default
## student_t(3, 0, 2.5) sd                            LogitLapse  0         default
## student_t(3, 0, 2.5) sd           animal           LogitLapse  0    (vectorized)
## student_t(3, 0, 2.5) sd Intercept animal           LogitLapse  0    (vectorized)
## (flat)               b                                 Width            default
## normal(3,3)          b Intercept                       Width            default
## normal(0,3)          b  typebeta                       Width            default
## student_t(3, 0, 2.5) sd                                 Width  0         default
## student_t(3, 0, 2.5) sd           animal                Width  0    (vectorized)
## student_t(3, 0, 2.5) sd Intercept animal                Width  0    (vectorized)


# . Define null priors ----------------------------------------------------

priorNull_nl = get_prior(formula = formulaNull_nl,
                         data = dta)
# . . Base rate prior -----------------------------------------------------
priorNull_nl = within(priorNull_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'Base' &
                      coef %in% 'Intercept' 
                      ] = with(prior_nl, #use the same prior as the full model
                                 {
                                   prior[
                                          class %in% 'b' & #just the fixed effects
                                          nlpar %in% 'Base' &
                                          coef %in% 'Intercept' 
                                        ]
                                 }
                                ) 
              })

#this prior is automatically bounded between 0 and 1, 
# but we may want to set additional upper and lower bounds 
#lower bound
priorNull_nl = within(priorNull_nl, 
              { lb[
                  nlpar %in% 'Base' #
                  ] = with(prior_nl, #use the same prior as the full model
                           {
                             lb[
                               nlpar %in% 'Base' #
                               ] 
                           }
                           )
              })
#upper bound
priorNull_nl = within(priorNull_nl, 
              {ub[
                nlpar %in% 'Base' #
              ] = with(prior_nl, #use the same prior as the full model
                       {
                         ub[
                           nlpar %in% 'Base' #
                         ] 
                       }
              )
              })

# . . Lapse rate priors ---------------------------------------------------
priorNull_nl = within(priorNull_nl,
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitLapse' &
                      coef %in% 'Intercept' 
                      ] = with(prior_nl, #use the same prior as the full model
                               {
                                 prior[
                                       class %in% 'b' & #just the fixed effects
                                       nlpar %in% 'LogitLapse' &
                                       coef %in% 'Intercept' 
                                       ]
                               }
                               )
              })
#for all other fixed effects on lapse rate, we'll suggest values around 0 (no effect)
priorNull_nl = within(priorNull_nl, 
              { prior[
                      class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitLapse' &
                      coef %in% 'typebeta' 
                      ] = with(prior_nl, #use the same prior as the full model
                               {
                                 prior[
                                     class %in% 'b' & #just the fixed effects
                                     nlpar %in% 'LogitLapse' &
                                     coef %in% 'typebeta' 
                                 ] 
                               }
                               )
              })

# . . Mean rate priors ---------------------------------------------------
#for the mean rates, we will use a normal distribution with a weak bias 
# towards log(odds) = 0, prob  = 0.5
# (see qlogis(0.5) for conversion from prob to log odds)

# this expectation is very broad, 95% probability between 0.0028 and 0.9972
round(plogis(qnorm(p = c(0,1) + #lower & upper
                     (c(1,-1)/2) * #add and subtract half the interval
                     (1 - 0.95), #proportion of probability density outside interval
                   mean = 0, 
                   sd = 3)
),
digits = 4)
xseq = seq(from  = 0, to = 1, by  = 0.01)

#inspect prior distribution
plot(x = xseq, 
     y = dnorm(x = qlogis(xseq),
               mean = 0, 
               sd = 3), 
     ylab = 'probability density',
     xlab = 'expected value: mean rate',
     type = 'l', 
     col = 4)
abline(v = c(0.5,
             plogis(
               qnorm(p = c(0,1) + #lower & upper
                       (c(1,-1)/2) * #add and subtract half the interval
                       (1 - 0.95), #proportion of probability density outside interval
                     mean = 0, 
                     sd = 3)
             )
),
lty = c(1,3,3)
)

#set prior distribution
priorNull_nl = within(priorNull_nl, 
                  { prior[
                          class %in% 'b' & #just the fixed effects
                          nlpar %in% 'LogitMean' &
                          coef %in% 'Intercept' 
                         ] = 'normal(0,3)' #a normal distribution centred on 0
                  })

print(priorNull_nl)# should look like the priors for the full model, but without fixed effects "stimulus" and "type"
## prior                  class      coef  group resp dpar      nlpar lb   ub       source
## (flat)                    b                                  Base  0 0.75      default
## beta(20,20)               b Intercept                        Base  0 0.75      default
## (flat)                    b                            LogitLapse              default
## normal(-3,3)              b Intercept                  LogitLapse              default
## student_t(3, 0, 2.5)     sd                            LogitLapse  0           default
## student_t(3, 0, 2.5)     sd           animal           LogitLapse  0      (vectorized)
## student_t(3, 0, 2.5)     sd Intercept animal           LogitLapse  0      (vectorized)
## (flat)                    b                             LogitMean              default
## normal(0,3)               b Intercept                   LogitMean              default
## student_t(3, 0, 2.5)     sd                             LogitMean  0           default
## student_t(3, 0, 2.5)     sd           animal            LogitMean  0      (vectorized)
## student_t(3, 0, 2.5)     sd Intercept animal            LogitMean  0      (vectorized)


# Fit model ---------------------------------------------------------------
#!be prepared to wait!
#The Markov Chain Monte-Carlo method used for Bayesian estimation requires
#a long series of iterations, between which the algorithm attempts to improve the fit.
#This process takes longer the more iterations, parameters and data are used.

# . Short dummy run to check the influence of the priors ------------------


#double check that the prior distribution is viable by first setting up a short dummy run
# Dummy run
system.time(
  {
    dummy_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = prior_nl, # our priors 
                     sample_prior = 'only', #ignore the data to check the influence of the priors
                     iter = 300, # short run for 300 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <60s, each chain running for <1 seconds (mainly compile time)

#the default plot shows the values estimated for each parameter
# in each chain for each iteration
#fixed effects
plot(dummy_fit, 
     N = 10,
     variable = "^b_", 
     regex = TRUE)

#random effects
plot(dummy_fit, 
     N = 10,
     variable = "^sd", 
     regex = TRUE )

# We can see that for this formula with the priors we've set 
# most parameters would eventually arrive at a value of zero.
# Because we didn't add any information from the data we
# see only the values the priors expect.


# As a sanity test, we can see that the priors alone
# are not biasing estimates away from the range of 
# plausible models. 
# We can see that for this formula there is an expectation 
# that proportion correct increases as a function of the stimulus
# but it is still possible to observe wide range of correct choice rates
# for all stimulus levels.
#as we intended, the combined prior distribution allows for a range of curve shapes
plot(
  conditional_effects(x = dummy_fit, 
                     spaghetti = TRUE, 
                     ndraws = 2e2,
                     effects = 'stimulus')
)

#and effects of stimulus type (mean correct choice rates just slightly above baseline)
plot(
  conditional_effects(x = dummy_fit, 
                      effects = 'type')
)
# For stimulus types, there is no expected difference, though the expected range
# is wider for beta since its coefficient is added to whatever alpha is.
# Generally, the priors accept most plausible models.

# N.B. We specified " sample_prior = 'only' " for this fit. We therefore see
# _only_ the effects of our prior expectations on the fitted model, but _no_
# influence from the data. These estimates should fit with our expectations,
# but should not limit the range of possible interpretations of the data by
# limiting the range of possible estimates to a narrow set of predictions.
# See: github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations for more details.

# . To check that estimation works  ---------------------------------------
# Run the model for a small number of iterations to check that it is possible to 
# estimate all parameters. This may be the point where we encounter numerical errors
# if the formula or priors are misspecified.
# e.g. if the formula returns estimates of correct choice rate outside of [0,1].

# Short run
system.time(
  {
    short_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = prior_nl, # our priors 
                     iter = 300, # short run for 300 iterations (less than 300 gives insufficient warmup time)
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <3 minutes, each chain running for <90 seconds
#!Pay attention to any warning messages! A fix may be just one web search away.

#inspect
#fixed effects
plot(short_fit, 
     N = 10,
     variable = "^b_", 
     regex = TRUE)

#random effects
plot(short_fit, 
     N = 10,
     variable = "^sd", 
     regex = TRUE )


# . Full runs -------------------------------------------------------------


# Full run
system.time(
  {
    full_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = prior_nl, # our priors 
                     iter = 1000, # long run for 1000 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <8 min, each chain running for 4-6 minutes

# null model
system.time(
  {
    null_fit = brm( formula = formulaNull_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = priorNull_nl, # our priors 
                    iter = 1000, # long run for 1000 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <2 min, each chain running for 50-80 seconds


# . . Inspect the fits ----------------------------------------------------
#fixed effects
plot(full_fit, 
     N = 10,
     variable = "^b_", 
     regex = TRUE)

#random effects
plot(full_fit, 
     N = 10,
     variable = "^sd", 
     regex = TRUE )

#summary of parameter estimates
full_sm = summary(full_fit,
                  robust = TRUE)#use the median estimate
#"r_hat" values tell us how well the 4 chains arrived at stable, overlapping estimates
#for each parameter. Values should be close to 1.00, and not >1.10.
# 
## Group-Level Effects: 
##   ~animal (Number of levels: 10) 
##                                                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(LogitLapse_Intercept)                          0.88      0.47     0.09     2.44 1.01      469      372
## sd(LogitLapse_typebeta)                           1.02      0.97     0.05     4.30 1.00     1255      848
## sd(Inflex_Intercept)                              1.49      0.41     0.88     2.65 1.00      815     1052
## sd(Inflex_typebeta)                               0.72      0.45     0.05     1.88 1.01      550      471
## sd(LogWidth_Intercept)                            0.53      0.37     0.04     1.50 1.02      255      630
## sd(LogWidth_typebeta)                             0.71      0.53     0.05     1.93 1.01      261      749
## cor(LogitLapse_Intercept,LogitLapse_typebeta)    -0.11      0.76    -0.96     0.93 1.00     2900     1362
## cor(Inflex_Intercept,Inflex_typebeta)            -0.72      0.31    -0.99     0.63 1.01     1272     1149
## cor(LogWidth_Intercept,LogWidth_typebeta)        -0.81      0.24    -1.00     0.84 1.01      356      791
## 
## Population-Level Effects: 
##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Base_Intercept           0.48      0.03     0.39     0.53 1.00      582      888
## LogitLapse_Intercept    -2.68      0.67    -5.63    -1.76 1.01      362      558
## LogitLapse_typebeta     -2.23      2.10    -7.18     0.81 1.00     1434      934
## Inflex_Intercept         2.60      0.58     1.43     3.83 1.02      467      608
## Inflex_typebeta          2.54      0.55     1.38     3.57 1.01      718      986
## LogWidth_Intercept       1.18      0.55     0.05     2.16 1.01      300      773
## LogWidth_typebeta        0.89      0.51    -0.11     2.02 1.01      419      917

#Estimate is the median of all post-warmup estimates (draws) from all chains.
#The values "l-95% CI" and "u-95% CI" are the edges of the 95% "credible interval"
#the interval containing 95% of post-warmup estimates, and therefore the most plausible values.

# . . Check parameter estimates -----------------------------------------------

#We can also check if these estimates match with our expectations.
#Extract fixed effects estimates
full_fix = full_sm$fixed
#extract rownames
full_fix_rn = rownames(full_fix)
#The estimate for baseline is very similar to our input value of 0.5
round(
  with(full_fix[full_fix_rn %in% 'Base_Intercept', ],
     {c(median = Estimate,
         percentile_2.5 = `l-95% CI`,
          percentile_97.5 = `u-95% CI`)
  }),digits = 2)
##median  percentile_2.5 percentile_97.5 
## 0.48            0.39            0.53
# the model has searched at lower values of baseline than we might reasonably expect
# unless animals have a strong bias to incorrect responses
# we might get more accurate estimates using a more restictive prior (e.g. beta(250, 250))

#The estimate for lapse rate (logit scale) is a little lower than our input value of 0.12
round(
plogis(q = 
         with(full_fix[full_fix_rn %in% 'LogitLapse_Intercept', ],
     {c(median = Estimate,
         percentile_2.5 = `l-95% CI`,
          percentile_97.5 = `u-95% CI`)
  })
),digits = 2)
## median  percentile_2.5 percentile_97.5 
## 0.06            0.00            0.15
# in our dataset many animals scored 100% for some conditions by chance 
# if we performed more trials with each animal, we might obtain a more accurate
# estimate of lapse rate

#The estimate for width (log scale) is close to our input value of 3.00
round(
exp(x =  
         with(full_fix[full_fix_rn %in% 'LogWidth_Intercept', ],
     {c(median = Estimate,
         percentile_2.5 = `l-95% CI`,
          percentile_97.5 = `u-95% CI`)
  })
),digits = 2)
## median  percentile_2.5 percentile_97.5 
## 3.26            1.05            8.63 

#The estimate for inflection point is close to our input value of 2.00
round( 
     with(full_fix[full_fix_rn %in% 'Inflex_Intercept', ],
     {c(median = Estimate,
         percentile_2.5 = `l-95% CI`,
          percentile_97.5 = `u-95% CI`)
      }),
     digits = 2)
## median  percentile_2.5 percentile_97.5 
## 2.60            1.43            3.83  

#importantly, the estimates of the effect of stimulus type are also accurate
#The estimate for effect of type on inflection point is close to our input value of 2.50
round( 
  with(full_fix[full_fix_rn %in% 'Inflex_typebeta', ],
       {c(median = Estimate,
          percentile_2.5 = `l-95% CI`,
          percentile_97.5 = `u-95% CI`)
       }),
  digits = 2)
## median  percentile_2.5 percentile_97.5 
## 2.54            1.38            3.57 

#The estimate for effect of stimulus width (log scale) is not far from our 
# input value of 3.5, or on a log scale: log(3.5) = 1.25
#but also includes a very wide range
round(
        with(full_fix[full_fix_rn %in% 'LogWidth_typebeta', ],
             {c(median = Estimate,
                percentile_2.5 = `l-95% CI`,
                percentile_97.5 = `u-95% CI`)
             }),
  digits = 2)
## median  percentile_2.5 percentile_97.5 
## 0.89           -0.11            2.02
#but note that this 95%CI includes _zero_ ( -0.11 < 0 <2.02 )
# with this particular dataset, we cannot be confident that
# there is an effect of stimulus type on width. For stimulus type beta, the
# inflection point (pop_inflex: 2 + type_inflex: 2.5 = 4.5) is within half a width 
# ( (3 + 3.5) /2 = 3.25: + 4.5 = 7.75) of the maximum stimulus value.
# As a result, the top part of rise region is not observed, so there is not 
# enough data in the relevant region to estimate this effect without 
# considerable uncertainty.
# For real data, we could use our estimates of these parameters to propose
# stimulus levels to add to the dataset to be able to measure this effect.
full_estimates = full_fix$Estimate
names(full_estimates) = full_fix_rn
round(
  with(data.frame(rbind(full_estimates)), 
       {
         Inflex_Intercept + Inflex_typebeta + 
           (exp(LogWidth_Intercept + LogWidth_typebeta)/2)
       }
       ),
  digits = 2
)
## 9.04
#stimulus intensity should go to 9 to observe 80% of maximum performance for type beta



# Model comparison --------------------------------------------------------

#How robust is the model to changes in the data structure?
#Would the model make good predictions refitted the model but dropped one datapoint, would it still give good predictions?
# calculate the Leave-One-Out (LOO) cross validation metric for the model
loo_full = loo(full_fit)#calculate for full model 
loo_null = loo(null_fit)#calculate for null model

loo_compare(loo_full,
            loo_null)
##         elpd_diff se_diff
## full_fit    0.0       0.0 
## null_fit -127.6      14.8 
# Expected log predictive density (ELPD) is higher for the full fit than for the
# null model, and the difference is larger than both the standard error of the
# ELPD and the difference in the number of parameters.


# Plot model predictions --------------------------------------------------

# We can get a general idea of the shape of the fitted curve by looking at a
# subsample (here only 200) of the draws for stimulus.
plot(
  conditional_effects(x = full_fit, 
                      spaghetti = TRUE, 
                      ndraws = 2e2,
                      effects = 'stimulus')
)

# to predict all effects, we can use the 'posterior_epred' method
#by default 100 predictions per continuous variable (but fewer for their interactions)
system.time(
  {
    full_cond =brms::conditional_effects(full_fit, 
                                         method = 'posterior_epred', # posterior epred not working
                                         cores =  parallel::detectCores()-1,
                                         effects = c('stimulus:type')
                                        )
  }
)#takes <2 seconds

pred_data = full_cond$`stimulus:type`
#plot each stimulus type
plot(
  NULL,
  xlab = 'Stimulus intensity',
  ylab = 'Proportion correct',
  xlim = range(stimulus) * c(0.7, 1.3),
  ylim = c(0,1),
)
abline(h = c(0,1,base, 1- lapse), 
       lty = c(1,1,3,3))
for (aa in agg$animal)
  with(subset(agg, animal == aa),
       #find the subset of the data with this animal
       {
         #Run code using this data as an environment
         points(
           x = stimulus,
           y = correct_incorrect,
           col = c('blue', 'orange3')[1 + type %in% levels(type)[2]],
           #colour for this animal
           pch = c(20, 21)[1 + type %in% levels(type)[2]]
           # a dot, filled or open
         )
       })

#plot total prediction intervals
with(subset(pred_data, type == 'beta'), 
     {
       polygon(x = c(sort(stimulus), rev(sort(stimulus))), 
               y = c(lower__[order(stimulus)],
                     rev(upper__[order(stimulus)])
               ), 
               col = adjustcolor('orange', alpha.f = 50/256),
               border = NA,
               lwd = 0.1
       )
     }
)
with(subset(pred_data, type == 'alpha'), 
     {
       polygon(x = c(sort(stimulus), rev(sort(stimulus))), 
               y = c(lower__[order(stimulus)],
                     rev(upper__[order(stimulus)])
               ), 
               col = adjustcolor('darkblue', alpha.f = 50/256),
               border = NA,
               lwd = 0.1
       )
     }
)
#plot the median prediction lines
with(subset(pred_data, type == 'alpha'), 
     lines(x = sort(stimulus), y = estimate__[order(stimulus)], 
           col = 'darkblue',
           lwd = 5)
)
with(subset(pred_data, type == 'beta'), 
     lines(x = sort(stimulus), y = estimate__[order(stimulus)], 
           col = 'orange',
           lwd = 5)
)
abline(v = full_fix[full_fix_rn %in% 'Inflex_Intercept', 'Estimate'] + 
             c(0, full_fix[full_fix_rn %in% 'Inflex_typebeta', 'Estimate'] ),
       col = c('darkblue', 'orange'),
       lty = 3
       )

legend(x = 'bottomright',
       legend = unique(agg$type),
       col = c('blue', 'orange3'),
       pch = c(20, 21))


# Plot comparison with simulation parameters ------------------------------


# . Curve predictions -----------------------------------------------------

#plot each stimulus type
plot(
  NULL,
  xlab = 'Stimulus intensity',
  ylab = 'Proportion correct',
  xlim = range(stimulus) * c(0.7, 1.3),
  ylim = c(0,1),
)
abline(h = c(0,1,base, 1- lapse), 
       lty = c(1,1,3,3))

#plot total prediction intervals
with(subset(pred_data, type == 'beta'), 
     {
       polygon(x = c(sort(stimulus), rev(sort(stimulus))), 
               y = c(lower__[order(stimulus)],
                     rev(upper__[order(stimulus)])
               ), 
               col = adjustcolor('orange', alpha.f = 50/256),
               border = NA,
               lwd = 0.1
       )
     }
)
with(subset(pred_data, type == 'alpha'), 
     {
       polygon(x = c(sort(stimulus), rev(sort(stimulus))), 
               y = c(lower__[order(stimulus)],
                     rev(upper__[order(stimulus)])
               ), 
               col = adjustcolor('darkblue', alpha.f = 50/256),
               border = NA,
               lwd = 0.1
       )
     }
)
#plot the median prediction lines
with(subset(pred_data, type == 'alpha'), 
     lines(x = sort(stimulus), y = estimate__[order(stimulus)], 
           col = 'darkblue',
           lwd = 5)
)
with(subset(pred_data, type == 'beta'), 
     lines(x = sort(stimulus), y = estimate__[order(stimulus)], 
           col = 'orange',
           lwd = 5)
)


lines(x = xx,
     y = base + (1 - lapse - base) * 
       plogis(q = 
                width_coef* (xx - pop_inflex) / pop_width
       ),
     col = 'blue',
     lwd = 5, 
     lty = 3
)

lines(x = xx,
     y = base + (1 - lapse - base) * 
       plogis(q = 
                width_coef* (xx - (pop_inflex + type_inflex)) / (pop_width+type_width)
       ),
     col = 'orange3',
     lwd = 5, 
     lty = 3
)


# . Parameter predictions -------------------------------------------------
#WORK IN PROGRESS
full_draws = prepare_predictions(full_fit)
full_pars = full_draws$nlpars

#plot each stimulus type
plot(
  NULL,
  xlab = 'Stimulus intensity',
  ylab = 'Proportion correct',
  xlim = range(stimulus) * c(0.7, 1.3),
  ylim = c(0,1),
)
abline(h = c(0,1,base, 1- lapse), 
       lty = c(1,1,3,3))

abline(v = full_fix[full_fix_rn %in% 'Inflex_Intercept', 'Estimate'] + 
         c(0, full_fix[full_fix_rn %in% 'Inflex_typebeta', 'Estimate'] ),
       col = c('darkblue', 'orange'),
       lty = 3
)
with(full_fix[full_fix_rn %in% 'Inflex_Intercept',],
     {
arrows(x0 = `l-95% CI`,
       x1 = `u-95% CI`,
       y0 = (full_fix[full_fix_rn %in% 'Base_Intercept', 'Estimate'] + 
         (1 - full_fix[full_fix_rn %in% 'Base_Intercept', 'Estimate'] -
            - plogis(full_fix[full_fix_rn %in% 'LogitLapse_Intercept', 'Estimate']) )
       )/2,
       code = 3,
       angle = 90,
       length = 0.1,
       col = c('darkblue'),
       lwd = 3
)
})
with(full_fix[full_fix_rn %in% 'Inflex_Intercept',],
     {
arrows(x0 = `l-95% CI` + full_fix[full_fix_rn %in% 'Inflex_typebeta','l-95% CI'],
       x1 = `u-95% CI` + full_fix[full_fix_rn %in% 'Inflex_typebeta','u-95% CI'],
       y0 = (full_fix[full_fix_rn %in% 'Base_Intercept', 'Estimate'] + 
         (1 - full_fix[full_fix_rn %in% 'Base_Intercept', 'Estimate'] -
            - plogis(full_fix[full_fix_rn %in% 'LogitLapse_Intercept', 'Estimate'] +
                       full_fix[full_fix_rn %in% 'LogitLapse_typebeta', 'Estimate']) )
       )/2,
       code = 3,
       angle = 90,
       length = 0.1,
       col = c('orange'),
       lwd = 3
)
})
