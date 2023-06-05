# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2023 06 02
#     MODIFIED:	James Foster              DATE: 2023 06 05
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
width_coef = 2*log(2/(1-width_alpha)-1) # coeficient to rescale curve to width
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
extra_error_sd = 0.10#small source of unnaccounted error

#Plot ideal curve for stimulus type 1
xx = seq(from = 1,
         to  = n_levels,
         length.out = 1e3)
plot(x = xx,
    y = base + (1 - lapse - base) * 
              plogis(q = 
                width_coef* (xx - pop_inflex) / pop_width
                ),
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
install.packages('brms')

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
  #Most of these are subject to futher fixed (type) and random (animal) effects,
  #these need to be defined for each parameter.
  
  #Two parameters need special transformations
  #To keep the output between 0 and 1, additional effects of lapse rate
  #will be added on the "logit" scale (Lapse = inv_logit(LogitLapse)).
  #To avoid curve widths of 0, we can assume a positive slope (≥0)
  #and add additional effects to width on a log scale (Width = exp(LogWidth))
  formula = correct_incorrect ~ 
    Base + (1 - inv_logit(LogitLapse) - Base) *#curve region
    inv_logit( 4.39*(stimulus - Inflex) / exp(LogWidth) ) , #inflection-width curve
  Base ~ 1, #Base rate of correct choices
  LogitLapse ~ 1 + type + (1|animal), #Lapse rate on a log(odds) scale
  LogWidth ~ 1 + type + (1|animal), #log 80% width of the curve
  Inflex ~ 1 + type + (1|animal), #inflection point of the initial curve
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
  LogitMean ~ 1 + (1|animal), #mean log(odds) of a correct choice
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
prior_nl = get_prior(formula = formula_nl,
                     data = dta)
##  prior                  class  coef  group resp dpar      nlpar lb ub       source
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
#for the baseline, we will use a beta distribution with a strong bias towards 0.5
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                nlpar %in% 'Base' &
                coef %in% 'Intercept' 
                }), "prior"] = 'beta(20,20)' #a beta distribution centred on 0.5
#this prior is atuomatically bounded between 0 and 1, 
# but we may want to set additional upper and lower bounds 
#lower bound
prior_nl[with(prior_nl, 
              { #class %in% 'b' & #just the fixed effects
                nlpar %in% 'Base' #&
                # coef %in% 'Intercept' 
                }), "lb"] = 0 #baseline should not be less than 0
#upper bound
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                nlpar %in% 'Base' #&
                # coef %in% 'Intercept' 
                }), "ub"] = 0.75 #if we expect lapse rates of 5-10%, baseline should not reach this range

# . . Lapse rate priors ---------------------------------------------------
#for the lapse rates, we will use a normal distribution with a strong bias 
# towards log(odds) = -3, prob ≈0.05
# (see qlogis(0.05) for conversion from prob to log odds)
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                  nlpar %in% 'LogitLapse' &
                  coef %in% 'Intercept' 
              }), "prior"] = 'normal(-3,3)' #a normal distribution centred on -3
#for all other fixed effects on lapse rate, we'll suggest values around 0 (no effect)
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                  nlpar %in% 'LogitLapse' &
                  coef %in% 'typebeta' 
              }), "prior"] = 'normal(0,3)' #a normal distribution:mean 0, sd 3

# . . Inflection point priors ---------------------------------------------
#for the population level inflection point, we expect it to be somewhere
#in the middle of our stimulus range (0-6) = 3
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                nlpar %in% 'Inflex' &
                coef %in% 'Intercept'
                }), "prior"] = 'normal(3,3)' #a normal distribution:mean 3, sd 3
#for all coefficients, we'll suggest values around 0 (no effect)
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                nlpar %in% 'Inflex' &
                coef %in% 'typebeta' 
                }), "prior"] = 'normal(0,3)' #a normal distribution:mean 0, sd 3

# . . Rise region width priors --------------------------------------------
#for the population level width, we expect it to be a positive value (>exp(-Inf))
#somewhat smaller than the range of stimulus values
#it might be reasonable to also expect an 80% rise region of around 3 ≈ exp(1)
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                nlpar %in% 'LogWidth' &
                coef %in% 'Intercept' 
                }), "prior"] = 'normal(1,3)' #a normal distribution:mean 3, sd 3
#for all coefficients, we'll suggest values around 0 (no effect)
prior_nl[with(prior_nl, 
              { class %in% 'b' & #just the fixed effects
                nlpar %in% 'LogWidth' &
                coef %in% 'typebeta'
                }), "prior"] = 'normal(0,3)' #a normal distribution:mean 0, sd 3

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
priorNull_nl[with(priorNull_nl, 
              { class %in% 'b' & #just the fixed effects
                  nlpar %in% 'Base' &
                  coef %in% 'Intercept' 
              }), "prior"] = 'beta(20,20)' #a beta distribution centred on 0.5
#this prior is atuomatically bounded between 0 and 1, 
# but we may want to set additional upper and lower bounds 
#lower bound
priorNull_nl[with(priorNull_nl, 
              { #class %in% 'b' & #just the fixed effects
                nlpar %in% 'Base' #&
                # coef %in% 'Intercept' 
              }), "lb"] = 0 #baseline should not be less than 0
#upper bound
priorNull_nl[with(priorNull_nl, 
              { class %in% 'b' & #just the fixed effects
                  nlpar %in% 'Base' #&
                # coef %in% 'Intercept' 
              }), "ub"] = 0.75 #if we expect lapse rates of 5-10%, baseline should not reach this range

# . . Lapse rate priors ---------------------------------------------------
#for the lapse rates, we will use a normal distribution with a strong bias 
# towards log(odds) = -3, prob ≈0.05
# (see qlogis(0.05) for conversion from prob to log odds)
priorNull_nl[with(priorNull_nl, 
              { class %in% 'b' & #just the fixed effects
                  nlpar %in% 'LogitLapse' &
                  coef %in% 'Intercept' 
              }), "prior"] = 'normal(-3,3)' #a normal distribution centred on -3
#for all other fixed effects on lapse rate, we'll suggest values around 0 (no effect)
priorNull_nl[with(priorNull_nl, 
              { class %in% 'b' & #just the fixed effects
                  nlpar %in% 'LogitLapse' &
                  coef %in% 'typebeta' 
              }), "prior"] = 'normal(0,3)' #a normal distribution:mean 0, sd 3

# . . Mean rate priors ---------------------------------------------------
#for the mean rates, we will use a normal distribution with a weak bias 
# towards log(odds) = 0, prob  = 0.5
# (see qlogis(0.5) for conversion from prob to log odds)
priorNull_nl[with(priorNull_nl, 
                  { class %in% 'b' & #just the fixed effects
                      nlpar %in% 'LogitMean' &
                      coef %in% 'Intercept' 
                  }), "prior"] = 'normal(0,3)' #a normal distribution centred on 0


# Fit model ---------------------------------------------------------------
#double check that the model is viable by first setting up a short dummy run
# Short run
system.time(
  {
    dummy_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = prior_nl, # our priors 
                     sample_prior = 'only', #ignore the data to check the influence of the priors
                     iter = 500, # short run for 500 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <30s, each chain running for <1 second

#the default plot shows the values estimated for each parameter
# in each chain for each iteration
plot(dummy_fit)
# This will need at least
# 2 plot windows

# We can see that for this formula with the priors we've set 
# most parameters would eventually arrive at a value of zero.
# Because we didn't add any information from the data we
# see only the values the priors expect.

dummy_cond = conditional_effects(x = dummy_fit)
plot(dummy_cond, ask = FALSE)
# This will need at least
# 2 plot windows

# As a sanity test, we can see that the priors alone
# are not biasing estimates away from the range of 
# plausible models. 
# We can see that for this formula there is an expectation 
# that proportion correct increases as a function of the stimulus
# but it is still possible to observe wide range of correct choice rates
# for all stimulus levels.
# For stimulus types, there is no expected difference, though the expected range
# is wider for beta since its coefficient is added to whatever alpha is.
# Generally, the priors accept most plausible models.


# Short run
system.time(
  {
    short_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = prior_nl, # our priors 
                     iter = 500, # short run for 500 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <60s, each chain running for <2 seconds
plot(short_fit)

# Full run
system.time(
  {
    full_fit = brm( formula = formula_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = prior_nl, # our priors 
                     iter = 2000, # long run for 2000 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <8 min, each chain running for <3 minutes
plot(full_fit)
summary(full_fit)

cond_full = conditional_effects(x = full_fit)
plot(cond_full, points = TRUE, point_args =  list(col = gray(0.7, 0.5)) )
#
conditional_effects(x = full_fit, spaghetti = TRUE, ndraws = 2e3)
#

# null model
system.time(
  {
    null_fit = brm( formula = formulaNull_nl, # using our nonlinear formula
                     data = dta, # our data
                     prior = priorNull_nl, # our priors 
                     iter = 500, # short run for 500 iterations
                     chains = 4, # 4 chains in parallel
                     cores = 4, # on 4 CPUs
                     refresh = 0, # don't echo chain progress
                     backend = 'cmdstanr') # use cmdstanr (other compilers broken)
  }
)
# On my computer this takes <60s, each chain running for <20 seconds
plot(null_fit)


# Model comparison --------------------------------------------------------

#How robust is the model to changes in the data structure?
#Would the model make good predictions refitted the model but dropped one datapoint, would it still give good predictions?
# calculate the Leave-One-Out (LOO) cross validation metric for the model
loo_short = loo(short_fit)
loo_null = loo(null_fit)

loo_compare(loo_short,
            loo_null)


