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
#check the operating system and assign a logical flag (TRUE or FALSE)
sys_win = Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if (sys_win) {
  #get rid of all the backslashes
  root_dir <-
    gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
} else{
  #Root directory should be the "HOME" directory on a Mac (or Linux?)
  root_dir <- Sys.getenv('HOME')#Life was easier on Mac
}
file_name = 'simulated_data.csv'
#write this data to an Excel-compatible "comma separated values" file
write.csv(
  file = file.path(root_dir,
                   'Documents', #assume the user has a folder called 'Documents' in the root environment
                   file_name),
  x = sim_data,
  row.names = F
)
#Now read in this file, or another file in the same format
dta = read.csv(file = file.path(root_dir,
                                'Documents', #assume the user has a folder called 'Documents' in the root environment
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
  pch = 20
)
legend(x = 'bottomright',
       legend = unique(dta$type),
       pch = c(20, 21))

             
             
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
#Maximal model with random intercepts for individuals
mixmod.max = lmer(formula = response_y ~
                    stimulus * type +
                    (1 + stimulus * type | animal),
                  data = dta)
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
#If AIC is lower, then the maximal model fits
# -4706.072 < 22160.38

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

# Inspect model -----------------------------------------------------------
require(lmerTest)
mixmod.max_tests = lmerTest::as_lmerModLmerTest(mixmod.max)
summary(mixmod.max_tests)
## Fixed effects:
##                      Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)          5.082471   0.296562  9.005755  17.138 3.50e-08 ***
##   stimulus           2.979613   0.280295  9.003568  10.630 2.14e-06 ***
##   typebeta           2.490391   0.006981 52.344033 356.720  < 2e-16 ***
##   stimulus:typebeta  3.513642   0.539917  9.009061   6.508  0.00011 ***

#significant effects of each variable
#the stimulus type explains the most variation
#and therefore has the largest t value

# Post-hoc comparisons ----------------------------------------------------
#Load the package for post-hoc comparisons
require(emmeans)
require(pbkrtest)# Used for calculating DF in mixed-effects model
#perform a general linear hypothesis tests on type
ht1 = emmeans(
  object = mixmod.max,
  spec = pairwise ~ type,
  #compare the effects of different types
  adjust = 'sidak')
#multiple comparisons with Sidak's correction
#Perform the post hoc tests between these means using the test() function
# from the 'emmeans' package (and not any other function called 'test()').
ht1_ph = emmeans::test(object = ht1)
# print the "contrast" (comparison) part of the test
print(ht1_ph$contrasts)
## contrast     estimate   SE df t.ratio p.value
## alpha - beta      -13 1.62  9 -8.044  <.0001
##
## Degrees-of-freedom method: kenward-roger
#After accounting for effects of stimulus strength, its interaction with type,
# and all random effects of individual animals, responses to stimulus
# alpha were 13.0 weaker than for stimlus beta, t(9.0) = -8.04, p <0.0001.
#N.B. for a precise, two.tailed estimate of p(>|t|), try
# signif(
#     pt(
#       q = ht1_ph$contrasts$t.ratio,
#       df = 9,
#       lower.tail = TRUE
#     ) * 2,
#     digits =  3
#   )
                           