# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2021 06 27
#     MODIFIED:	James Foster              DATE: 2022 06 22
#
#  DESCRIPTION: An example generalised mixed-effect model, using "glmer" from
#               the "lme4" package.
#
#       USAGE:  First install the packages :'lme4', 'report', 'emmeans', 'lmeTest'
#               and 'pbkrtest'  as suggested by Rstudio, or by running
#               install.packages('report'); install.packages('emmeans) etc. ...
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: -
#
#   REFERENCES: Bates et al., (2022) Fitting Linear Mixed-Effects Models Using lme4,
#               https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
#
#TODO
#- Add comments

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
file_name = 'simulated_binomial_data.csv'
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
#aggragate the proportion correct for each animal
agg = aggregate(formula = correct_incorrect~
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
#Maximal model with random intercepts for individuals
glmm.max = glmer(formula = correct_incorrect~
                  stimulus * type +
                  (1 + stimulus * type | animal),
                 data = dta,
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
# 685.7924 < 3590.112
#We can also perform a likelihood ratio test, confusingly called "anova"
anova(glmm.max, glmm.null, test = 'Chisq')
#This gives the same answer as the AIC, but the difference can be reported with
#a test statistic and p-value
#           npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# glmm.null    2 3590.1 3602.0 -1793.1   3586.1
# glmm.max    14  685.8  768.9  -328.9    657.8 2928.3 12  < 2.2e-16 ***

#report:
#change in deviance = 2928.3
#change in degrees of freedom = 12
# p < 2.2e-16 (the smallest number the computer can think of)
