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
replicates = 10#number of replicates of each condition

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
             times = n_levels
             )
animal = sort(animal)# vector of animal numbers organised in order
animal = rep(x = animal,
             times = n_types
             )
stimulus = rep(x = 1:n_levels-1,
            times = n_animals
            )
stimulus = rep(x = stimulus,
               times = n_types
               )
stype = rep(x = 1:n_types,
            times = n_animals*n_levels
          )
stype = sort(stype)

#replicate each observation
animal = rep(x = animal,
             times = replicates
)
stimulus = rep(x = stimulus,
             times = replicates
)
stype = rep(x = stype,
             times = replicates
)

#baseline bias (response when stimulus = 0 and type = 1)
base_bias_animal = rnorm(n = n_animals, 
                        mean  = pop_mean,
                        sd = pop_sd
                        )
#response increase with each stimulus level
slope_animal = rnorm(n = n_animals,
                     mean = slope_mean,
                     sd = slope_sd
                     )
#response to type 2
type_animal = rnorm(n = n_animals,
                     mean = 1.0, # on average, no random effect
                     sd = type_animal_sd
                     )

#Generate data
#Make an empty vector
response_y = rep(x = NA,
                 length = n_animals * n_levels * n_types *replicates
                 )
#Loop through experiment design and generate theoretical response
for(ii in 1:length(response_y))
{
  #each data point is generated from the animal's intercept
  #plus the effect of stimulus type
  #plus the animal's response (slope) to changing stimulus level
  #scaled by the effect of different stimlus types
  #plus a small additional source or error
  response_y[ii] = base_bias_animal[animal[ii]] +
                (stype[ii]-1) * type_mean + 
                  (slope_animal[animal[ii]] + 
                     (stype[ii]-1) * type_slope * type_animal[animal[ii]]
                   ) * stimulus[ii] +
                  rnorm(n = 1,
                        mean = 0,
                        sd = extra_error_sd
                        )
}

# Combine this into a single data.frame format object,
# with the experimental design and the measured response data
sim_data = data.frame(response_y = response_y,
                      animal = LETTERS[animal],#animal names will now be capitcal letters
                      stimulus = stimulus,
                      type = factor(stype, 
                                    labels = c('alpha',
                                                'beta'
                                               )
                                    )
                      )
View(sim_data)


# Write/Read data ---------------------------------------------------------
#check the operating system and assign a logical flag (TRUE or FALSE)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  root_dir <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  root_dir <- Sys.getenv('HOME')#Life was easier on Mac
}
file_name = 'simuluated_data.csv'
#write this data to an Excel-compatible "comma separated values" file
write.csv(file = file.path(root_dir,
                           'Documents',#assume the user has a folder called 'Documents' in the root environment
                           file_name
                           ),
          x = sim_data,
          row.names = F
          )
#Now read in this file, or another file in the same format
dta = read.csv(file = file.path(root_dir,
                           'Documents',#assume the user has a folder called 'Documents' in the root environment
                           file_name
                           ),
               header = T
              )
#format categorical variables as "factors"
dta = within(dta,
             {#Run code within this data frame, using it as an environment
               animal = factor(animal)
               type = factor(type)
             }
             )

# Plot data ---------------------------------------------------------------
#Set up plot colours
clz <- sapply(
  length(unique(dta$animal)),
  colorRampPalette(
    c('red','blue','cyan4','magenta4','orange')#on Rstudio these are distinct
  )
)
clz <- sample(clz)#Randomise order?

#general overview

#open an empty plot
plot(NULL,
     xlab = 'Stimulus intensity',
     ylab = 'Response strength',
     xlim = range(stimulus)*c(0.7,1.3),
     ylim = range(response_y)*c(0.7,1.3),
     )
for(aa in dta$animal)
  with(subset(dta, animal == aa),#find the subset of the data with this animal
       {#Run code using this data as an environment
        points(x = stimulus, 
               y = response_y,
               col = clz[which(LETTERS %in% aa)],#colour for this animal
               pch = c(20,21)[1+ type %in% levels(type)[2]] # a dot, filled or open
              )
       }
  )
legend(x = 'topright',
       legend = unique(dta$animal),
       col = clz,
       pch = 20
       )
legend(x = 'bottomright',
       legend = unique(dta$type),
       pch = c(20,21)
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
#Maximal model with random intercepts for individuals
mixmod.max<-lmer(formula = response_y ~
                                       stimulus*type +
                                       (1+stimulus*type|animal),
                 data = dta
                 )
#boundary (singular) fit: see ?isSingular
#Some random effects are too small to estimate properly, common warning
#Null model, with only random effects
mixmod.null<-lmer(formula = response_y ~
                                       1 + (1|animal),
                   data = dta
                   )

# Model comparison --------------------------------------------------------
#To prove any fixed effects, the mixed-effects model has to 
#describe the data better than the random effects model alone.
extractAIC(mixmod.max)[2]#2nd component is the AIC, 1st is the d.f.
extractAIC(mixmod.null)[2]
#If AIC is lower, then the maximal model fits
# -2167.315 < 11096.12

#We can also perform a likelihood ratio test, confusingly called "anova"
anova(mixmod.max, mixmod.null)
#This gives the same answer as the AIC, but the difference can be reported with
#a test statistic and p-value
#              npar     AIC     BIC  logLik deviance Chisq Df Pr(>Chisq)    
# mixmod.null    3 11096.1 11111.9 -5545.1  11090.1                        
# mixmod.max    15 -2167.3 -2088.7  1098.7  -2197.3 13287 12  < 2.2e-16 ***

#report:
#change in deviance = 13287 
#change in degrees of freedom = 12
# p < 2.2e-16 (the smallest number the computer can think of)

# Inspect model -----------------------------------------------------------
summary(mixmod.max)
# Fixed effects:
#                    Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        5.07837    0.29889  9.00658  16.991 3.77e-08 ***
#   stimulus           2.98131    0.28017  9.00038  10.641 2.13e-06 ***
#   typebeta           2.50359    0.01007 42.90191 248.668  < 2e-16 ***
#   stimulus:typebeta  3.50849    0.54039  9.01924   6.493 0.000111 ***

#significant effects of each variable
#the stimulus type explains the most variation
#and therefore has the largest t value

# Post-hoc comparisons ----------------------------------------------------
#Load the package for multiple comparisons
require(multcomp)
#perform a general linear hypothesis tests on type
ht1 <- glht(model = mixmod.max, 
            linfct = mcp(type = "Tukey")#multiple comparisons with Tukey's correction
            )
#View a summary (with statistics and p-values)
summary(ht1)#perform a general linear hypothesis tests on factor2
