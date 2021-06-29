# Basic Mixed Model Script ------------------------------------------------

#  Welcome to R!
#	Statements preceded by "#" are comments,
#	"<-" commands assign the variable to the left with the value to the right
#	"()" contain the targets of functions
#	"{}" contain conditional statements or loops
#	"~" specifies a relationship in a model formula


####################################################################
#	Read in Data													#
####################################################################
setwd(paste0(Sys.getenv('HOME'), '/Documents/'))#set to your data folder
dta <- read.csv('experiment1.csv', header = TRUE, sep = ";", row.names = NULL)


####################################################################
#	Fit the most complicated possible model							#
####################################################################
# You know a lot of factors that might have affected your data
# and it is a good idea to check if all (or any) did.
# You do this by fitting a model that has an effect of all factors
# and their interactions (the effect of specific combinations).
# The thing that you measured in your experiment is a
# "response" variable (it responds to the other factors),
# whereas the other factors are "predictor" variables
# (they might help you predict the response variable).
# These are "fixed" effects and their model is:
# response ~ predictor
# , which tells us that we can do something to the predictor
# to guess the response.

# Because individuals have an inconsistent effect on the response
# variable they are considered a "random" factor and are entered
# into the model in a different way:
# response ~ (1|random_factor)
# is the most common.

# Mixing fixed and random effects gives us the model including 
# everything that might have an effect.

# Load packages required for mixed-effects modelling ----------------------
require(lme4)

# Fit the model -----------------------------------------------------------
#Maximal model with random intercepts for individuals
mixmod.max<-lmer(formula = response~factor1*factor2 + (1|individual),
                 data = dta
                 )
#Null model, with only random effects
mixmod1.null<-lmer(formula = response~1 + (1|individual),
                   data = dta
                   )

# Model comparison --------------------------------------------------------
#To prove any fixed effects, the mixed-effects model has to 
#describe the data better than the random effects model alone.
extractAIC(mixmod1.max)
extractAIC(mixmod1.null)
#If AIC is lower, then the maximal model fits

#We can also perform a likelihood ratio test, confusingly called "anova"
anova(mixmod1.max, mixmod1.null)
#This gives the same answer as the AIC, but the difference can be reported with
#a test statistic and p-value

# Post-hoc comparisons ----------------------------------------------------
#Load the package for multiple comparisons
require(multcomp)
#perform a general linear hypothesis tests on factor1
ht1 <- glht(model = mixmod1.max, 
            linfct = mcp(factor1 = "Tukey")#multiple comparisons with Tukey's correction
            )
#View a summary (with statistics and p-values)
summary(ht1)#perform a general linear hypothesis tests on factor2
ht2 <- glht(model = mixmod1.max, 
            linfct = mcp(factor2 = "Tukey")#multiple comparisons with Tukey's correction
            )
#View a summary (with statistics and p-values)
summary(ht2)
#for their interaction
ht.1.2 <- glht(model = mixmod1.max, 
            linfct = model.matrix(mixmod.max)#all comparisons
            )
#View a summary (with statistics and p-values)
summary(ht.1.2)
