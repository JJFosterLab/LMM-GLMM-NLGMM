# Load packages required for mixed-effects modelling ----------------------
require(lme4)
# Fit the GENERALISED linear model ----------------------------------------
#Maximal model with random intercepts for individuals
glmm.max<-glmer(formula = correct_incorrect~factor1*factor2 + (1|individual),
                 data = dta,
                 family = binomial(link = 'logit')
)
#Null model, with only random effects
glmm.null<-glmer(formula = correct_incorrect~1 + (1|individual),
                   data = dta,
                   family = binomial(link = 'logit')
)
# Model comparison --------------------------------------------------------
#To prove any fixed effects, the mixed-effects model has to 
#describe the data better than the random effects model alone.
extractAIC(glmm.max)
extractAIC(glmm.null)
#If AIC is lower, then the maximal model fits
#We can also perform a likelihood ratio test, confusingly called "anova"
anova(glmm.max, glmm.null)
#This gives the same answer as the AIC, but the difference can be reported with
#a test statistic and p-value
