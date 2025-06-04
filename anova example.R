# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 06 16
#     MODIFIED:	James Foster              DATE: 2025 06 04
#
#  DESCRIPTION: An example ANOVA, using the "aov" and "lm" base R functions.
#
#       USAGE:  First install the 'report' and 'emmeans' packages as suggested by
#               Rstudio, or by running install.packages('report'); install.packages('emmeans)
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: - Removed "paired" argument (deprecated)
#
#   REFERENCES: Turner, S. (2017) Essential Statistics with R
#               www.bioconnector.github.io/workshops/r-stats.html#continuous_variables
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


# Set up R session --------------------------------------------------------
require(report) #load a package for easy reporting, needs to be installed: install.packages('report')
set.seed(20220622) #for repeatability, start random number generator at today's date

# Enter data --------------------------------------------------------------
#we will simulate random data for this example

#simulate a random normal distribution for condition A
dt_A = rnorm(n = 10, #sample size of 10
             mean = 1.0, # mean of 1.0
             sd = 0.3) # standard deviation of 0.3
#set up factor labels for condition A
fct_A = rep(x = 'A', #repeat the letter A
            times = length(dt_A)) #enough labels for each entry in the A data

#simulate a random normal distribution for condition B
dt_B = rnorm(n = 10, #sample size of 10
             mean = 1.5, # mean of 1.5 (i.e. different from A)
             sd = 0.3) # standard deviation of 0.3 (i.e. the same as A)
#set up factor labels for condition B
fct_B = rep(x = 'B', #repeat the letter B
            times = length(dt_B)) #enough labels for each entry in the B data


# Combine into a single dataset -------------------------------------------
#collect the data in a "data.frame"
#N.B. we ensure that they are aligned the data and labels from A & B
#     by entering them in the same order
#
dt = data.frame(value = c(dt_A, #"c(...,...)" combines the data into a single vector
                          dt_B),
                #we'll call the data from all conditions 'value'
                condition = c(fct_A, #"c(...,...)" combines the labels into a single vector
                              fct_B)) #we'll call the condition labels 'condition'
#The resulting data frame is organised in "narrow format" https://en.wikipedia.org/wiki/Wide_and_narrow_data
#We can view the data in table format.
View(dt)#Close the viewing tab to return to the main script.
#you could also read in a data frame with: dt = read.table(...)
summary(dt)#inspect the summary statistics
# 'value' has a mean of 1.24, condition contains the 20 text (character) labels


# Perform a t-test --------------------------------------------------------
#we'll use the t.test function for a "Student's t-test"
#We want to know if 'condition' (A or B) has an effect on value,
# so we can use the 'formula': value ~ condition .
#This asks: "is condition a predictor of value?".
#Since we have only two conditions, the question simplifies to:
# "is there a difference between values in each condition?"
tt = t.test(
  formula = value ~ condition,
  #condition predicts value
  data = dt,
  # search for variables 'condition' and 'value' in our data frame
  alternative = "two.sided",
  # difference could be bigger or smaller
  # paired  = FALSE,
  # there are no pairs of data
  var.equal = TRUE # Student's t-test assumes "equal variances" in the two groups
)

#print the test result
print(tt)
##
## Two Sample t-test
##
## data:  value by condition
## t = -3.2324, df = 18, p-value = 0.004621
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   -0.6878178 -0.1459245
## sample estimates:
##   mean in group A mean in group B
## 1.035062        1.451933
#N.B the means are similar to our input values of 1.0 and 1.5,
# but not exactly the same.

#we can use the 'report' package to see how we would write this in an article
suppressWarnings(report(tt))#the warning messages are irrelevant
## The Two Sample t-test testing the difference of value by condition
## (mean in group A = 1.04, mean in group B = 1.45)
## suggests that the effect is negative, statistically significant,
## and large (difference = -0.42, 95% CI [-0.69, -0.15], t(18) = -3.23, p = 0.005;
## Cohen's d = -1.52, 95% CI [-2.56, -0.46])


# Add another condition for ANOVA -----------------------------------------
#simulate a random normal distribution for condition A
dt_C = rnorm(n = 10, # sample size of 10
             mean = 1.1, # mean of 1.1 (within 1 standard deviation of condition A)
             sd = 0.3) # same standard deviation as the other conditions
#set up factor labels for condition C
fct_C = rep(x = 'C', #repeat the letter C
            times = length(dt_C)) #enough labels for each entry in the C data

#collect the new data in a "data.frame", with the same variable names
dt_new = data.frame(value = dt_C, # data values for C
                    condition = fct_C) # condition labels for C
#We can view the new data in table format.
View(dt_new)#Close the viewing tab to return to the main script.

#Because this new data frame has the same variable names as the old one
# we can merge them together into a single data frame.
#N.B. the option "all = TRUE" ensures that no data is _lost_ during alignment.
dt_all = merge.data.frame(x = dt, #use variable names in the original data
                          y = dt_new, # to align it with the new data
                          all = TRUE) #KEEP ALL DATA IN BOTH DATAFRAMES
#We now have a new data frame containing data from 3 conditions
#Inspect that data as a table
View(dt_all)#Close the viewing tab to return to the main script.

summary(dt_all)#inspect the summary statistics
# 'value' has a mean of 1.18, condition contains the 30 text (character) labels


# Perform one-way ANOVA using aov() ---------------------------------------
#there are now too many conditions to compare using a t-test
#try: t.test(formula = value ~ condition, data = dt_all)
## Error in t.test.formula(formula = value ~ condition, data = dt_all) :
##   grouping factor must have exactly 2 levels

#Instead, we can use aov() to perform an ANalysis Of VAriance (ANOVA)
av = aov(formula = value ~ condition, #condition predicts value
         data = dt_all) # use the full dataset

#We can use summary() to summarise the results of the ANOVA
av_summary = summary(object = av)#collect the summary
print(av_summary)#print the summary
##              Df Sum Sq Mean Sq F value  Pr(>F)
## condition    2  1.133  0.5667   6.727 0.00426 **
## Residuals   27  2.275  0.0842
#We can see that the F statistic 6.7, 
# with 2 degrees of freedom (one less than number of conditions)
# has a probability of 0.004 of occurring when all conditions have the same mean.

#We can also perform an F-test on all predictor variables in the ANOVA
# using the anova() function.
#N.B. Whereas the aov() function creates an ANOVA test object, the anova() function
# only performs analysis of the variance on the test object. i.e. anova() is not
# an ANOVA test function, it is a general analysis of variance function.
av_Ftest = anova(object = av, #this is an R "model object"
                 test = 'F') #perform an F-test
print(av_Ftest)#print the test result
##              Df Sum Sq Mean Sq F value  Pr(>F)
## condition    2  1.1334  0.56672  6.7275 0.004259 **
## Residuals   27  2.2745  0.08424
#for a one-way ANOVA, this gives the same result

#we can use the 'report' package to see how we would write this in an article
suppressWarnings(report(av))#the warning messages are irrelevant
## The main effect of condition is statistically significant and large
## (F(2, 27) = 6.73, p = 0.004; Eta2 = 0.33, 95% CI [0.08, 1.00])

# Perform ANOVA post-hoc tests with correction ----------------------------
#To easily perform the post-hoc tests we load the 'Estimated Marginal Means' package
require(emmeans) #Needs to be installed: install.packages('emmeans')
#Find the means for each condition, and compare each pair of conditions.
av_em = emmeans(object = av, #using the ANOVA model object
                specs = pairwise ~ condition, # compare pairs: A & B, B & C, C & A
                adjust = 'sidak') # correct for multiple comparisons
#Perform the post hoc tests between these means using the test() function
# from the 'emmeans' package (and not any other function called 'test()').
av_ph = emmeans::test(object = av_em)# perform the tests and save them
print(av_ph$contrasts)#print the "contrast" (comparison) part of the test
## contrast estimate   SE df t.ratio p.value
## A - B    -0.41687 0.13 27 -3.212  0.0102 
## A - C    -0.00924 0.13 27 -0.071  0.9998 
## B - C     0.40763 0.13 27  3.140  0.0121 
## 
## P value adjustment: sidak method for 3 tests 


# Use a Linear Model instead of ANOVA -------------------------------------
#The ANOVA is a limited case of a 'linear model'.
#Linear models can therefore be applied to all ANOVA questions,
# as well as many others.
#Fit a linear model using the formula method and the lm() function
lmd = lm(formula = value ~ condition, # condition predicts value
         data = dt_all)  # use the full dataset
#To see the main effect of condition, we use the anova() function to perform an
# F-test
lmd_Ftest = anova(object = lmd,# use the linear model object
                 test = 'F')#perform an F-test
print(lmd_Ftest)#print the F-test result
##              Df Sum Sq Mean Sq F value   Pr(>F)   
##   condition  2 1.1334 0.56672  6.7275 0.004259 **
##   Residuals 27 2.2745 0.08424            
#This is the same information we got from the ANOVA

#We can also report the linear model's F-test, as we did for the other tests.
suppressWarnings(report(lmd_Ftest))
## The main effect of condition is statistically significant and large
## (F(2, 27) = 6.73, p = 0.004; Eta2 = 0.33, 95% CI [0.08, 1.00])

#We can also extract the model summary statistics, which give us more detailed
# information than the ANOVA.
lmd_summary = summary(object = lmd) #extract the summary statistics
print(lmd_summary)# print the summary
##               Estimate Std. Error t value Pr(>|t|)    
##   (Intercept)  1.03506    0.09178  11.277 1.01e-11 ***
##   conditionB   0.41687    0.12980   3.212   0.0034 ** 
##   conditionC   0.00924    0.12980   0.071   0.9438    
#This tells us how each component in the model contributes to the answer.
#The model used the mean for condition A as its intercept (1.03)
# the means for condition B and condition C are added relative to the intercept
# (1.03 + 0.42, 1.03 + 0.01). For condition to be a significant predictor of 
# value, one or more of these differences needs a large t value.
#N.B. the p-value for the intercept only tells us that some of the variation in 
# the data can be described as variation around this intercept. This is almost
# always the case, and does _not_ suggest that the model as a whole is significant.

# Perform post-hoc tests on the linear model ------------------------------
#Just like for the ANOVA, to perform post-hoc tests we must find the means for 
# each condition, and compare each pair of conditions.
lmd_em = emmeans(object = lmd, #using the Linear Model object
                 specs = pairwise ~ condition, # compare pairs: A & B, B & C, C & A
                 adjust = 'sidak') # correct for multiple comparisons
#Perform the post hoc tests between these means using the test() function
# from the 'emmeans' package (and not any other function called 'test()').
lmd_ph = emmeans::test(object = lmd_em)
# print the "contrast" (comparison) part of the test
print(lmd_ph$contrast)
# contrast estimate   SE df t.ratio p.value
# A - B    -0.41687 0.13 27 -3.212  0.0102 
# A - C    -0.00924 0.13 27 -0.071  0.9998 
# B - C     0.40763 0.13 27  3.140  0.0121 
# 
# P value adjustment: sidak method for 3 tests 
