# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 06 16
#     MODIFIED:	James Foster              DATE: 2023 05 28
#
#  DESCRIPTION: Example two-group hypothesis tests, using the "t.test" and "wilcox.test" base R functions.
#
#       USAGE:  First install the 'report' package as suggested by
#               Rstudio, or by running install.packages('report')
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: -
#
#   REFERENCES: Turner, S. (2017) Essential Statistics with R
#               www.bioconnector.github.io/workshops/r-stats.html#continuous_variables
#
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
#The resulting data frame is organised clearly in "narrow format" https://en.wikipedia.org/wiki/Wide_and_narrow_data
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
  paired  = FALSE,
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
#   but not exactly the same.

#we can use the 'report' package to see how we would write this in an article
suppressWarnings(report(tt))#the warning messages are irrelevant
## The Two Sample t-test testing the difference of value by condition
## (mean in group A = 1.04, mean in group B = 1.45)
## suggests that the effect is negative, statistically significant,
## and large (difference = -0.42, 95% CI [-0.69, -0.15], t(18) = -3.23, p = 0.005;
## Cohen's d = -1.52, 95% CI [-2.56, -0.46])


# Welch's t-test for Unequal Variances ------------------------------------

#The effects of conditions A and B may cause them to have different variances.
#Student's t-test can report false-positive significant differences in this case,
# even though the two means are not truly different.
#We can simulate this by replacing values for B with new simulated values.
dt_unequal = within(dt, #Within our data frame,
                    {
                      # find all values for condition B
                      value[condition %in% 'B'] = rnorm(n = 10, #sample size of 10
                                                        mean = 1.5, # mean of 1.5 (i.e. different from A)
                                                        sd = 0.15) # standard deviation of 0.15 (i.e. half that for A)
                    })


#We can view the updated data in table format.
View(dt_unequal)#Close the viewing tab to return to the main script.
#you could also read in a data frame with: dt = read.table(...)
summary(dt_unequal)#inspect the summary statistics
# 'value' has a mean of 1.25 rather than 1.24, condition still contains the 20 text (character) labels

#we'll use the t.test function, with the setting unequal = TRUE
# for a "Welch's t-test for unequal variances"
#We want to know if 'condition' (A or B) has an effect on value,
# so we can use the 'formula': value ~ condition .
#This asks: "is condition a predictor of value?".
#Since we have only two conditions, the question simplifies to:
# "is there a difference between values in each condition?"
tt_welch = t.test(
  formula = value ~ condition,
  #condition predicts value
  data = dt_unequal,
  # search for variables 'condition' and 'value' in our data frame
  alternative = "two.sided",
  # difference could be bigger or smaller
  paired  = FALSE,
  # there are no pairs of data
  var.equal = FALSE # Welch's t-test does not assume "equal variances" in the two groups
)

#print the test result
print(tt_welch)
##
## Welch Two Sample t-test
##
## data:  value by condition
## t = -3.8411, df = 12.465, p-value = 0.002196
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   -0.6840013 -0.1901767
## sample estimates:
##   mean in group A mean in group B
## 1.035062        1.472151
#N.B the means are still similar to our input values of 1.0 and 1.5, but not exactly the same.


#we can use the 'report' package to see how we would write this in an article
suppressWarnings(report(tt_welch))#the warning messages are irrelevant
## he Welch Two Sample t-test testing the difference of value by condition
## (mean in group A = 1.04, mean in group B = 1.47)
## suggests that the effect is negative, statistically significant,
## and large (difference = -0.44, 95% CI [-0.68, -0.19], t(12.46) = -3.84, p = 0.002;
## Cohen's d = -2.18, 95% CI [-3.54, -0.75])


# For non-normal distributions, use a Mann-Whitney test ---------------------
#Data may have an unknown distribution that does not fit with the t-test's
# assumption of a normal distribution.
#we will simulate random data for this example

#simulate a random uniform distribution for condition A
dt_un_A = rnorm(n = 10, #sample size of 10
                mean = 1.0, #mean of 1.0
                sd = 0.3 # standard deviation of 0.3
                ) +  rpois(n = 10, # noise from a Poisson process
                lambda = 3.4) # average of 3.4 added asymmetrically
#set up factor labels for condition A
fct_un_A = rep(x = 'A', #repeat the letter A
               times = length(dt_un_A)) #enough labels for each entry in the A data

#simulate a random normal distribution for condition B
# dt_un_B = runif(n = 500, #sample size of 10
#                 min = 0.0, # values between a minimum of 0
#                 max = 10.0) # and a maximum of 10
dt_un_B = rnorm(n = 10, #sample size of 10
                 mean = 3.0, #mean of 1.0
                 sd = 0.6 #  standard deviation of 0.6 (unequal variances)) 
           ) +  rpois(n = 10, # noise from a Poisson process
           lambda = 3.4) # average of 3.4 added asymmetrically
#set up factor labels for condition B
fct_un_B = rep(x = 'B', #repeat the letter B
               times = length(dt_un_B)) #enough labels for each entry in the B data

#collect the data in a "data.frame"
dt_unknown = data.frame(value = c(dt_un_A, #"c(...,...)" combines the data into a single vector
                                  dt_un_B),
                        #we'll call the data from all conditions 'value'
                        condition = c(fct_un_A, #"c(...,...)" combines the labels into a single vector
                                      fct_un_B)) #we'll call the condition labels 'condition'

#We can view the data in table format.
View(dt_unknown)#Close the viewing tab to return to the main script.
#you could also read in a data frame with: dt = read.table(...)
summary(dt_unknown)#inspect the summary statistics
# 'value' has a mean of 5.72, condition contains the 20 text (character) labels

#calculate the mean and confidence interval for each condition
mean_unknown = aggregate(
  formula = value ~ condition,
  data = dt_unknown,
  FUN = function(x)
    # function for calculating 95% CI
  {
    sum_stat = mean(x) + #calculate mean
      1.96 * c(0, -1, 1) * #mean, lower and upper 95%CI
      sd(x) / sqrt(length(x))  #standard error of the mean
    names(sum_stat) = c('mean', 'lower CI', 'upper CI')
    return(sum_stat)
  }
)
print(mean_unknown)
## condition value.mean value.lower CI value.upper CI
## 1         A   4.683984       3.306476       6.061492
## 2         B   6.762036       5.298387       8.225685
#Although the means of the two conditions are different,
# the 95% confidence intervals of the two means overlap
# due to the large variances.
#Perform a Welch's t-test
tt_unknown = t.test(
  formula = value ~ condition,
  #condition predicts value
  data = dt_unknown,
  # search for variables 'condition' and 'value' in our data frame
  alternative = "two.sided",
  # difference could be bigger or smaller
  paired  = FALSE,
  # there are no pairs of data
  var.equal = FALSE # Welch's t-test does not assume "equal variances" in the two groups
)
print(tt_unknown)
#As a result, the t-test finds only a marginal difference with p>0.05
## Welch Two Sample t-test
##
## data:  value by condition
## t = -2.0264, df = 17.934, p-value = 0.05786
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   -4.23305430  0.07695191
## sample estimates:
##   mean in group A mean in group B
## 4.683984        6.762036

#The Mann-Whitney Wilcoxon rank sum test ranks the data and compares which condition
# has higher ranked data. It therefore does not assume any specific distribution.
# any distribution.

mww = wilcox.test(
  formula = value ~ condition,
  #condition predicts value
  data = dt_unknown,
  # search for variables 'condition' and 'value' in our data frame
  alternative = "two.sided",
  #A could be bigger or smaller
  paired = FALSE #perform a rank sum test (paired would be signed-ranks)
)
#print the test result
print(mww)
## Wilcoxon rank sum exact test
##
## data:  value by condition
## W = 23, p-value = 0.04326
## alternative hypothesis: true location shift is not equal to 0
#The Mann-Whitney Wilcoxon rank sum test correctly identified the difference.


# Plot to show the effect of rank transforming ----------------------------

par(mfrow = c(1,2))
with(dt_unknown,
     {
       hist(value[condition %in% 'A'],
            breaks = 1e2,
            col = 'blue',
            xlim = range(value),
            xlab = 'value',
            main = 'original values')
       hist(value[condition %in% 'B'],
            breaks = 1e2,
            col = 'red',
            add = T
       )
       hist(rank(value)[condition %in% 'A'],
            breaks = 1e2,
            col = 'blue',
            xlim = c(0,max(rank(value))),
            xlab = 'value',
            main = 'rank-transformed'
       )
       hist(rank(value)[condition %in% 'B'],
            breaks = 1e2,
            col = 'red',
            add = T
       )
     }
)
