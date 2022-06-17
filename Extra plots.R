# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 06 16
#     MODIFIED:	James Foster              DATE: 2022 06 16
#
#  DESCRIPTION: Extra plots for introducing hypothesis testing and modelling.
#
#       USAGE:  First install the 'report' and 'emmeans' packages as suggested by
#               Rstudio, or by running install.packages('report'); install.packages('emmeans)
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: -
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


# t-distribution and p-value ----------------------------------------------
xx = seq(from = -5,
         to = 5,
         length.out = 1e3)
pt = dt(x = xx,
        df = 18
        )
t_val = -3.2324
x_undert = subset(x = xx, 
                  subset = xx<t_val)
pt_undert = subset(x = pt, 
                   subset = xx<t_val)
plot(x = xx, 
     y = pt,
     lwd = 5,
     type = 'l',
     col = 'blue',
     xlab = 't-statistic',
     ylab = 'p(t) for null hypothesis')
abline(h = 0)
abline(v = -3.2324,
       col = 'darkblue',
       lwd = 2)
polygon(x = c(x_undert, rev(x_undert)),
        y = c(pt_undert, rep(x = 0, times = length(x_undert))),
        col = 'cyan',
        border = 'cyan')
polygon(x = c(-x_undert, rev(-x_undert)),
        y = c(pt_undert, rep(x = 0, times = length(x_undert))),
        col = 'cyan',
        border = 'cyan')


# Logit transform of sinusoid ---------------------------------------------
n_per_obs = 100

yy = rbinom(n = length(xx),
            size = n_per_obs,
            prob = plogis(xx*1.0 - 0.2 + rnorm(length(xx)))
)
co = 
  lapply(X = yy,
            FUN = function(y)
            { xnew = if(y < n_per_obs & y > 0)
              { c(rep(1, times = y),
                             rep(0, times = (n_per_obs-y))
              )
            }else
            {
              if(y == n_per_obs){rep(1, times = y)}else{rep(0, times = n_per_obs)}
            }
            
            }
           )
co = do.call(c,
             co)

xxx = lapply(X = xx,
                    FUN = function(x)
                      {
                      rep(x, times = n_per_obs)
                    })
xxx = do.call(c, xxx)

plot(x = xx, 
     y = yy/n_per_obs,
     lwd = 3,
     pch = 21,
     col = adjustcolor(col = 'blue', alpha.f = 200/255),
     bg = 'white',
     xlab = 'predictor variable',
     ylab = 'proportion of correct choices')
abline(h = c(0,1))

plot(x = xx, 
     y = qlogis(p = yy/n_per_obs),
     lwd = 3,
     pch = 21,
     col = adjustcolor(col = 'blue', alpha.f = 200/255),
     bg = 'white',
     xlab = 'predictor variable',
     ylab = 'log(odds) of correct choices')
ll = glm(formula = co ~ xxx,
         family = binomial(link = logit))
abline(ll, col = 'cyan3', lwd = 5)
