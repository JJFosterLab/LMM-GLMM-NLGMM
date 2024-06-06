# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 06 16
#     MODIFIED:	James Foster              DATE: 2024 06 04
#
#  DESCRIPTION: Extra plots for introducing hypothesis testing and modelling.
#
#       USAGE:  First install the 'report' and 'emmeans' packages as suggested by
#               Rstudio, or by running install.packages('report'); install.packages('emmeans)
#               Run line by line (ctrl+enter), or run whole script (ctrl+shift+s)
#               Organised into subsections in overview (ctrl+shift+o)
#
#	   CHANGES: -More plots for MLE
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
#- Comment MLE plots


# Function for saving as PNG ----------------------------------------------
#Save the current figure as a single page PDF with the dimensions displayed
PNGsave  = function(Directory = getwd(),#place to save PNG
                    PlotName = 'My Plot',#name of the plot
                    Expr,#the plot command, as an expression between "{" and "}"
                    Dim = par("din"),#dimensions c(width, height) in inches (default)
                    Res = 150,#resolution in pixels/inch, defaults to 150 
                    ...){#any other commands are passed to png()
  #EXAMPLE
  # PNGsave(Directory = getwd(), PlotName = 'My Plot',
  #   Expr = 
  #     {plot(1:10, (1:10)^2)
  #       abline(h = 50, col = 2)
  #       legend('top',legend = c('x^2', 'x=50'),
  #              pch = c(1,NA), lty = c(NA,1), col = 1:2)
  #     }
  # )
  #open PNG device with the correct name and dimensions
  png(file = file.path(Directory, paste0(PlotName,'.png')),
      width = Dim[1]*Res, height = Dim[2]*Res, res = Res,
      ...
  )
  #evaluate the plotting expression using the environment *outside* the function
  eval(Expr, envir = .GlobalEnv)
  #Doesn't save until invisible device is closed
  dev.off()
  dev.set(dev.prev())#if there was an active device, switch back
}


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



# Likelihood calculation --------------------------------------------------
#plot parameters
dm = c(15.5,10)
rs = 300#dots per inch
lw = 7#line width
arlw = 5#arrow line width

set.seed(17070523)#seed the random number generator with Linnaeus' birthday
#generate a random sample of 10 points from a narrow normal distribution
smpl = rnorm(n = 10,
             mean = 0.0,
             sd = 0.5)
#set up parameters for prospective normal distributions
mean_lst = list(a = -2,
                b = 0,
                c = 0)
sd_lst = list(a = 1.0,
                b = 1.0,
                c = 0.5)
#a function to estimate normal distributions across the xx values
Dnorm_xx = function(m,s, ...)
{dnorm(x = xx, mean = m, sd = s, ...)}
#apply the function across mean and sd values
norm_curve_lst = mapply(FUN = Dnorm_xx,
                        m = mean_lst,
                        s = sd_lst,
                        SIMPLIFY = FALSE)

#Plot 0, just the sample
PNGsave(PlotName = 'MLE_0',
        Res = rs,
        Dim = dm,
        Expr = 
          {
            #open the plot
        par(mar = c(4,4,0,0),
            cex = 2)
        #set up the axes
        plot(x = NULL, # no data yet
             xlim = c(-5, 5),
             ylim = c(0,0.8),
             xlab = 'observed y',
             ylab = 'probability density'
             )
        #show minimum probability density
        abline(h = 0)
        #add the data as a stripchart
        stripchart(x = smpl,
                   at = 0,
                   add = TRUE,
                   pch = 21,
                   bg = 'salmon',
                   col = 'yellow',
                   cex = 1.3
                   )
        #label with a legend in the top right
        legend(x = 'topright',
               pch = c(20),
               col = c('salmon'),
               legend = c('observed values of y (1-10)'
               ),
               bty = 'n',
               cex = 1.0
        )
        }
)

#Plot 1 
PNGsave(PlotName = 'MLE_1',
        Res = rs,
        Dim = dm,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 2)
plot(x = NULL,
     xlim = c(-5, 5),
     ylim = c(0,0.8),
     xlab = 'observed y',
     ylab = 'probability density'
)
abline(h = 0)
lines(x = xx,
      y = norm_curve_lst$a,
      lwd = lw,
      col = 'darkgreen')
arrows(x0 = min(smpl),
       x1 = min(smpl),
       y0 = rep(x = 0, times = length(smpl)),
       y1 = dnorm(x = min(smpl),
                  mean = mean_lst$a,
                  sd = sd_lst$a)*0.99,
       col = 'salmon', 
       lwd = arlw,
       length = 0.1)
stripchart(x = smpl,
           at = 0,
           add = TRUE,
           pch = 21,
           bg = 'salmon',
           col = 'yellow',
           cex = 1.3
)
legend(x = 'topright',
       lty = c(1,NA,NA),
       lwd = c(5,NA,NA),
       pch = c(NA, 18, 18),
       col = c('darkgreen','salmon', 'salmon'),
       legend = c('mean = -2.0, sd = 1.0',
                  paste('likelihood of observing',
                        round(min(smpl),3),
                        '\n=',
                        signif(x = 
                                 dnorm(x = min(smpl),
                                            mean = mean_lst$a,
                                            sd = sd_lst$a),
                               digits = 3)
                  )
       ),
       bty = 'n',
       cex = 1.0
)
})

PNGsave(PlotName = 'MLE_2',
        Res = rs,
        Dim = dm,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 2)
plot(x = NULL,
     xlim = c(-5, 5),
     ylim = c(0,0.8),
     xlab = 'observed y',
     ylab = 'probability density'
     )
abline(h = 0)
lines(x = xx,
      y = norm_curve_lst$a,
      lwd = lw,
      col = 'darkgreen')
arrows(x0 = smpl,
       x1 = smpl,
       y0 = rep(x = 0, times = length(smpl)),
       y1 = dnorm(x = smpl,
                  mean = mean_lst$a,
                  sd = sd_lst$a)*0.99,
       col = 'salmon', 
       lwd = arlw,
       length = 0.1)
stripchart(x = smpl,
           at = 0,
           add = TRUE,
           pch = 21,
           bg = 'salmon',
           col = 'yellow',
           cex = 1.3
           )
legend(x = 'topright',
       lty = c(1,NA,NA),
       lwd = c(5,NA,NA),
       pch = c(NA, 18, 18),
       col = c('darkgreen','salmon', 'salmon'),
       legend = c('mean = -2.0, sd = 1.0',
                 paste('likelihood = ',
                      signif(x = 
                        prod(dnorm(x = smpl,
                                 mean = mean_lst$a,
                                 sd = sd_lst$a)),
                        digits = 3)
                      ),
                  paste('log-likelihood = ',
                      signif(x = sum(dnorm(x = smpl,
                                 mean = mean_lst$a,
                                 sd = sd_lst$a,
                                log = TRUE)),
                          digits = 3)
                  )
       ),
       bty = 'n',
       cex = 1.0
)
})

PNGsave(PlotName = 'MLE_3',
        Res = rs,
        Dim = dm,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 2)
plot(x = NULL,
     xlim = c(-5, 5),
     ylim = c(0,0.8),
     xlab = 'observed y',
     ylab = 'probability density'
     )
abline(h = 0)
stripchart(x = smpl,
           at = 0,
           add = TRUE,
           pch = 21,
           bg = 'salmon',
           col = 'yellow',
           cex = 1.3
           )
lines(x = xx,
      y = norm_curve_lst$a,
      lwd = lw,
      col = 'darkgreen')
lines(x = xx,
      y = norm_curve_lst$b,
      lwd = lw,
      col = 'darkred')
arrows(x0 = smpl,
       x1 = smpl,
       y0 = rep(x = 0, times = length(smpl)),
       y1 = dnorm(x = smpl,
                  mean =mean_lst$b,
                  sd = sd_lst$b)*0.99,
       col = 'salmon', 
       lwd = arlw,
       length = 0.1)
legend(x = 'topright',
       lty = c(1,NA,1,NA),
       lwd = c(5,NA,5,NA),
       col = c('darkgreen',
               NA,
               'darkred',
               NA),
       legend = c('mean = -2.0, sd = 1.0',
                  paste('log-likelihood = ',
                        signif(x = 
                                 sum(dnorm(x = smpl,
                                            mean = mean_lst$a,
                                            sd = sd_lst$a,
                                           log = TRUE)),
                               digits = 3)
                  ),
                  'mean = 0.0, sd = 1.0',
                  paste('log-likelihood = ',
                        signif(x = sum(dnorm(x = smpl,
                                             mean = mean_lst$b,
                                             sd = sd_lst$b,
                                             log = TRUE)),
                               digits = 3)
                  )
       ),
       bty = 'n',
       cex = 1.0
)
})

PNGsave(PlotName = 'MLE_4',
        Res = rs,
        Dim = dm,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 2)
plot(x = NULL,
     xlim = c(-5, 5),
     ylim = c(0,0.8),
     xlab = 'observed y',
     ylab = 'probability density'
     )
abline(h = 0)
stripchart(x = smpl,
           at = 0,
           add = TRUE,
           pch = 21,
           bg = 'salmon',
           col = 'yellow',
           cex = 1.3
           )
lines(x = xx,
      y = norm_curve_lst$a,
      lwd = lw,
      col = 'darkgreen')
lines(x = xx,
      y = norm_curve_lst$b,
      lwd = lw,
      col = 'darkred')
lines(x = xx,
      y = norm_curve_lst$c,
      lwd = lw,
      col = 'darkblue')
arrows(x0 = smpl,
       x1 = smpl,
       y0 = rep(x = 0, times = length(smpl)),
       y1 = dnorm(x = smpl,
                  mean = mean_lst$c,
                  sd = sd_lst$c)*0.99,
       col = 'salmon', 
       lwd = arlw,
       length = 0.1)
legend(x = 'topright',
       lty = c(1,NA,1,NA),
       lwd = c(5,NA,5,NA),
       col = c('darkgreen',
               NA,
               'darkred',
               NA,
               'darkblue',
               NA),
       legend = c('mean = -2.0, sd = 1.0',
                  paste('log-likelihood = ',
                        signif(x = 
                                 sum(dnorm(x = smpl,
                                            mean = mean_lst$a,
                                            sd = sd_lst$a,
                                           log = TRUE)),
                               digits = 3)
                  ),
                  'mean = 0.0, sd = 1.0',
                  paste('log-likelihood = ',
                        signif(x = sum(dnorm(x = smpl,
                                             mean = mean_lst$b,
                                             sd = sd_lst$b,
                                             log = TRUE)),
                               digits = 3)
                  ),
                  'mean = 0.0, sd = 0.5',
                  paste('log-likelihood = ',
                        signif(x = sum(dnorm(x = smpl,
                                             mean = mean_lst$c,
                                             sd = sd_lst$c,
                                             log = TRUE)),
                               digits = 3)
                  )
       ),
       bty = 'n',
       cex = 1.0
)
})

# Multiple parameter MLE calculation --------------------------------------------------
#plot parameters
dm_m = c(6.24,5.45)
rs = 300#dots per inch
lw = 7#line width
arlw = 5#arrow line width

set.seed(17070523)#seed the random number generator with Linnaeus' birthday
#generate a random sample of 10 points from a narrow normal distribution
#set up parameters for prospective normal distributions
m_mean_lst = list(a = -0.60,  
                b = 1.00,
                c = 1.50)
m_sd_lst = list(a = 1.0,
              b = 1.0,
              c = 1.0)
#multiple samples
mlts = sapply(X = m_mean_lst,
              FUN = rnorm,
              n = 4)
#apply the function across mean and sd values
meas_mean_lst = sapply(data.frame(mlts), mean)
# meas_sd_lst = sapply(data.frame(mlts), sd) #not how it works in models
meas_sd_all = sd(apply(X = mlts,MARGIN = 2, FUN = function(x){x - mean(x)}) )
m_norm_curve_lst = mapply(FUN = Dnorm_xx,
                        m = meas_mean_lst,
                        s = meas_sd_all,
                        log = TRUE,
                        SIMPLIFY = FALSE)
m_norm_curv_all = Dnorm_xx(m = mean(mlts), 
                           s = sd(mlts),
                           log = TRUE)



PNGsave(PlotName = 'MultMLE_0',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = 'salmon',
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50')
          })

PNGsave(PlotName = 'MultMLE_0_0',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = 'salmon',
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50')
            abline(v = mean(mlts),
                   col = 'purple4',
                   lwd = arlw)
          })
PNGsave(PlotName = 'MultMLE_0_1',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = 'salmon',
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50')
            abline(v = mean(mlts),
                   col = 'purple4',
                   lwd = arlw)
            arrows(x0 = mean(mlts)-sd(mlts),
                   x1 = mean(mlts)+sd(mlts),
                   y0 = dnorm(x = mean(mlts)+sd(mlts),
                              mean = mean(mlts),
                              sd = sd(mlts),
                                      log = TRUE),
                   y1 = dnorm(x = mean(mlts)+sd(mlts),
                              mean = mean(mlts),
                              sd = sd(mlts),
                                      log = TRUE),
                   code = 3,
                   angle = 90,
                   lwd = arlw,
                   length = 0.1,
                   col = 'red3'
                   )
          })

PNGsave(PlotName = 'MultMLE_1',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = 'salmon',
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50')
            arrows(x0 = mlts,
                   x1 = mlts,
                   y0 = rep(x = -5, times = length(smpl)),
                   y1 = dnorm(x = mlts,
                              mean = mean(mlts),
                              sd = sd(mlts),
                              log = TRUE)*0.99,
                   col = 'salmon', 
                   lwd = arlw,
                   length = 0.1)
            legend(x = 'topleft',
                   inset = 0.1,
                   lty = c(1,NA),
                   lwd = c(5,NA),
                   col = c('gray50',
                           NA
                           ),
                   legend = c('mean = 0.84, sd = 1.6',
                              paste('log-likelihood = ',
                                    signif(x = 
                                             sum(dnorm(x = mlts,
                                                       mean = mean(mlts),
                                                       sd = sd(mlts),
                                                       log = TRUE)),
                                           digits = 3)
                                    )
                   ),
                   bty = 'n',
                   cex = 1.0
            )
          })

PNGsave(PlotName = 'MultMLE_2_0',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50',
                  lty = 3)
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = sort(rep(x = 
                                       c('orange2',
                                          'seagreen',
                                        'slategray3'),
                                     times = 4) ),
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
          })

PNGsave(PlotName = 'MultMLE_2_1',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50',
                  lty = 3)
            lines(x = xx,
                  y = m_norm_curve_lst$a,
                  lwd = lw,
                  col = 'orange2')
            lines(x = xx,
                  y = m_norm_curve_lst$b,
                  lwd = lw,
                  col = 'seagreen')
            lines(x = xx,
                  y = m_norm_curve_lst$c,
                  lwd = lw,
                  col = 'slategray3')
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = sort(rep(x = 
                                       c('orange2',
                                          'seagreen',
                                        'slategray3'),
                                     times = 4) ),
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
          })

PNGsave(PlotName = 'MultMLE_3_0',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50',
                  lty = 3)
            lines(x = xx,
                  y = m_norm_curve_lst$a,
                  lwd = lw,
                  col = 'orange2')
            lines(x = xx,
                  y = m_norm_curve_lst$b,
                  lwd = lw,
                  col = 'seagreen')
            lines(x = xx,
                  y = m_norm_curve_lst$c,
                  lwd = lw,
                  col = 'slategray3')
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = sort(rep(x = 
                                       c('orange2',
                                          'seagreen',
                                        'slategray3'),
                                     times = 4) ),
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
            arrows(x0 = mlts[,'a'],
                   x1 = mlts[,'a'],
                   y0 = -5,
                   y1 = dnorm(x = mlts[,'a'],
                              mean = mean(mlts[,'a']),
                              sd = meas_sd_all,
                              # sd = sd(mlts[,'a']),
                              log = TRUE),
                   col = 'orange2', 
                   lwd = arlw,
                   length = 0.1)
            arrows(x0 = mlts[,'b'],
                   x1 = mlts[,'b'],
                   y0 = -5,
                   y1 = dnorm(x = mlts[,'b'],
                              mean = mean(mlts[,'b']),
                              sd = meas_sd_all,
                              # sd = sd(mlts[,'b']),
                              log = TRUE),
                   col = 'seagreen', 
                   lwd = arlw,
                   length = 0.1)
            arrows(x0 = mlts[,'c'],
                   x1 = mlts[,'c'],
                   y0 = -5,
                   y1 = dnorm(x = mlts[,'c'],
                              mean = mean(mlts[,'c']),
                              sd = 
                                meas_sd_all,
                              # sd(mlts[,'c']),
                              log = TRUE),
                   col = 'slategray3', 
                   lwd = arlw,
                   length = 0.1)
            legend(x = 'topleft',
                   legend = c(
                     paste('all data: log-likelihood = ',
                                    signif(x = 
                                             sum(dnorm(x = mlts,
                                                       mean = mean(mlts),
                                                       sd = sd(mlts),
                                                       log = TRUE)),
                                           digits = 3) ),
                                    paste('by conditions: log-likelihood = ',
                                    signif(x = 
                                             sum(dnorm(x = mlts[,'a'],
                                                       mean = mean(mlts[,'a']),
                                                       sd = meas_sd_all,
                                                       # sd = sd(mlts[,'a']),
                                                       log = TRUE)) +
                                             sum(dnorm(x = mlts[,'b'],
                                                       mean = mean(mlts[,'b']),
                                                       sd = meas_sd_all,
                                                       # sd = sd(mlts[,'b']),
                                                       log = TRUE)) +
                                             sum(dnorm(x = mlts[,'c'],
                                                       mean = mean(mlts[,'c']),
                                                       sd = meas_sd_all,
                                                       # sd = sd(mlts[,'c']),
                                                       log = TRUE))  ,
                                           digits = 3)
                              )
                   ),
                   bty = 'n',
                   cex = 1.0
            )
            
          })

PNGsave(PlotName = 'MultMLE_3_1',
        Res = rs,
        Dim = dm_m,
        Expr = 
          {
            par(mar = c(4,4,0,0),
                cex = 1.5)
            plot(x = NULL,
                 xlim = c(-5, 5),
                 ylim = c(-5,0),
                 xlab = 'observed y',
                 ylab = 'log probability density'
            )
            abline(h = 0)
            lines(x = xx,
                  y = m_norm_curv_all,
                  lwd = lw,
                  col = 'gray50',
                  lty = 3)
            lines(x = xx,
                  y = m_norm_curve_lst$a,
                  lwd = lw,
                  col = 'orange2')
            lines(x = xx,
                  y = m_norm_curve_lst$b,
                  lwd = lw,
                  col = 'seagreen')
            lines(x = xx,
                  y = m_norm_curve_lst$c,
                  lwd = lw,
                  col = 'slategray3')
            stripchart(x = mlts,
                       at = -5,
                       add = TRUE,
                       pch = 21,
                       bg = sort(rep(x = 
                                       c('orange2',
                                          'seagreen',
                                        'slategray3'),
                                     times = 4) ),
                       col = 'yellow',
                       cex = 1.3,
                       method = 'overplot'
                         
            )
            arrows(x0 = mean(mlts[,'a']),
                   x1 = mean(mlts[,'a']),
                   y0 = -5,
                   y1 = dnorm(x = 0,
                              mean = 0,
                              sd = meas_sd_all,
                              # sd = sd(mlts[,'a']),
                              log = TRUE),
                   col = 'orange2', 
                   lwd = arlw,
                   length = 0)
            arrows(x0 = mean(mlts[,'b']),
                   x1 = mean(mlts[,'b']),
                   y0 = -5,
                   y1 = dnorm(x = 0,
                              mean = 0,
                              sd = meas_sd_all,
                              # sd = sd(mlts[,'b']),
                              log = TRUE),
                   col = 'seagreen', 
                   lwd = arlw,
                   length = 0)
            arrows(x0 = mean(mlts[,'c']),
                   x1 = mean(mlts[,'c']),
                   y0 = -5,
                   y1 = dnorm(x = 0,
                              mean = 0,
                              sd = 
                                meas_sd_all,
                                # sd(mlts[,'c']),
                              log = TRUE),
                   col = 'slategray3', 
                   lwd = arlw,
                   length = 0)
          })


# 3D MLE Optimisation -----------------------------------------------------
# set.seed(17070523)#seed the random number generator with Linnaeus' birthday
# #multiple samples, larger sample for smooth surface
# mlts_3d = sapply(X = m_mean_lst[c('a','b')],
#               FUN = rnorm,
#               n = 10)
dm_3d = c(7.381890, 4.901575)*1.1
mlts_3d = mlts

lim = 2
condition_A = seq(from = -1*lim,
         to = 1*lim,
         length.out = 5e1)
condition_B = seq(from = -1*lim,
         to = 1*lim,
         length.out = 5e1)
LikFun  = function(x, y)
{ 
  est_sd = sqrt( #sum of squared errors
              sum(
                  (mlts_3d[,'a'] - x)^2 + 
                    (mlts_3d[,'b'] - y)^2
                  ) / length(mlts_3d)
            ) * 
              length(mlts_3d)/(length(mlts_3d)-1) #Bessel correction
  ll = sum(dnorm(x = mlts_3d[,'a'],
            mean = x,
            sd = est_sd ,
            log = TRUE)) +
      sum(dnorm(x = mlts_3d[,'b'],
                mean = y,
                sd = est_sd,
                log = TRUE))
  return(ll)
}
VLikFun = Vectorize(LikFun)
log_likelihood = outer(X = condition_A,
                       Y = condition_B, 
                       FUN = Vectorize(LikFun) )
log_likelihood_NA = ifelse(log_likelihood < median(log_likelihood)-1.6,
                           yes = NA,
                           no = log_likelihood)
PNGsave(PlotName = 'MLE3D_0',
        Res = rs,
        Dim = dm_3d,
        Expr = 
          {
par(mar = c(0,0,0,0))
rr = persp(x = condition_A, 
      y = condition_B, 
      z = log_likelihood_NA,
      zlim = c(median(log_likelihood), max(log_likelihood)),
      theta = 30,
      phi = 30,
      expand = 0.5,
      col = adjustcolor("lightblue",alpha.f = 0.5),
      border = adjustcolor('blue', alpha.f = 0.5),
      xlab = "Condition A mean",
      ylab = "Condition B mean",
      zlab = "log(likelihood)",
      cex.axis = 0.8,
      ticktype = 'detailed')
points(trans3d(x = mlts_3d[,'a'],
               y = rep(-2, length(mlts_3d[,'a'])), 
               z = rep(-15, length(mlts_3d[,'a'])),          
               pmat = rr),
       col = 'orange2',
       pch = 20,
       lwd = lw,
       cex = 2.0)
points(trans3d(y = mlts_3d[,'b'],
               x = rep(2, length(mlts_3d[,'b'])), 
               z = rep(-15, length(mlts_3d[,'b'])),          
               pmat = rr),
       col = 'seagreen',
       pch = 20,
       lwd = lw,
       cex = 2.0)
}
)

Ofun = function(prm)
{ - LikFun(prm[1],prm[2]) }
Ostepper = function(n, ...)
{
  oo = optim(par = c(a = 1,b = -1),
        fn = Ofun,
        control = list(maxit = n)
  )
  return(c(oo$par, ll = oo$value))
}
osteps = sapply(X = 1:100,
                FUN = Ostepper)
#prune identical osteps
usteps = osteps[,!duplicated(osteps['ll',])]

PNGsave(PlotName = 'MLE3D_1_0',
        Res = rs,
        Dim = dm_3d,
        Expr = 
          {
            par(mar = c(0,0,0,0))
rr = persp(x = condition_A, 
           y = condition_B, 
           # z = ll_mat,
           z = log_likelihood_NA,
           zlim = c(median(log_likelihood), max(log_likelihood)),
           theta = 30,
           phi = 30,
           expand = 0.5,
           col = adjustcolor("lightblue",alpha.f = 0.5),
           border = adjustcolor('blue', alpha.f = 0.5),
           xlab = "Condition A mean",
           ylab = "Condition B mean",
           zlab = "log(likelihood)",
           cex.axis = 0.8,
           ticktype = 'detailed')
points(trans3d(x = usteps['a',1:4],
              y = usteps['b',1:4], 
              z = -usteps['ll',1:4],          
              pmat = rr),
      col = 'red',
      type = 'b',
      pch = paste(1:4),
      lwd = 1.0,
      cex = 2.0)
points(trans3d(x = mlts_3d[,'a'],
               y = rep(-2, length(mlts_3d[,'a'])), 
               z = rep(-15, length(mlts_3d[,'a'])),          
               pmat = rr),
       col = 'orange2',
       pch = 20,
       lwd = lw,
       cex = 2.0)
points(trans3d(y = mlts_3d[,'b'],
               x = rep(2, length(mlts_3d[,'b'])), 
               z = rep(-15, length(mlts_3d[,'b'])),          
               pmat = rr),
       col = 'seagreen',
       pch = 20,
       lwd = lw,
       cex = 2.0)
})

PNGsave(PlotName = 'MLE3D_1',
        Res = rs,
        Dim = dm_3d,
        Expr = 
          {
            par(mar = c(0,0,0,0))
rr = persp(x = condition_A, 
           y = condition_B, 
           # z = ll_mat,
           z = log_likelihood_NA,
           zlim = c(median(log_likelihood), max(log_likelihood)),
           theta = 30,
           phi = 30,
           expand = 0.5,
           col = adjustcolor("lightblue",alpha.f = 0.5),
           border = adjustcolor('blue', alpha.f = 0.5),
           xlab = "Condition A mean",
           ylab = "Condition B mean",
           zlab = "log(likelihood)",
           cex.axis = 0.8,
           ticktype = 'detailed')
points(trans3d(x = usteps['a',1:9],
              y = usteps['b',1:9], 
              z = -usteps['ll',1:9],          
              pmat = rr),
      col = 'red',
      type = 'b',
      pch = paste(1:9),
      lwd = 1.0,
      cex = 2.0)
points(trans3d(x = mlts_3d[,'a'],
               y = rep(-2, length(mlts_3d[,'a'])), 
               z = rep(-15, length(mlts_3d[,'a'])),          
               pmat = rr),
       col = 'orange2',
       pch = 20,
       lwd = lw,
       cex = 2.0)
points(trans3d(y = mlts_3d[,'b'],
               x = rep(2, length(mlts_3d[,'b'])), 
               z = rep(-15, length(mlts_3d[,'b'])),          
               pmat = rr),
       col = 'seagreen',
       pch = 20,
       lwd = lw,
       cex = 2.0)
})

PNGsave(PlotName = 'MLE3D_2',
        Res = rs,
        Dim = dm_3d,
        Expr = 
          {
            par(mar = c(0,0,0,0))
rr = persp(x = condition_A, 
           y = condition_B, 
           # z = ll_mat,
           z = log_likelihood_NA,
           zlim = c(median(log_likelihood), max(log_likelihood)),
           theta = 30,
           phi = 30,
           expand = 0.5,
           col = adjustcolor("lightblue",alpha.f = 0.5),
           border = adjustcolor('blue', alpha.f = 0.5),
           xlab = "Condition A mean",
           ylab = "Condition B mean",
           zlab = "log(likelihood)",
           cex.axis = 0.8,
           ticktype = 'detailed')
points(trans3d(x = usteps['a',],
              y = usteps['b',], 
              z = -usteps['ll',],          
              pmat = rr),
      col = adjustcolor('red',0.3),
      type = 'b',
      pch = 19,
      lwd = arlw)

points(trans3d(x = m_mean_lst$a,
               y = m_mean_lst$b, 
               z = LikFun(m_mean_lst$a,
                          m_mean_lst$b),          
               pmat = rr),
       col = 'magenta',
       pch = 4,
       lwd = arlw,
       cex = 3)
points(trans3d(x = osteps['a',100],
               y = osteps['b',100], 
               z = -osteps['ll',100],          
               pmat = rr),
       col = 'red',
       pch = 3,
       lwd = arlw,
       cex = 3)
points(trans3d(x = mlts_3d[,'a'],
               y = rep(-2, length(mlts_3d[,'a'])), 
               z = rep(-15, length(mlts_3d[,'a'])),          
               pmat = rr),
       col = 'orange2',
       pch = 20,
       lwd = lw,
       cex = 2.0)
points(trans3d(y = mlts_3d[,'b'],
               x = rep(2, length(mlts_3d[,'b'])), 
               z = rep(-15, length(mlts_3d[,'b'])),          
               pmat = rr),
       col = 'seagreen',
       pch = 20,
       lwd = lw,
       cex = 2.0)
})

# Logit transform of sigmoid ---------------------------------------------
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
