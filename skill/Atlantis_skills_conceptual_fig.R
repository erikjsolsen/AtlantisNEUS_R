# sample plots to visualize different measures of skill

#User parameters
window <- T
if(window == T){
  r.dir    <- "L:\\Rworkspace\\"
  data.dir <- "L:\\ATLANTIS\\Skill_comparisons\\"
  out.dir  <- "L:\\ATLANTIS\\Skill_comparisons\\"
  memory.limit(4000)
}
if(window == F){
  r.dir    <- "slucey/Rworkspace/"
  data.dir <- "slucey/ATLANTIS/Skill_comparisons/"
  out.dir  <- "slucey/ATLANTIS/Skill_comparisons/"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table)

#-------------------------------------------------------------------------------
#User created functions

# following from http://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/
# Function that returns Root Mean Squared Error
rmse <- function(error) sqrt(mean(error^2))

# Function that returns Mean Absolute Error (SAME AS AAE FOR US)
aae <- function(error) mean(abs(error))

# Function that returns Mean Absolute Error (SAME AS AAE FOR US)
ae <- function(error) mean(error)

# Function that returns Modeling Efficiency MEF
mef <- function(obs, error){
  obsavg <- mean(obs)
  obserr <- obs - obsavg
  out <- (sum(obserr^2) - sum(error^2)) / sum(obserr^2)
  return(out)
}

myplot <- function(x, y, mycol = 'darkblue', mycex = 1.5, ...){
  plot(x, y, col = mycol, cex = mycex, xlab = '', ylab = '', axes = F, ...)
} 
myline <- function(x, y, mylwd = 2, mycol = 'skyblue', ...){
  lines(x, y, lwd = mylwd, col = mycol, ...)
} 
mybox  <- function(blwd = 2, ...) box(lwd = blwd, ...)
#-------------------------------------------------------------------------------
# correlations--time series and correlation plot
# correlation of 1 is best for us. 0 and -1 are poor fits

# generate fake time series, observed and modeled
set.seed(123)
obs <- (rnorm(50))
mod <- (rnorm(50))

# par(mfcol = c(2, 4))
# 
# plot(obs, main="time series, correlation = 1")
# lines(obs)
# plot(obs, obs, main="correlation = 1")
# 
# plot(obs, ylim=c(-3,8), main="time series, correlation = 1")
# lines(6+obs)
# #abline(h=mean(obs))
# #abline(h=mean(6+obs))
# plot(obs, 6+obs, main="correlation = 1")
# 
# plot(obs, main="time series, correlation ~ 0")
# lines(mod)
# plot(obs, mod, main="correlation ~ 0")
# 
# plot(obs, main="time series, correlation = -1")
# lines(-obs)
# #abline(h=mean(obs))
# #abline(h=mean(-obs))
# plot(obs, -obs, main="correlation = -1")

fakedat <- as.data.table(cbind(obs, mod))

# calculate RMSE for perfect fit, poor fit
obserror     <- fakedat[, obs] - mean(fakedat[, obs])
perfect      <- fakedat[, obs] - fakedat[, obs]
scaleoff     <- fakedat[, obs] - (6 + fakedat[, obs])
perfectlyoff <- fakedat[, obs] - (-1 * fakedat[, obs])
awful        <- fakedat[, obs] - fakedat[, mod]
awfuloff     <- fakedat[, obs] - (6 + fakedat[, mod])
trend        <- seq(3, -3, ((-3 - 3) / (50 - 1)))
awfultrend   <- fakedat[, obs] - (trend + fakedat[, mod])

# rmse(perfect)
# rmse(scaleoff)
# rmse(perfectlyoff)
# rmse(awful)
# rmse(awfuloff)
# rmse(awfultrend)
# 
# aae(perfect)
# aae(scaleoff)
# aae(perfectlyoff)
# aae(awful)
# aae(awfuloff)
# aae(awfultrend)
# 
# ae(perfect)
# ae(scaleoff)
# ae(perfectlyoff)
# ae(awful)
# ae(awfuloff)
# ae(awfultrend)
# 
# mef(fakedat[, obs], perfect)
# mef(fakedat[, obs], scaleoff)
# mef(fakedat[, obs], perfectlyoff)
# mef(fakedat[, obs], awful)
# mef(fakedat[, obs], awfuloff)
# mef(fakedat[, obs], awfultrend)
# 
# rmse(awful*2)
# rmse((awful)^3)
# rmse((awful)^10)
# rmse(fakedat$obs - (6+fakedat$mod))

# par(mfrow=c(3,2))
# plot(perfect, main="error series, RMSE = 0")
# plot(perfectlyoff, main="error series, RMSE ~ 2.2")
# plot(awful, main="error series, RMSE ~ 1.5")
# plot(awful*2, main="error series, RMSE ~ 3")
# plot(awful^3, main="error series, RMSE ~ 10")
# plot(awful^10, main="error series, RMSE ~ 36,000")


###########################
#  the plot

png(file = paste(out.dir, "Skills_conceptual_fig.png", sep = ''), 
                 height = 2000, width = 2500, res = 300)

par(mfcol = c(4, 6), oma = c(4, 4, 4, 1))

#Perfect Fit
par(mar = c(1, 0, 0, 0))
myplot(1:50, fakedat[, obs])
myline(1:50, fakedat[, obs])
mybox()
mtext(3, text = 'Perfect\nFit',  line = 0.25)
mtext(2, text = 'Observations', line = 1)

par(mar = c(0, 0, 0, 0))
myplot(fakedat[, obs], fakedat[, obs])
myline(fakedat[, obs], fakedat[, obs])
mybox()
mtext(2, text = 'Correlation', line = 1)
legend('topleft', legend = '1:1', lty = 1, lwd = 2, col = 'skyblue', bty = 'n')

myplot(1:50, perfect)
myline(1:50, perfect)
mybox()
mtext(2, text = 'RMSE / AAE / AE', line = 1)
legend('topleft', legend = '0', lty = 1, lwd = 2, col = 'skyblue', bty = 'n')

myplot(obserror^2, obserror^2 - perfect^2)
myline(obserror^2, obserror^2)
abline(h = 0, lwd = 2, lty = 2, col = 'skyblue')
mybox()
mtext(2, text = 'MEF', line = 1)
legend('topleft', legend = c('1:1', 0), lty = c(1, 2), lwd = 2, col = 'skyblue', 
       bty = 'n')

#Perfect Variation, Scale Off
par(mar = c(1, 0, 0, 0))
myplot(1:50, fakedat[, obs], ylim = c(min(fakedat[, obs]), max(fakedat[, obs]) + 6))
myline(1:50, fakedat[, obs] + 6)
mybox()
mtext(3, text = 'Perfect Variation\nScale Off', line = 0.25)

par(mar = c(0, 0, 0, 0))
myplot(fakedat[, obs], fakedat[, obs] + 6, ylim = c(0, 8))
myline(fakedat[, obs], fakedat[, obs])
mybox()

myplot(1:50, scaleoff, ylim = c(-7, 1))
myline(1:50, perfect)
mybox()

myplot(obserror^2, obserror^2 - scaleoff^2, ylim = c(-40, 10))
myline(obserror^2, obserror^2)
abline(h = 0, lwd = 2, lty = 2, col = 'skyblue')
mybox()

#Perfect Inverse Variation
par(mar = c(1, 0, 0, 0))
myplot(1:50, fakedat[, obs])
myline(1:50, fakedat[, obs] * -1)
mybox()
mtext(3, text = 'Perfect Inverse\nVariation', line = 0.25)

par(mar = c(0, 0, 0, 0))
myplot(fakedat[, obs], fakedat[, obs] * -1)
myline(fakedat[, obs], fakedat[, obs])
mybox()

myplot(1:50, perfectlyoff)
myline(1:50, perfect)
mybox()

myplot(obserror^2, obserror^2 - perfectlyoff^2, ylim = c(-40, 10))
myline(obserror^2, obserror^2)
abline(h = 0, lwd = 2, lty = 2, col = 'skyblue')
mybox()

#uncorrelated, correct scale
par(mar = c(1, 0, 0, 0))
myplot(1:50, fakedat[, obs])
myline(1:50, fakedat[, mod])
mybox()
mtext(3, text = 'Uncorrelated\nCorrect Scale', line = 0.25)

par(mar = c(0, 0, 0, 0))
myplot(fakedat[, obs], fakedat[, mod])
myline(fakedat[, obs], fakedat[, obs])
mybox()

myplot(1:50, awful)
myline(1:50, perfect)
mybox()

myplot(obserror^2, obserror^2 - awful^2, ylim = c(-10, 10))
myline(obserror^2, obserror^2)
abline(h = 0, lwd = 2, lty = 2, col = 'skyblue')
mybox()

#uncorrelated, scale off
par(mar = c(1, 0, 0, 0))
myplot(1:50, fakedat[, obs], ylim = c(-3, 8))
myline(1:50, fakedat[, mod] + 6)
mybox()
mtext(3, text = 'Uncorrelated\nScale Off', line = 0.25)

par(mar = c(0, 0, 0, 0))
myplot(fakedat[, obs], fakedat[, mod] + 6, ylim = c(0, 8))
myline(fakedat[, obs], fakedat[, obs])
mybox()

myplot(1:50, awfuloff, ylim = c(-9, 1))
myline(1:50, perfect)
mybox()

myplot(obserror^2, obserror^2 - awfuloff^2, ylim = c(-80, 10))
myline(obserror^2, obserror^2)
abline(h = 0, lwd = 2, lty = 2, col = 'skyblue')
mybox()

#uncorrelated, trend mismatch
par(mar = c(1, 0, 0, 0))
myplot(1:50, fakedat[, obs], ylim = c(-5, 5))
myline(1:50, trend + fakedat[, mod])
mybox()
mtext(3, text = 'Uncorrelated\ntrend mismatch', line = 0.25)

par(mar = c(0, 0, 0, 0))
myplot(fakedat[, obs], trend + fakedat[, mod])
myline(fakedat[, obs], fakedat[, obs])
mybox()

myplot(1:50, awfultrend)
myline(1:50, perfect)
mybox()

myplot(obserror^2, obserror^2 - awfultrend^2, ylim = c(-30, 10))
myline(obserror^2, obserror^2)
abline(h = 0, lwd = 2, lty = 2, col = 'skyblue')
mybox()

dev.off()
###################################

