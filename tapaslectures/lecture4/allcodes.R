library(micEcon); library(psych); library(lmtest); library(car); library(miscTools)
options(scipen = 999)
data( "appleProdFr86", package = "micEcon" )
help("appleProdFr86")
dat <- appleProdFr86
rm(appleProdFr86)
head(dat, 5)  # A truncated preview of the data set
describe(dat)
# Generate input quantities
dat$qCap <- dat$vCap / dat$pCap
dat$qLab <- dat$vLab / dat$pLab
dat$qMat <- dat$vMat / dat$pMat
#
# Creating quantity indices
dat$XP <- with( dat, ( vCap + vLab + vMat ) / ( mean( qCap ) * pCap + mean( qLab ) * pLab + mean( qMat ) * pMat ) ) # Paasche Index
dat$XL <- with( dat, ( qCap * mean( pCap ) + qLab * mean( pLab ) + qMat * mean( pMat ) ) / ( mean( qCap ) * mean( pCap ) + mean( qLab ) * mean( pLab ) + mean( qMat ) * mean( pMat ) ) ) # Laspeyres Index
dat$X <- sqrt( dat$XP * dat$XL ) # Fisher Index
# You can also generate these indices directly using micEconIndex package
#
# Measuring (partial) average product
dat$apCap <- dat$qOut / dat$qCap
dat$apLab <- dat$qOut / dat$qLab
dat$apMat <- dat$qOut / dat$qMat
hist( dat$apCap )
hist( dat$apLab )
hist( dat$apMat )
#
# Plotting average partial productivity of one input against another across firms
plot( dat$apCap, dat$apLab )
plot( dat$apCap, dat$apMat )
plot( dat$apLab, dat$apMat )
#
# Plotting partial average products against output
plot( dat$qOut, dat$apCap, log = "x" )
plot( dat$qOut, dat$apLab, log = "x" )
plot( dat$qOut, dat$apMat, log = "x" )
#
# Measuring total factor productivity
dat$tfp <- dat$qOut / dat$X # using Fisher index
dat$tfpP <- dat$qOut / dat$XP # using Paasche Index
dat$tfpL <- dat$qOut / dat$XL # using Laspeyres Index
hist( dat$tfp )
#
# Plotting tfp against output and input quantity index
plot( dat$qOut, dat$tfp, log = "x" )
plot( dat$X, dat$tfp, log = "x" )
#
#Does advisory service (a dummy) affects tfp?
boxplot( tfp ~ adv, data = dat )
boxplot( log(qOut) ~ adv, data = dat )
#
# Fitting a linear model
prodlinear <- lm( qOut ~ qCap + qLab + qMat, data = dat )
summary( prodlinear )
#
# Predicted vs. observed plot
dat$qOutLin <- fitted( prodlinear )
compPlot( dat$qOut, dat$qOutLin )
#
# Predicted vs. observed (logarithmic scaling of axes)
table( dat$qOutLin >= 0 )
compPlot( dat$qOut[ dat$qOutLin > 0 ], dat$qOutLin[ dat$qOutLin > 0 ], log = "xy" )
#
# Caclulate output elasticity, compute mean and meadian, plot histograms
dat$eCap <- coef(prodlinear)["qCap"] / dat$apCap
dat$eLab <- coef(prodlinear)["qLab"] / dat$apLab
dat$eMat <- coef(prodlinear)["qMat"] / dat$apMat
colMeans( subset( dat, , c( "eCap", "eLab", "eMat" ) ) )
colMedians( subset( dat, , c( "eCap", "eLab", "eMat" ) ) )
hist( dat$eCap , 20)
sum( dat$eCap > 1)
hist( dat$eLab , 20)
sum( dat$eLab > 1)
hist( dat$eMat , 20)
sum( dat$eMat > 1)
sum( dat$eCap > 1) + sum( dat$eLab > 1) + sum( dat$eMat > 1)
#
# Calculate elasticity of scale, compute mean and median, plot histograms, both based on observed average productivity and predicted average productivity
dat$eScale <- with( dat, eCap + eLab + eMat )
colMeans( subset( dat, , c( "eScale" ) ) )
colMedians( subset( dat, , c( "eScale" ) ) )
hist( dat$eScale, 30)
sum( dat$eScale > 2)
#
# Plot elasticity of scale against input index and output index
plot( dat$qOut, dat$eScale, log = "x" )
abline( 1, 0 )
plot( dat$X, dat$eScale, log = "x" )
abline( 1, 0 )
#
# Calculate MRTS
mrtsCapLab <- - coef(prodlinear)["qLab"] / coef(prodlinear)["qCap"]
mrtsLabCap <- - coef(prodlinear)["qCap"] / coef(prodlinear)["qLab"]
mrtsCapMat <- - coef(prodlinear)["qMat"] / coef(prodlinear)["qCap"]
mrtsMatCap <- - coef(prodlinear)["qCap"] / coef(prodlinear)["qMat"]
mrtsLabMat <- - coef(prodlinear)["qMat"] / coef(prodlinear)["qLab"]
mrtsMatLab <- - coef(prodlinear)["qLab"] / coef(prodlinear)["qMat"]
# Calculate RMRTS
dat$rmrtsCapLab <- - dat$eLab / dat$eCap
dat$rmrtsLabCap <- - dat$eCap / dat$eLab
dat$rmrtsCapMat <- - dat$eMat / dat$eCap
dat$rmrtsMatCap <- - dat$eCap / dat$eMat
dat$rmrtsLabMat <- - dat$eMat / dat$eLab
dat$rmrtsMatLab <- - dat$eLab / dat$eMat
# Draw histogram of RMRTS
hist( dat$rmrtsCapLab, 20 )
hist( dat$rmrtsLabCap, 20 )
hist( dat$rmrtsCapMat, 20 )
hist( dat$rmrtsMatCap, 20 )
hist( dat$rmrtsLabMat, 20 )
hist( dat$rmrtsMatLab, 20 )
#
# Calculate MVP, plot MVP against input prices
dat$mvpCap <- dat$pOut * coef(prodlinear)["qCap"]
dat$mvpLab <- dat$pOut * coef(prodlinear)["qLab"]
dat$mvpMat <- dat$pOut * coef(prodlinear)["qMat"]
compPlot( dat$pCap, dat$mvpCap, log = "xy" )
compPlot( dat$pLab, dat$mvpLab, log = "xy" )
compPlot( dat$pMat, dat$mvpMat, log = "xy" )
#
# Draw histogram of input price ratios
hist( dat$pCap / dat$pLab )
abline( v = - mrtsLabCap, lwd = 3  )
hist( dat$pCap / dat$pMat )
abline( v = - mrtsMatCap, lwd = 3  )
hist( dat$pLab / dat$pMat )
abline( v = - mrtsMatLab, lwd = 3  )
hist( dat$pLab / dat$pCap )
abline( v = - mrtsCapLab, lwd = 3  )
hist( dat$pMat / dat$pCap )
abline( v = - mrtsCapMat, lwd = 3  )
hist( dat$pMat / dat$pLab )
abline( v = - mrtsLabMat, lwd = 3  )
#
# Fitting a linear model
prodCD <- lm( log( qOut ) ~ log( qCap ) + log( qLab ) + log( qMat ), data = dat )
summary( prodCD )
# Predicted vs. observed plot
dat$qOutCD <- exp( fitted( prodCD ) )
compPlot( dat$qOut, dat$qOutCD , log = "xy" )
#
# Compute marginal products
dat$mpCapCD <- coef(prodCD)["log(qCap)"] * dat$qOut / dat$qCap
dat$mpLabCD <- coef(prodCD)["log(qLab)"] * dat$qOut / dat$qLab
dat$mpMatCD <- coef(prodCD)["log(qMat)"] * dat$qOut / dat$qMat
# Draw histograms
hist( dat$mpCapCD )
hist( dat$mpLabCD )
hist( dat$mpMatCD )
#
# Calculate MRTS and draw histograms
dat$mrtsCapLabCD <- - dat$mpLabCD / dat$mpCapCD
dat$mrtsMatCapCD <- - dat$mpCapCD / dat$mpMatCD
dat$mrtsMatLabCD <- - dat$mpLabCD / dat$mpMatCD
hist( dat$mrtsCapLabCD )
hist( dat$mrtsMatCapCD )
hist( dat$mrtsMatLabCD )
#
# Calculate RMRTS
rmrtsCapLabCD <- - coef(prodCD)["log(qLab)"] / coef(prodCD)["log(qCap)"]
rmrtsCapLabCD
rmrtsMatCapCD <- - coef(prodCD)["log(qCap)"] / coef(prodCD)["log(qMat)"]
rmrtsMatCapCD 
rmrtsMatLabCD <- - coef(prodCD)["log(qLab)"] / coef(prodCD)["log(qMat)"]
rmrtsMatLabCD
#
# Calculate MVP, plot MVP against input prices
dat$mvpCapCd <- dat$pOut * dat$mpCapCD
dat$mvpLabCd <- dat$pOut * dat$mpLabCD
dat$mvpMatCd <- dat$pOut * dat$mpMatCD
compPlot( dat$pCap, dat$mvpCap, log = "xy" )
compPlot( dat$pLab, dat$mvpLab, log = "xy" )
compPlot( dat$pMat, dat$mvpMat, log = "xy" )
#
# Plot MRTS against input price ratios
compPlot( dat$pLab / dat$pCap, - dat$mrtsCapLabCD, log = "xy" )
compPlot( dat$pCap / dat$pMat, - dat$mrtsMatCapCD, log = "xy" )
compPlot( dat$pLab / dat$pMat, - dat$mrtsMatLabCD, log = "xy" )
#
# Linear model: R-square of y
summary(prodlinear)$r.squared
# Cobb-Douglas: Hypothetical R-square of y
rSquared( dat$qOut, dat$qOut - dat$qOutCD )
# Linear model: Hypothetical R-square of ln(y)
rSquared( log( dat$qOut[ dat$qOutLin > 0 ] ), log( dat$qOut[ dat$qOutLin > 0 ] ) - log( dat$qOutLin[ dat$qOutLin > 0 ] ) )
# Cobb-Douglas: R-square of ln(y)
summary(prodCD)$r.squared
#
# RESET test
resettest( prodlinear )
resettest( prodCD )
#
# Estiamting cost function
dat$cost <- with( dat, vCap + vLab + vMat )
costCD <- lm( log( cost ) ~ log( pCap ) + log( pLab ) + log( pMat ) + log( qOut ), data = dat )
summary( costCD )
# Liner homogeneity condition check
linearHypothesis( costCD, "log(pCap) + log(pLab) + log(pMat) = 1"  )
# Estimation with linear homogeneity in input prices imposed
costCDHom <- lm( log( cost / pMat ) ~ log( pCap / pMat ) + log( pLab / pMat ) +  log( qOut ), data = dat )
summary( costCDHom )
# Likelihood ratio test
lrtest( costCDHom, costCD )
# Predicting total cost based on homogeneity imposed model
dat$costCDHom <- exp( fitted( costCDHom ) ) * dat$pMat
# Storing coeffs from the homogeneity-imposed model
chCap <- coef( costCDHom )[ "log(pCap/pMat)" ]
chLab <- coef( costCDHom )[ "log(pLab/pMat)" ]
chMat <- 1 - chCap - chLab
# Testing Shepherd's lemma
compPlot( chCap * dat$costCDHom / dat$pCap, dat$qCap, log = "xy" )
compPlot( chLab * dat$costCDHom / dat$pLab, dat$qLab, log = "xy" )
compPlot( chMat * dat$costCDHom / dat$pMat, dat$qMat, log = "xy" )
#
# Histogram of cost shares
hist( dat$pCap * dat$qCap / dat$cost ) 
abline(v=chCap,lwd=3 )
hist( dat$pLab * dat$qLab / dat$cost ) 
abline(v=chLab,lwd=3 )
hist( dat$pMat * dat$qMat / dat$cost ) 
abline(v=chMat,lwd=3 )
#
# Estimating profit funciton
dat$profit <- with( dat, pOut * qOut - cost )
hist( dat$profit, 30 )
plot( dat$qCap, dat$profit, log = "xy" )
# Considering capital as a fixed input, deriving a short-term profit function
dat$vProfit <- with( dat, pOut * qOut - vLab - vMat )
hist( dat$vProfit, 30 )
plot( dat$qCap, dat$vProfit, log = "xy" )
#
# Filter out rows with negative or zero values in relevant columns
dat_clean <- subset(dat, profit > 0 & pOut > 0 & pCap > 0 & pLab > 0 & pMat > 0)
#
# Fitting a Cobb-Douglas form
profitCD <- lm( log( profit ) ~ log( pOut ) + log( pCap ) + log( pLab ) + log( pMat ), data = dat_clean )
summary( profitCD )
# Linear hypothesis test
linearHypothesis( profitCD, "log(pOut) + log(pCap) + log(pLab) + log(pMat) = 1" )
# Estimating Cobb-Douglas with linear homogeneity imposed
profitCDHom <- lm( log( profit / pOut ) ~ log( pCap / pOut ) + log( pLab / pOut ) + log( pMat / pOut ), data = dat_clean )
summary( profitCDHom )
# Likelihood ratio test
lrtest( profitCD, profitCDHom )
# Storing coeffs from the homegeneity-imposed model
ghCap <- coef( profitCDHom )["log(pCap/pOut)"]
ghLab <- coef( profitCDHom )["log(pLab/pOut)"]
ghMat <- coef( profitCDHom )["log(pMat/pOut)"]
ghOut <- 1- ghCap - ghLab - ghMat
#
# Testing Hotelling's lemma
hist( ( dat$pOut * dat$qOut / dat$profit )[ dat$profit > 0 ], 30 ) >abline(v=ghOut,lwd=3 )
hist( ( - dat$pCap * dat$qCap / dat$profit )[ dat$profit > 0 ], 30 ) >abline(v=ghCap,lwd=3 )
hist( ( - dat$pLab * dat$qLab / dat$profit )[ dat$profit > 0 ], 30 ) >abline(v=ghLab,lwd=3 )
hist( ( - dat$pMat * dat$qMat / dat$profit )[ dat$profit > 0 ], 30 ) >abline(v=ghMat,lwd=3 )