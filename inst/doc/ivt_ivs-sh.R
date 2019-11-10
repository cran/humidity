## ------------------------------------------------------------------------
# load humidity package
library(humidity)

# display built-in guinea pig airborne influenza virus transmission data
str(ivt)

## ---- message=FALSE------------------------------------------------------
library(dplyr)
ivt <- ivt %>% 
  mutate(Tk = C2K(T), # tempature in Kelvin
         Es = SVP(Tk), # saturation vapor pressure in hPa
         E = WVP2(RH, Es), # partial water vapor pressure in Pa
         rho = AH(E, Tk), # absolute humidity in kg/m^3
         q = SH(E), # specific humidity in kg/kg
         omega = MR(q), # mixing ratio in kg/kg
         ) %>%
  mutate(PT = ifelse(PT == 0, 1, PT)) # add a small quantity to avoid taking log of zero
# check the calculation results
head(ivt)

## ------------------------------------------------------------------------
# log-linear regression of influenza virus airborne  transmission on specific humididty
loglm <- glm(PT ~ q, family = gaussian(link = "log"), data = ivt)
summary(loglm)

## ---- fig.width=5.5, fig.height=5.5, fig.align='center'------------------
# get fitted value to plot regression curve
ivt$fit.val <- exp(predict(loglm))
ivt <- ivt[with(ivt, order(q)), ]

# plot percentage viable virus vs. specific humidity
par(pty = "s")
plot(x = ivt$q, y = ivt$PT, col = "green", pch = 1, lwd = 3, 
     xaxt = "n", yaxt = "n", xlim = c(0, 0.025), ylim = c(0, 100), 
     xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
     main = "Influenza Virus Transmission\n Regression on Specific Humidity")
title(xlab = "Specific Humidity (kg/kg)", ylab = "Percent  Transmission (%)", mgp = c(2, 1, 0))
# plot regression curve
lines(x = ivt$q, y = ivt$fit.val, lty = "dashed", lwd = 4)
axis(side = 1, at = seq(0, 0.03, by = 0.01), labels = c("0", "0.01", "0.02", "0.03"), 
     tck = 0.01, padj = -0.5)
axis(side = 2, at = seq(0, 100, by = 20), tck = 0.01, las = 2, hadj = 0.5)
axis(side = 3, at = seq(0, 0.03, by = 0.01), labels = FALSE, tck = 0.01)
axis(side = 4, at = seq(0, 100, by = 20), labels = FALSE, tck = 0.01)
legend(0.011, 95, legend = c("Data", "p = 0.002"), pch = c(1, NA), 
       col = c("green", "black"), lty = c(NA, "dashed"), lwd = c(3, 4), seg.len = 5)

## ---- message=FALSE------------------------------------------------------
# display built-in 1-h influenza virus surival data
str(ivs)

ivs <- ivs %>% 
  mutate(Tk = C2K(T), # tempature in Kelvin
         Es = SVP(Tk), # saturation vapor pressure in hPa
         E = WVP2(RH, Es), # partial water vapor pressure in Pa
         rho = AH(E, Tk), # absolute humidity in kg/m^3
         q = SH(E), # specific humidity in kg/kg
         omega = MR(q), # mixing ratio in kg/kg
         )
# check the calculation results
head(ivs)

## ------------------------------------------------------------------------
# log-linear regression of 1-h infuenza A virus survival on specific humididty
loglm <- lm(log(PV) ~ q, data = ivs)
summary(loglm)

## ---- fig.width=5.5, fig.height=5.5, fig.align='center'------------------
# get fitted value to plot regression curve
ivs$fit.val <- exp(predict(loglm))
ivs <- ivs[with(ivs, order(q)), ]

# plot percentage viable virus vs. specific humidity
par(pty = "s")
plot(x = ivs$q, y = ivs$PV, col = "red", pch = 3, lwd = 3, 
     xaxt = "n", yaxt = "n", xlim = c(0, 0.03), ylim = c(0, 100), 
     xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
     main = "Influenza Virus Survival\n Regression on Specific Humidity")
title(xlab = "Specific Humidity (kg/kg)", ylab = "Percent  Viable (%)", mgp = c(2, 1, 0))
# plot regression curve
lines(x = ivs$q, y = ivs$fit.val, lty = "dashed", lwd = 4)
axis(side = 1, at = seq(0, 0.03, by = 0.01), labels = c("0", "0.01", "0.02", "0.03"), 
     tck = 0.01, padj = -0.5)
axis(side = 2, at = seq(0, 100, by = 20), tck = 0.01, las = 2, hadj = 0.5)
axis(side = 3, at = seq(0, 0.03, by = 0.01), labels = FALSE, tck = 0.01)
axis(side = 4, at = seq(0, 100, by = 20), labels = FALSE, tck = 0.01)
legend(0.011, 95, legend = c("1 Hour Viability", "p < 0.0001"), pch = c(3, NA), 
       col = c("red", "black"), lty = c(NA, "dashed"), lwd = c(3, 4), seg.len = 5)

