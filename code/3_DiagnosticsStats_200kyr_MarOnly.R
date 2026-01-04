# MCMC diagnostics ----
library(R2jags)
load("bigout/postCeno200kyr.rda")

## Traceplots
# R2jags::traceplot(p, varname = "pco2_m")
# R2jags::traceplot(p, varname = c("pco2_m.pre", "pco2_m.eps.ac"))
# dev.off()

## Summary
View(p$BUGSoutput$summary)

# Process and save data ----
# source("code/Helpers.R")
## 500 kyr CO2 ----
### Set up ages vector
ages.bin <- 0.2
ages.max <- 70
ages <- agevec(ages.max, ages.bin)
ages.len <- length(ages)
ages <- ages[-length(ages)]

### Load data
cp <- p$BUGSoutput$sims.list$pco2_m
cp <- cp[, -(ncol(cp))]
# cp <- cp[, -(1:4)]

### Stats for timeseries plot
pts <- apply(cp, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
pts <- t(pts)
pts <- cbind(ages, pts)

### Save
write.csv(pts, "out/200kyrCO2.csv", row.names = FALSE)


### Load data - marine only
load("bigout/postCeno200kyr_MarOnly.rda")
cp <- p$BUGSoutput$sims.list$pco2_m
cp <- cp[, -(ncol(cp))]
# cp <- cp[, -(1:4)]

### Stats for timeseries plot
pts <- apply(cp, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
pts <- t(pts)
pts <- cbind(ages, pts)

# ### Save
write.csv(pts, "out/200kyrCO2MarOnly.csv", row.names = FALSE)

