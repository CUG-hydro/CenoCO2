# MCMC diagnostics ----
pacman::p_load(
  R2jags, Ipaper,
  data.table, dplyr, magrittr
)

stats <- function(x) {
  q <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  min <- min(x)
  max <- max(x)
  mean <- mean(x)

  names <- c("q2.5", "q25", "q50", "q75", "q97.5", "min", "max", "mean")
  res <- c(q, min, max, mean) %>% set_names(names)
  exp(res)
}


process <- function(step = 100) {
  infile <- glue("bigout/postCeno{step}kyr.rda")
  outfile <- glue("out/{step}kyrCO2.csv")
  load(infile)

  ages.bin <- step / 1000
  ages.max <- 70
  ages <- agevec(ages.max, ages.bin)
  ages.len <- length(ages)
  ages <- ages[-length(ages)]

  ### Load data
  cp <- p$BUGSoutput$sims.list$pco2_m
  cp <- cp[, -(ncol(cp))]
  # cp <- cp[, -(1:4)]

  ### Stats for timeseries plot
  pts <- apply(cp, 2, stats) %>%
    t() %>%
    as.data.table() %>%
    cbind(age = ages, .) %>%
    relocate(age, "min", "q2.5", "q25", "q50", "mean", "q75", "q97.5", "max")
  fwrite(pts, outfile)
}

process(100)
process(200)
process(500)
