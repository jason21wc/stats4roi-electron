# Exponential Test Functions for stats4ROI
# These functions replicate the exponential testing functionality from the original app

# Shapiro-Wilk Exponential Test (replicating app.R lines 29048-29114)
shapiro.exp.test <- function(x, bail = 20000, nrepl = 100000, session = NULL) {
  # from package exptest, modified to give two-tailed p-value and have early bail
  # also modified to use lolcat's shapiro.wilk.exponentiality.test() if n<=100
  x <- na.omit(x)
  DNAME <- deparse(substitute(x))
  l <- 0
  n <- length(x)
  x <- sort(x)
  y <- mean(x)
  w <- n * (y - x[1])^2 / ((n - 1) * sum((x - y)^2))
  update_inc <- nrepl / 100
  
  # look up if n <= 100
  if (n <= 100) {
    temp <- shapiro.wilk.exponentiality.test(x = x)
    
    prop.above <- NA
    prop.below <- NA
    p.value <- temp[["p.value"]]
    RVAL <- list(statistic = c(W = temp[["statistic"]][["W"]]), 
                 p.value = p.value, 
                 method = "Lookup", 
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  for (i in 1:bail) {
    s <- rexp(n)
    s <- sort(s)
    y <- sum(s) / n  # y <- mean(s)
    # W <- n*(y-s[1])^2/((n-1)*sum((s-y)^2))
    W <- n * (y - s[1]) * (y - s[1]) / ((n - 1) * sum((s - y) * (s - y)))
    if (W < w) l <- l + 1
    if (i / update_inc == trunc(i / update_inc)) {  # only update a few times
      if (!is.null(session)) {
        updateProgressBar(id = "mvp_exp_prog", value = i, total = nrepl, session = session)
      }
    }
  }
  
  if (l == 0 || l == bail) {
    RVAL <- list(statistic = c(W = w), 
                 p.value = 0, 
                 method = "Monte Carlo", 
                 data.name = DNAME)
    class(RVAL) <- "htest"
    if (!is.null(session)) {
      updateProgressBar(id = "mvp_exp_prog", value = 100, session = session)
    }
    return(RVAL)
  }
  
  for (i in (bail + 1):nrepl) {
    s <- rexp(n)
    s <- sort(s)
    y <- sum(s) / n  # y <- mean(s)
    # W <- n*(y-s[1])^2/((n-1)*sum((s-y)^2))
    W <- n * (y - s[1]) * (y - s[1]) / ((n - 1) * sum((s - y) * (s - y)))
    
    if (W < w) l <- l + 1
    if (i / update_inc == trunc(i / update_inc)) {  # only update a few times
      if (!is.null(session)) {
        updateProgressBar(id = "mvp_exp_prog", value = i, total = nrepl, session = session)
      }
    }
  }
  prop.above <- l / nrepl
  prop.below <- (nrepl - l) / nrepl
  p.value <- min(prop.above, prop.below) * 2  # two-sided
  RVAL <- list(statistic = c(W = w), 
               p.value = p.value, 
               method = "Shapiro-Wilk test for exponentiality", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

# MVP Exponential Test (replicating app.R lines 28999-29046)
mvp_exp <- function(x, bail = 20000, max_sims = 100000, session = NULL) {
  # Monte Carlo approach to exponential distribution testing by Michael V. Petrovich
  # The expected value of mvp_e is 1 if the distribution is exponentially distributed
  # mvp_rsd is the random sampling distribution of the MVP(E) statistic
  # filter out na
  x <- na.omit(x)
  samp_mean <- mean(x)
  samp_var <- var(x)
  samp_min <- min(x)
  mvp_e <- (samp_mean - samp_min)^2 / samp_var
  n <- length(x)
  mvp_rsd <- rep(NA, max_sims)
  update_inc <- max_sims / 100
  
  # early bail if it is clearly not exponential
  for (i in 1:bail) {
    # take a sample of size n from theoretical exp dist
    sim <- samp_min + rexp(n = n, rate = samp_mean^-1)
    sum_x <- sum(sim)
    sim_mvp <- ((sum_x / n) - min(sim)) * ((sum_x / n) - min(sim)) / 
      {{sum(sim * sim) - sum_x * sum_x / n} / (n - 1)}  # crazy math speeds it up
    mvp_rsd[i] <- sim_mvp
    if (i / update_inc == trunc(i / update_inc)) {  # only update a few times
      if (!is.null(session)) {
        updateProgressBar(id = "mvp_exp_prog", value = i, total = max_sims, session = session)
      }
    }
  }
  if (length(mvp_rsd[na.omit(mvp_rsd) > mvp_e]) == 0 || 
      length(mvp_rsd[na.omit(mvp_rsd) > mvp_e]) == bail) {
    if (!is.null(session)) {
      updateProgressBar(id = "mvp_exp_prog", value = 100, session = session)
    }
    return(list("MVP(E) = " = mvp_e, "p-value" = 0))
  }
  
  # continue
  for (i in (bail + 1):max_sims) {
    # take a sample of size n from theoretical exp dist
    sim <- samp_min + rexp(n = n, rate = samp_mean^-1)
    sum_x <- sum(sim)
    sim_mvp <- ((sum_x / n) - min(sim)) * ((sum_x / n) - min(sim)) / 
      {{sum(sim * sim) - sum_x * sum_x / n} / (n - 1)}  # crazy math speeds it up
    mvp_rsd[i] <- sim_mvp
    if (i / update_inc == trunc(i / update_inc)) {  # only update a few times
      if (!is.null(session)) {
        updateProgressBar(id = "mvp_exp_prog", value = i, total = max_sims, session = session)
      }
    }
  }
  
  prop_above <- length(mvp_rsd[mvp_rsd > mvp_e]) / max_sims
  prop_below <- length(mvp_rsd[mvp_rsd < mvp_e]) / max_sims
  prop <- min(prop_above, prop_below)
  output <- list("MVP(E) = " = mvp_e, "p-value" = prop * 2)
  return(output)
}

# Anderson-Darling Exponential Test (using agop package function)
exp_test_ad <- function(x) {
  # This function uses the agop package's exp_test_ad function
  # The original app uses require(agop) and calls exp_test_ad directly
  if (requireNamespace("agop", quietly = TRUE)) {
    return(agop::exp_test_ad(x))
  } else {
    # Fallback implementation if agop is not available
    stop("agop package is required for exp_test_ad function")
  }
}
