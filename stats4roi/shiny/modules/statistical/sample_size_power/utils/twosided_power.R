# Correct two-sided power when only one rejection tail is counted (lolcat / legacy pattern).
# beta = P(critical_low <= Z <= critical_high | ncp)
# power = 1 - beta

power_z_twosided <- function(ncp, alpha = 0.05) {
  zl <- stats::qnorm(alpha / 2)
  zu <- stats::qnorm(1 - alpha / 2)
  beta <- stats::pnorm(zu, mean = ncp, sd = 1, lower.tail = TRUE) -
    stats::pnorm(zl, mean = ncp, sd = 1, lower.tail = TRUE)
  1 - beta
}

power_t_twosided <- function(ncp, df, alpha = 0.05) {
  tl <- stats::qt(alpha / 2, df)
  tu <- stats::qt(1 - alpha / 2, df)
  beta <- stats::pt(tu, df = df, ncp = ncp, lower.tail = TRUE) -
    stats::pt(tl, df = df, ncp = ncp, lower.tail = TRUE)
  1 - beta
}

twosided_scalar_power <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  if (is.numeric(x) && length(x) == 1) {
    return(as.numeric(x))
  }
  if (is.data.frame(x)) {
    if ("power" %in% names(x)) {
      p <- x[["power"]]
      if (is.numeric(p) && length(p) >= 1) {
        return(as.numeric(p[1]))
      }
    }
    if ("power" %in% rownames(x)) {
      return(as.numeric(x["power", 1]))
    }
  }
  NA_real_
}

patch_power_result <- function(out, power_val) {
  if (is.null(out)) {
    return(out)
  }
  if (is.data.frame(out)) {
    if ("power" %in% names(out)) {
      out[["power"]] <- power_val
    } else if ("power" %in% rownames(out)) {
      out["power", ] <- power_val
    }
    if ("beta" %in% names(out)) {
      out[["beta"]] <- 1 - power_val
    } else if ("beta" %in% rownames(out)) {
      out["beta", ] <- 1 - power_val
    }
    return(out)
  }
  power_val
}

power_twosided_via_onesided <- function(fn, args) {
  alpha <- args$alpha
  args_base <- args[setdiff(names(args), c("alternative", "details", "alpha"))]
  pg <- do.call(fn, c(args_base, list(alternative = "greater", alpha = alpha / 2, details = TRUE)))
  pl <- do.call(fn, c(args_base, list(alternative = "less", alpha = alpha / 2, details = TRUE)))
  twosided_scalar_power(pg) + twosided_scalar_power(pl)
}

apply_twosided_power_fix <- function(out, power_val) {
  patch_power_result(out, power_val)
}

mean_z_ncp <- function(effect.size, sample.size, variance) {
  abs(effect.size) * sqrt(sample.size / variance)
}

mean_t_ncp_onesample <- function(effect.size, sample.size, variance.est) {
  abs(effect.size) * sqrt(sample.size / variance.est)
}

mean_t_ncp_twosample_equal <- function(effect.size, variance.est, n1, n2) {
  abs(effect.size) / sqrt(variance.est * (1 / n1 + 1 / n2))
}

fisher_z_ncp_onesample <- function(r_null, r_alt, sample.size) {
  z0 <- 0.5 * log((1 + r_null) / (1 - r_null))
  z1 <- 0.5 * log((1 + r_alt) / (1 - r_alt))
  (z1 - z0) * sqrt(sample.size - 3)
}

proportion_z_ncp <- function(p_null, p_alt, sample.size) {
  abs(p_alt - p_null) / sqrt(p_null * (1 - p_null) / sample.size)
}

poisson_z_ncp_onesample <- function(lambda_null, lambda_alt, sample.size) {
  2 * sqrt(sample.size) * (sqrt(lambda_alt) - sqrt(lambda_null))
}

poisson_z_ncp_twosample <- function(lambda_1, lambda_2, n1, n2) {
  (sqrt(lambda_2) - sqrt(lambda_1)) / (0.5 * sqrt(1 / n1 + 1 / n2))
}

.needs_lolcat_twosided_fix <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    if (!requireNamespace("lolcat", quietly = TRUE)) {
      cache <<- FALSE
      return(FALSE)
    }
    cache <<- tryCatch(
      {
        r <- lolcat::power.mean.z.onesample(
          sample.size = 100,
          effect.size = 0,
          variance = 16.9^2,
          alpha = 0.05,
          alternative = "two.sided",
          details = TRUE
        )
        p <- twosided_scalar_power(r)
        is.finite(p) && abs(p - 0.025) < 0.005
      },
      error = function(e) FALSE
    )
    cache
  }
})

lolcat_power_with_twosided_fix <- function(fn, args, fix_fn = NULL, fallback_sum_halves = TRUE) {
  alt <- args$alternative
  if (is.null(alt) || length(alt) != 1L) {
    alt <- "two.sided"
  }
  out <- do.call(fn, c(args, list(details = TRUE)))
  if (alt != "two.sided" || !isTRUE(.needs_lolcat_twosided_fix())) {
    return(out)
  }
  power_val <- if (!is.null(fix_fn)) {
    fix_fn(args)
  } else if (isTRUE(fallback_sum_halves)) {
    power_twosided_via_onesided(fn, args)
  } else {
    twosided_scalar_power(out)
  }
  apply_twosided_power_fix(out, power_val)
}
