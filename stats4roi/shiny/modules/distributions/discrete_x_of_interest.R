# Tail-membership cutoffs for discrete X/R of interest.
# Include (1): keep the entered integer. Exclude (2): move the cutoff so that
# integer is not in that tail — lower/one-tail: X -> X-1 (X <= cutoff);
# upper two-tail: X -> X+1 (X >= cutoff).

adjust_discrete_x_of_interest <- function(x_l, low_inc, x_u = NULL, hi_inc = NULL,
                                          two_tails = FALSE) {
  if (!is.null(low_inc) && as.numeric(low_inc) == 2) {
    x_l <- x_l - 1
  }
  if (isTRUE(two_tails) && !is.null(hi_inc) && as.numeric(hi_inc) == 2) {
    x_u <- x_u + 1
  }
  list(x_l = x_l, x_u = x_u)
}

lookup_discrete_table_row <- function(dist_table, x) {
  idx <- x + 1
  list(
    x = x,
    p_at = dist_table$p.at.x[idx],
    p_and_below = dist_table$eq.and.below[idx],
    p_and_above = dist_table$eq.and.above[idx]
  )
}

discrete_x_of_interest_probs <- function(dist_table, x_l, low_inc,
                                        x_u = NULL, hi_inc = NULL,
                                        two_tails = FALSE) {
  adj <- adjust_discrete_x_of_interest(
    x_l, low_inc, x_u, hi_inc, two_tails = two_tails
  )
  lower <- lookup_discrete_table_row(dist_table, adj$x_l)
  if (!isTRUE(two_tails)) {
    return(list(lower = lower, upper = NULL, p_between = NULL, p_tails = NULL))
  }
  upper <- lookup_discrete_table_row(dist_table, adj$x_u)
  p_between <- 1 - lower$p_and_below - upper$p_and_above
  p_tails <- lower$p_and_below + upper$p_and_above
  list(lower = lower, upper = upper, p_between = p_between, p_tails = p_tails)
}
