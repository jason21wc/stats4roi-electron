# Override for cor.bowker.mcnemar.symmetry.1948() from lolcat package
# This corrects a bug in the original function
#
# TO REMOVE THIS OVERRIDE WHEN lolcat IS UPDATED:
# 1. Remove the source() statement from crosstabs_module.R
# 2. Test both "Manual" and "Use Data" modes with McNemar's Test/Symmetry Test
# 3. Optionally delete or archive this file

#' Modified to use McNemar's exact test when categories = 2
#' Calculates Bowker's Symmetry Test test for given contingency table.
#'
#' @param observed.frequencies A matrix of observed frequency values to test.
#' @param alternative The alternative hypothesis to use for the test computation.
#'
#' @return The results of the statistical test.
cor.bowker.mcnemar.symmetry.1948 <- function(
    observed.frequencies, #matrix
    alternative = c("greater", "two.sided", "less") #Paper identifies it as a one-tail (greater) test
) {
  validate.htest.alternative(alternative = alternative)
  
  # If only two categories, return McNemar's Exact
  if(nrow(observed.frequencies) == 2 && ncol(observed.frequencies) == 2){
    if(observed.frequencies[1,2] == 0 && observed.frequencies[2,1] == 0){#if zeroes it will throw an error
      retval <- list(
        data.name   = "off-diagonal 2x2 elements",
        statistic   = NA, 
        estimate    = c(b = 0, p_b = 0, c = 0, p_c = 0),
        parameter   = NA,
        p.value     = 1,
        null.value  = 0.5,
        alternative = 'two.sided',
        method      = "McNemar's Test for Dependent Proportions (exact)"
      )
      
      names(retval$null.value) <- "proportion"
      names(retval$parameter) <- "null hypothesis proportion"
      
      class(retval) <- "htest"
      return(retval)
    }
    return(proportion.test.mcnemar.simple(b = observed.frequencies[1,2], c = observed.frequencies[2,1]))
  }
  
  # Otherwise, use Bowker's chi-square approximate
  m <- nrow(observed.frequencies)
  df <- (m * (m - 1)) / 2
  
  chi.square.statistic <- 0
  
  for (i in 1:m) {
    for (j in 1:m) {
      if (i > j) {
        n_ij <- observed.frequencies[i, j]
        n_ji <- observed.frequencies[j, i]
        
        add_to <- ((n_ij - n_ji)^2 / (n_ij + n_ji))
        
        if (is.finite(add_to)) {
          chi.square.statistic <- chi.square.statistic + add_to
        }
      }
    }
  }
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp <- pchisq(chi.square.statistic, df)
    min(tmp, 1 - tmp) * 2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square.statistic, df, lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square.statistic, df, lower.tail = TRUE)
  } else {
    NA
  }
  
  retval <- list(
    data.name   = "observed frequencies",
    statistic   = chi.square.statistic, 
    parameter   = 0,
    p.value     = p.value,
    null.value  = 0,
    alternative = alternative[1],
    method      = "Bowker's Test of Symmetry (1948)"
  )
  
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "off-diagonal differences"
  names(retval$parameter) <- "null hypothesis off-diagonal differences"
  
  class(retval) <- "htest"
  retval
}

# Override the function in the lolcat namespace
# Note: The namespace uses "cor.bowker.symmetry.1948" (without "mcnemar")
assignInNamespace(x = "cor.bowker.symmetry.1948", 
                  value = cor.bowker.mcnemar.symmetry.1948, 
                  ns = "lolcat")
