# ANOVA Helper Functions
# Utility functions for ANOVA calculations

library(dplyr)
library(stringr)

# =============================================================================
# BALANCE TEST FUNCTION
# =============================================================================
# Check if a design is balanced (equal sample sizes across factor combinations)
# Extracted from app_monolithic.R lines 29863-29879
balance_test <- function(factors_names, data) {
  # Test to see if unbalanced from EMSaov
  if (is.null(factors_names) || is.null(data)) {
    return(FALSE)
  }
  
  EMSflag <- FALSE
  n.table <- NULL
  
  for(i in 1:length(factors_names)) {
    temp <- table(data[, factors_names[i]])
    if(sum(temp != mean(temp)) != 0) {
      EMSflag <- TRUE
    }
    n.table <- c(n.table, length(temp))
  }
  
  return(EMSflag)
}
