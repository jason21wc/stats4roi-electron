# Override for cor.kendall.tau.simple() from lolcat package
# This corrects a bug in the original function
# Also override cor.kendall.tau() wrapper to ensure it uses our fixed version
#
# TO REMOVE THIS OVERRIDE WHEN lolcat IS UPDATED:
# 1. See: modules/statistical/correlation_association/utils/OVERRIDE_REMOVAL_GUIDE.md
# 2. Remove the source() statement from correlation_association_module.R (line ~18)
# 3. Test both "Enter Statistics" and "Use Data" modes with Test 3: Kendall's tau
# 4. Optionally delete or archive this file

#' Kendall's Tau 
#' 
#' Calculate Kendall's Rank Correlation Coefficient (also called Kendall's Tau).
#' 
#' @param x1 Vector - group 1 - scores 
#' @param x2 Vector - group 2 - scores
#' @param count.concordant Scalar - counts of concordant pairs between groups.
#' @param count.discordant Scalar - counts of discordant pairs between groups.
#' @param ties.x1 Vector - group 1 - score counts tied for a particular score.
#' @param ties.x2 Vector - group 2 - score counts tied for a particular score.
#' @param sample.size Scalar - sample size to use for the calculation.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @aliases cor.kendall.tau
#'
#' @return Hypothesis test result showing results of test.
cor.kendall.tau.simple <- function(
    count.concordant = 0
    ,count.discordant = 0
    ,ties.x1 = NA #vector of score count tied for particular score
    ,ties.x2 = NA #vector of score count tied for particular score
    ,sample.size = .5*(1+sqrt(8*(count.concordant+count.discordant) + 1)) #Number of subjects in x1 or x2
    ,alternative = c("two.sided", "greater", "less")
    ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  tau     <- NA
  z       <- NA
  p.value <- NA
  
  n <- sample.size
  
  t1 <- NA
  t2 <- NA
  
  # Fix: Handle case where ties.x1 or ties.x2 might be a single value or vector
  # Ensure all() returns a single logical value
  ties_x1_all_na <- if (length(ties.x1) == 1 && is.na(ties.x1)) TRUE else all(is.na(ties.x1))
  ties_x2_all_na <- if (length(ties.x2) == 1 && is.na(ties.x2)) TRUE else all(is.na(ties.x2))
  
  if (ties_x1_all_na && ties_x2_all_na) {
    tau <- (count.concordant - count.discordant)/(.5*n*(n-1))
  } else {
    S <- count.concordant - count.discordant
    
    d1 <-n*(n-1)
    
    if (!ties_x1_all_na) {
      t1 <- sum(sapply(ties.x1, FUN = function(x) {
        x^2 - x
      }))
    }
    
    if (!ties_x2_all_na) {
      t2 <- sum(sapply(ties.x2, FUN = function(x) {
        x^2 - x
      }))
    }
    
    tau <- 2*S/(sqrt(d1 - t1) * sqrt(d1 - t2))
    
  }
  
  z <- 3*tau*sqrt(n*(n-1))/sqrt(2*(2*n+5))
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  retval<-list(data.name   = "concordant pairs, discordant pairs, and ties",
               statistic   = z, 
               estimate    = c(tau = tau 
                               ,sample.size = n
                               ,count.concordant = count.concordant
                               ,count.discordant = count.discordant
                               #,Tx = t1
                               #,Ty = t2
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Kendall's Tau",
               conf.int    = c(NA, NA)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "tau"
  names(retval$parameter) <- "null hypothesis tau"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}

# Also override cor.kendall.tau() wrapper to ensure it uses our fixed version
# This is needed because "Use Data" mode calls cor.kendall.tau() which internally calls cor.kendall.tau.simple()
cor.kendall.tau <- function(x1, x2, alternative = c("two.sided", "greater", "less"), conf.level = 0.95) {
  # Calculate concordant and discordant pairs
  n <- length(x1)
  if (n != length(x2)) {
    stop("x1 and x2 must have the same length")
  }
  
  concordant <- 0
  discordant <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if ((x1[i] < x1[j] && x2[i] < x2[j]) || (x1[i] > x1[j] && x2[i] > x2[j])) {
        concordant <- concordant + 1
      } else if ((x1[i] < x1[j] && x2[i] > x2[j]) || (x1[i] > x1[j] && x2[i] < x2[j])) {
        discordant <- discordant + 1
      }
    }
  }
  
  # Calculate ties
  ties_x1 <- table(x1)
  ties_x2 <- table(x2)
  ties_x1_counts <- ties_x1[ties_x1 > 1]
  ties_x2_counts <- ties_x2[ties_x2 > 1]
  
  # Convert to vectors, use NA if no ties
  ties_x1_vec <- if (length(ties_x1_counts) > 0) as.numeric(ties_x1_counts) else NA
  ties_x2_vec <- if (length(ties_x2_counts) > 0) as.numeric(ties_x2_counts) else NA
  
  # Call our fixed version
  cor.kendall.tau.simple(
    count.concordant = concordant,
    count.discordant = discordant,
    ties.x1 = ties_x1_vec,
    ties.x2 = ties_x2_vec,
    sample.size = n,
    alternative = alternative,
    conf.level = conf.level
  )
}
