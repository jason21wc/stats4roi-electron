# SPC limit-calculation helpers ported from app_monolithic.R

# Lookup table function for c6 used for median standard deviations
c6 <- function(sample.size = 5) {
  c6_tab <- c(
    .674489524, .832554611, .888064165, .9160641325, .932894441, .944115161,
    .952126282, .958131091, .962798738, .966530795, .969582762, .972124923,
    .974275110, .976117460, .977713643, .979109892, .980341548, .981436090,
    .982415200, .983296223, .984093190, .984817583, .985478882, .986084984,
    .988487644, .990182489, .991442675, .992416146, .993190756, .993821792,
    .994345788, .994787849, .995165799, .995492642, .995778088, .996029532,
    .996252710, .996631404
  )
  c6_tab <- cbind(value = c6_tab, n = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 100))
  idx <- which(abs(c6_tab[, 2] - sample.size) == min(abs(c6_tab[, 2] - sample.size)))
  c6_tab[idx]
}

# Calculate the moving range across a designated span
MR_span <- function(data = NULL, span = 2) {
  n <- length(data)
  mr <- NULL
  loops <- seq(n - span + 1)
  for (i in loops) {
    low <- i
    high <- span + i - 1
    mr[i] <- max(data[low:high]) - min(data[low:high])
  }
  return(c(rep(NA, span - 1), mr))
}

