make_nce_from_percentile <- function(percentile.vec) {
  qnorm(percentile.vec/100)*21.06 + 50
}