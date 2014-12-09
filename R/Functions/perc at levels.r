find_perc_at_levels <- function(d, col) {
  out <- data.frame(prop.table(table(d[col])))
  names(out) <- c('level', 'percent')
  out
}

find_perc_on_or_above <- function(d, col) {
  out <- data.frame(prop.table(table(d[col])))
  names(out) <- c('level', 'percent')
  out
}