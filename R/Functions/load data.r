load_wide_map_data <- function(dir=NULL) {
  d <- load_all_map_from_raw(dir)
  d.w <- make_wide_map_data(d)
  return(d.w)
}

load_all_map_from_raw <- function(dir=NULL) {
  filenames <- list.files(paste0("./../Data/Raw", dir), pattern=".csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv, head=TRUE, na.string=c(""), stringsAsFactors=F)
  res <- lapply(ldf, extract_map_fields)
  d <- rbind_all(res)
  return(d)
}

load_ges <- function() {
  d <- read.csv(file="./../Data/grade level equivs.csv", head=TRUE, na.string=c(""))
  return(d)
}

load_leap_data <- function() {
  d <- read.csv(file="./../Data/benchmark and leap data.csv", head=TRUE, na.string=c(""))
  names(d) <- tolower(names(d))
  return(d)
}

load_ps_hr_data <- function() {
  d <- read.csv(file="./../Data/ps home room data.csv", head=TRUE, na.string=c(""))
  names(d) <- tolower(names(d))
  return(d)
}