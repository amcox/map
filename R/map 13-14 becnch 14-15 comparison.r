library(ggplot2)
library(gdata)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

dm <- load_all_map_from_raw('/2014')
dm <- subset(dm, term == 'spring')

dl <- load_leap_data()