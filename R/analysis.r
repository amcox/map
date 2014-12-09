library(ggplot2)
library(gdata)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_all_map_from_raw()

d.a <- make_wide_map_data(df)

save_df_as_csv(d.a, 'map data fall')