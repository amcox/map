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

df <- load_wide_map_data()

save_df_as_csv(subset(df, subject == 'reading'), "map for americorps, reading")
save_df_as_csv(subset(df, subject == 'math'), "map for americorps, math")