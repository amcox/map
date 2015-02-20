library(gdata)
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
df <- subset(df, map.grade %in% numeric.grades)
df <- subset(df, subject %in% c("reading", "math"))

df <- add_growth_columns_to_winter(df)

save_df_as_csv(df, 'map data all')