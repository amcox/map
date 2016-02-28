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

# Elementary
df <- load_wide_map_data()
df <- subset(df, map.grade %in% 0:2)
df <- subset(df, subject %in% c("reading", "math"))
df <- subset(df, last.name != 'Sample')

df <- add_growth_columns_to_spring(df)

save_df_as_csv(df, 'map data for tif')

# High School
df <- load_wide_map_data()
df <- subset(df, map.grade > 3)
df <- subset(df, last.name != 'Sample')

df <- add_growth_columns_to_spring(df)

save_df_as_csv(df, 'hs map data for tif')
