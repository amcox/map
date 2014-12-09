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
df <- subset(df, !is.na(fall.rit))
df <- subset(df, !is.na(winter.rit))

df$fall.winter.rit.growth <- fall_winter_rit_growth(df)
df$fall.winter.rit.growth.dif <- fall_winter_rit_growth_dif(df)

d.sg <- df %.% group_by(school, map.grade) %.%
  summarize(perc.met.goal = length(fall.winter.rit.growth.dif[fall.winter.rit.growth.dif >= 0]) / length(fall.winter.rit.growth.dif),
    perc.50th.or.above = length(winter.percentile[winter.percentile >= 50]) / length(winter.percentile)
  )
d.s <- df %.% group_by(school) %.%
  summarize(perc.met.goal = length(fall.winter.rit.growth.dif[fall.winter.rit.growth.dif >= 0]) / length(fall.winter.rit.growth.dif),
    perc.50th.or.above = length(winter.percentile[winter.percentile >= 50]) / length(winter.percentile)
  )
d.s$map.grade <- rep('all', nrow(d.s))
d.g <- df %.% group_by(map.grade) %.%
  summarize(perc.met.goal = length(fall.winter.rit.growth.dif[fall.winter.rit.growth.dif >= 0]) / length(fall.winter.rit.growth.dif),
    perc.50th.or.above = length(winter.percentile[winter.percentile >= 50]) / length(winter.percentile)
  )
d.g$school <- rep('all', nrow(d.g))

d <- rbind(d.sg, d.s, d.g)

save_df_as_csv(d, 'map summary stats, winter')