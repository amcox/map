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

df <- add_growth_columns_to_spring(df)

summary_func <- function(df) {
  summarize(df,
    perc.met.goal = length(fall.spring.rit.growth.dif[fall.spring.rit.growth.dif >= 0 & !is.na(fall.spring.rit.growth.dif)]) / sum(!is.na(fall.spring.rit.growth.dif)),
    perc.40th.or.above = length(spring.percentile[spring.percentile >= 40 & !is.na(spring.percentile)]) / sum(!is.na(spring.percentile)),
    perc.50th.or.above = length(spring.percentile[spring.percentile >= 50 & !is.na(spring.percentile)]) / sum(!is.na(spring.percentile)),
    avg.fall.winter.rit.growth = mean(fall.winter.rit.growth, na.rm=T),
    avg.fall.winter.rit.growth.dif = mean(fall.winter.rit.growth.dif, na.rm=T),
    avg.fall.winter.rit.growth.percent = mean(fall.winter.rit.growth.percent, na.rm=T),
    avg.winter.spring.rit.growth = mean(winter.spring.rit.growth, na.rm=T),
    avg.fall.spring.rit.growth = mean(fall.spring.rit.growth, na.rm=T),
    avg.fall.spring.rit.growth.dif = mean(fall.spring.rit.growth.dif, na.rm=T),
    avg.fall.spring.rit.growth.percent = mean(fall.spring.rit.growth.percent, na.rm=T),
    
    fall.winter.ggs.closed.percent = length(fall.winter.ggs[fall.winter.ggs == 'closed' & !is.na(fall.winter.ggs)]) / sum(!is.na(fall.winter.ggs)),
    fall.winter.ggs.none.percent = length(fall.winter.ggs[fall.winter.ggs == 'none' & !is.na(fall.winter.ggs)]) / sum(!is.na(fall.winter.ggs)),
    fall.winter.ggs.opened.percent = length(fall.winter.ggs[fall.winter.ggs == 'opened' & !is.na(fall.winter.ggs)]) / sum(!is.na(fall.winter.ggs)),
    
    fall.spring.ggs.closed.percent = length(fall.spring.ggs[fall.spring.ggs == 'closed' & !is.na(fall.spring.ggs)]) / sum(!is.na(fall.spring.ggs)),
    fall.spring.ggs.none.percent = length(fall.spring.ggs[fall.spring.ggs == 'none' & !is.na(fall.spring.ggs)]) / sum(!is.na(fall.spring.ggs)),
    fall.spring.ggs.opened.percent = length(fall.spring.ggs[fall.spring.ggs == 'opened' & !is.na(fall.spring.ggs)]) / sum(!is.na(fall.spring.ggs))
  )
}

d.sg <- df %.% group_by(school, map.grade, subject) %.% summary_func()
  
d.s <- df %.% group_by(school, subject) %.% summary_func()
d.s$map.grade <- rep('all', nrow(d.s))

d.g <- df %.% group_by(map.grade, subject) %.% summary_func()
d.g$school <- rep('all', nrow(d.g))

d <- rbind(d.sg, d.s, d.g)

save_df_as_csv(d, 'map summary stats 2014-15 spring')