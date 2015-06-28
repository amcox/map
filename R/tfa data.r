library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# 14-15 STEP Data Eval Setting Plot
d <- load_wide_map_data()
d <- subset(d, map.grade %in% numeric.grades & subject %in% c('reading', 'math'))
# Change the col names in the PS file to "id", "home.room"
ps.hrs <- load_ps_hr_data()
d <- merge(d, ps.hrs)
d <- d %>% mutate(fall.spring.rit.growth = spring.rit - fall.rit, met.fall.spring.rit.growth.goal = fall.spring.rit.growth >= goal.fall.spring)

dg <- d %>% group_by(home.room, school, subject) %>% summarize(mean.fall.rit = mean(fall.rit, na.rm=T),
	mean.spring.rit = mean(spring.rit, na.rm=T)
)

save_df_as_csv(dg, 'tfa map data')