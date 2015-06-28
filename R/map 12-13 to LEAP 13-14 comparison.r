library(ggplot2)
library(gdata)
library(RColorBrewer)
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

dm <- load_all_map_from_raw('/2013')
dm <- subset(dm, term == 'spring' & map.grade == 2)
dm$subject <- str_replace(dm$subject, 'reading', 'ela')
dm.s <- dm %>% select(id, map.grade, subject, rit, percentile)

dl <- load_leap_data()
dl <- subset(dl, test_name == 'L14' & achievement_level %in% c('U', 'AB', 'B', 'M', 'A'))
dl.s <- dl %>% select(subject, scaled_score, achievement_level, ai_points, on_level, grade, school, student_number)

d <- merge(dm.s, dl.s, by.x=c("id", "subject"), by.y=c("student_number", "subject"))

ggplot(d, aes(x=percentile))+
	geom_histogram()+
	facet_wrap(~achievement_level)
	
ggplot(d, aes(x=percentile, y=scaled_score))+
	geom_point()+
	geom_smooth(method='lm')+
	facet_wrap(~subject)
	
# Make text files of the LM summaries
d.e <- subset(d, subject == 'ela')
d.m <- subset(d, subject == 'math')
lm.e <- lm(d.e$percentile ~ d.e$scaled_score)
lm.m <- lm(d.m$percentile ~ d.m$scaled_score)

lm.file <- file("./../Output/MAP to iLEAP Linear Model Summary.txt", open = "wt")
sink(lm.file)
summary(lm.e)
summary(lm.m)
sink()
unlink(lm.file)

calc_basic_threshes <- function(d){
	find_percent_basic <- function(cut, data){
		mean(data[data$percentile >= cut,]$achievement_level %in% c('B', 'M', 'A'), na.rm=TRUE)
	}
	perciles <- unique(d$percentile)
	just.percs <- sapply(perciles, find_percent_basic, data=d)
	data.frame(map.percile=perciles, percent.cr=just.percs)
}

make_thresh_plot <- function(d, title) {
	ggplot(d, aes(x=map.percile, y=percent.cr))+
		geom_point()+
	  scale_y_continuous(breaks=seq(0,1,.05), label=percent)+
	  scale_x_continuous(breaks=seq(0,100,10))+
	  labs(title=title,
	        x="National Percentile on EOY 2nd Grade MAP",
	        y="Percent of Students at or Above the Percentile in 2nd Grade\nthat Scored Basic or Above on LEAP in 3rd Grade"
	  )+
		theme_bw()
}

threshes <- d.e %>% do(calc_basic_threshes(.))
p <- make_thresh_plot(threshes, 'ELA 2nd Grade MAP to 3rd Grade LEAP Scores')
save_plot_as_pdf(p, 'Thresholds for 2nd MAP to 3rd LEAP, ELA')

threshes <- d.m %>% do(calc_basic_threshes(.))
p <- make_thresh_plot(threshes, 'Math 2nd Grade MAP to 3rd Grade LEAP Scores')
save_plot_as_pdf(p, 'Thresholds for 2nd MAP to 3rd LEAP, Math')
