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

dm <- load_all_map_from_raw('/2014')
dm <- subset(dm, term == 'spring' & map.grade == 2)
dm$subject <- str_replace(dm$subject, 'reading', 'ela')
dm.s <- dm %>% select(id, school, subject, rit, percentile) %>% rename(school.map = school)

dl <- load_parc_data_15()
dl.al <- dl %>% select(id, school, ela.al, math.al, sci.al, soc.al) %>%
	gather(subject, al, -(id:school)) %>%
	separate(subject, into = c("subject", "del"), sep = "\\.") %>%
	select(-del)
dl.pts <- dl %>% select(id, ela.pts, math.pts, sci.pts, soc.pts) %>%
	gather(subject, pts, -id) %>%
	separate(subject, into = c("subject", "del"), sep = "\\.") %>%
	select(-del)
dl.ss <- dl %>% select(id, ela.ss, math.ss) %>%
	gather(subject, ss, -id) %>%
	separate(subject, into = c("subject", "del"), sep = "\\.") %>%
	select(-del)
dl.comb <- merge(dl.al, merge(dl.pts, dl.ss)) %>% rename(school.parc = school)

d <- merge(dm.s, dl.comb)

cuts <- load_parc_cuts_15()
	
p <- ggplot(d, aes(x=percentile, y=ss))+
	geom_point()+
	geom_smooth()+
	geom_hline(data=cuts, aes(yintercept=cut))+
  scale_x_continuous(breaks=seq(0,100,10))+
  labs(title="2013-14 EOY 2nd Grade MAP Predicting\nPerformance on 2014-15 3rd Grade PARC",
        x="National Percentile on EOY 2nd Grade MAP",
        y="2014-15 PARC 3rd Scaled Score"
  )+
	theme_bw()+
	facet_wrap(~subject)
save_plot_as_pdf(p, '13-14 2nd MAP vs 2014-15 3rd PARC')
	
calc_basic_and_mastery_threshes <- function(d){
	find_percent_basic <- function(cut, data){
		mean(data[data$percentile >= cut,]$al %in% c('B', 'M', 'A'), na.rm=TRUE)
	}
	find_percent_mast <- function(cut, data){
		mean(data[data$percentile >= cut,]$al %in% c('M', 'A'), na.rm=TRUE)
	}
	perciles <- unique(d$percentile)
	basic.percs <- sapply(perciles, find_percent_basic, data=d)
	mast.percs <- sapply(perciles, find_percent_mast, data=d)
	d.m <- data.frame(map.percile=perciles, perc=mast.percs, al=rep('M', length(mast.percs)))
	d.b <- data.frame(map.percile=perciles, perc=basic.percs, al=rep('B', length(basic.percs)))
	rbind(d.m, d.b)
}

make_thresh_plot <- function(d, title) {
	ggplot(threshes, aes(x=map.percile, y=perc, color=al))+
		geom_point()+
	  scale_y_continuous(breaks=seq(0,1,.05), labels=scales::percent)+
	  scale_x_continuous(breaks=seq(0,100,10))+
		scale_color_discrete(name="Achievement\nLevel",
		  breaks=c("B", "M"),
		  labels=c("Basic", "Mastery")
		)+
	  labs(title=title,
	        x="National Percentile on EOY 2nd Grade MAP",
	        y="Percent of Students at or Above the Percentile in 2nd Grade\nthat Scored at or Above the Achievement Leve on 3rd Grade PARC"
	  )+
		theme_bw()
}

threshes <- d %>% subset(subject == 'ela') %>% do(calc_basic_and_mastery_threshes(.))
p <- make_thresh_plot(threshes, '2013-14 ELA 2nd Grade MAP to 2014-15 3rd Grade PARC Scores')
save_plot_as_pdf(p, 'Thresholds for 13-14 2nd MAP to 14-15 3rd PARC, ELA')

threshes <- d %>% subset(subject == 'math') %>% do(calc_basic_and_mastery_threshes(.))
p <- make_thresh_plot(threshes, '2013-14 Math 2nd Grade MAP to 2014-15 3rd Grade PARC Scores')
save_plot_as_pdf(p, 'Thresholds for 13-14 2nd MAP to 14-15 3rd PARC, Math')
