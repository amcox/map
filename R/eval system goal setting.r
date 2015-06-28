library(reshape2)
library(gdata)
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

make_map_eval_setting_plot <- function(d, year.string) {
	ggplot(d, aes(x=percentile, y=perc.met.growth.goal))+
		geom_text(aes(label=round(perc.met.growth.goal*100)), size=1.75)+
		geom_vline(x=c(0.1, 0.55, 0.9), linetype=3)+
		scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1), label=percent)+
		scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1), label=percent)+
		labs(x="Percent of Teachers at or Below Performance Level",
			y="Percent of Students in Class\nMeeting RIT Fall-Spring Growth Goal",
			title=paste0("K-2 MAP Data, ", year.string)
		)+
		theme_bw()+
		theme(axis.title.x = element_text(size=8),
			axis.title.y = element_text(size=8),
			axis.text.x = element_text(size=7),
			axis.text.y = element_text(size=7),
			plot.title = element_text(size=9)
		)+
		facet_wrap(~subject)
}

# 13-14 MAP Data Eval Setting Plot - NOT UPDATED FOR THIS YEAR YET
d <- load_wide_map_data()
d <- subset(d, map.grade %in% numeric.grades & subject %in% c('reading', 'math'))

# Change the col names in the PS file to "id", "home.room"
ps.hrs <- load_ps_hr_data()
d <- merge(d, select(ps.hrs, id, home.room))
d <- d %>% mutate(fall.spring.rit.growth = spring.rit - fall.rit,
	met.fall.spring.rit.growth.goal = fall.spring.rit.growth >= goal.fall.spring
)

dg <- d %>% group_by(subject, home.room) %>%
	summarize(perc.met.growth.goal = mean(met.fall.spring.rit.growth.goal, na.rm=T),
		n = n()
	) %>%
	group_by(subject) %>%
	mutate(percentile = ecdf(perc.met.growth.goal)(perc.met.growth.goal)) %>%
	arrange(subject, perc.met.growth.goal)

p <- make_map_eval_setting_plot(dg, '2014-15')
save_plot_as_pdf_adjustable(p, 'K-2 MAP 14-15', w=7.5, h=3)

# 14-15 MAP Data Eval Setting Plot
d <- load_wide_map_data()
d <- subset(d, map.grade %in% numeric.grades & subject %in% c('reading', 'math'))

# Change the col names in the PS file to "id", "home.room"
ps.hrs <- load_ps_hr_data()
d <- merge(d, select(ps.hrs, id, home.room))
d <- d %>% mutate(fall.spring.rit.growth = spring.rit - fall.rit,
	met.fall.spring.rit.growth.goal = fall.spring.rit.growth >= goal.fall.spring
)

dg <- d %>% group_by(subject, home.room) %>%
	summarize(perc.met.growth.goal = mean(met.fall.spring.rit.growth.goal, na.rm=T),
		n = n()
	) %>%
	group_by(subject) %>%
	mutate(percentile = ecdf(perc.met.growth.goal)(perc.met.growth.goal)) %>%
	arrange(subject, perc.met.growth.goal)

p <- make_map_eval_setting_plot(dg, '2014-15')
save_plot_as_pdf_adjustable(p, 'K-2 MAP 14-15', w=7.5, h=3)