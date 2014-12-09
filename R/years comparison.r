library(ggplot2)
library(scales)
library(gridExtra)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(gdata)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Load data
d3 <- load_all_map_from_raw('/2013')
d4 <- load_all_map_from_raw()
d <- rbind(d3, d4)
d$year.term <- factor(paste(d$year, d$term, sep='.'))
d$year.term <- reorder(d$year.term, new.order=year.terms)
d <- drop.levels(subset(d, map.grade %in% numeric.grades &
  subject %in% c('reading', 'math') &
  id > 1000 &
  school %in% schools)
)
d$on.level <- cut_percentile_to_on_level(d$percentile)

# Make plots of percent on level over time, not by cohort
make_percent_on_level_plot <- function(d, subj) {
  p <- ggplot(d, aes(x=year.term, y=perc.at.or.above, color=school))+
    geom_line(aes(group=school))+
    facet_wrap(~map.grade, nrow=1)+
    scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, .1), labels=percent)+
    scale_color_manual(values=schools.pal)+
    labs(x='Test Date',
      y='Percent of Students at or Above the 40th Percentile',
      title=paste0('Percent of Students On or Above Level on MAP ', simpleCap(subj), ' 2012 to 2014, Not Cohort')
    )+
    theme_bw()+
    theme(axis.text.x=element_text(angle=90, vjust=0.5)
    )
  save_plot_as_pdf(p, paste0('MAP ', simpleCap(subj), ' Percent of Students on Level, Multiple Years, Not Cohort'))
}
d.p <- d %>% group_by(school, map.grade, year.term, subject) %>%
  summarize(perc.at.or.above=mean(on.level %in% c('on', 'above')))
d.p$year.term <- reorder(d.p$year.term, new.order=year.terms)
make_percent_on_level_plot(subset(d.p, subject == 'reading'), 'reading')
make_percent_on_level_plot(subset(d.p, subject == 'math'), 'math')

# Make plots of percent on level over time, by cohort
d.14 <- subset(d, year == '2013-2014')
cohort.keys <- d.14 %>% group_by(id) %>% summarize(map.grade.14=max(map.grade),
  school.14=school[1]
)
d.cohort <- merge(d, cohort.keys, by='id', all.x=F)
d.p <- d.cohort %>% group_by(school.14, map.grade.14, year.term, subject) %>%
  summarize(perc.at.or.above=mean(on.level %in% c('on', 'above')))
d.p$year.term <- reorder(d.p$year.term, new.order=year.terms)
# Remove any students now at SCH that were at other schools in previous years, since n is so small
d.p <- subset(d.p, !(year.term %in% year.terms.12.13 & school.14 == 'SCH'))
# Remove any cohort 0th grade previous years, since n is so small
d.p <- subset(d.p, !(year.term %in% year.terms.12.13 & map.grade.14 == 0))
make_percent_on_level_cohort_plot <- function(d, subj) {
  p <- ggplot(d, aes(x=year.term, y=perc.at.or.above, color=school.14))+
    geom_line(aes(group=school.14))+
    scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, .1), labels=percent)+
    scale_color_manual(values=schools.pal)+
    labs(x='Test Date',
      y='Percent of Students at or Above the 40th Percentile',
      title=paste0('Percent of Students On or Above Level on MAP ', simpleCap(subj), ' 2012 to 2014\nCohorts by 2014 Grade and School')
    )+
    theme_bw()+
    theme(axis.text.x=element_text(angle=90, vjust=0.5)
    )+
    facet_wrap(~map.grade.14, nrow=1)
  save_plot_as_pdf(p, paste0('MAP ', simpleCap(subj), ' Percent of Students on Level, Multiple Years, Cohort'))
}
make_percent_on_level_cohort_plot(subset(d.p, subject == 'reading'), 'reading')
make_percent_on_level_cohort_plot(subset(d.p, subject == 'math'), 'math')
