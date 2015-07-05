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

# Load data
d <- load_all_years_map_data()
d.s <- subset(d, map.grade %in% c(0, 1, 2) & school %in% schools)
d.s$nce <- make_nce_from_percentile(d.s$percentile)
d.s$term <- factor(d.s$term, levels=c('fall', 'winter', 'spring'))

# Make and save plot with average NCE for all grades and years
make_multi_year_nce_plot <- function(d, subject.string) {
	ggplot(d, aes(x=term, y=mean.nce, color=school))+
		geom_hline(aes(yintercept=50), color='blue', linetype=2)+
		geom_line(aes(group=school), alpha=.8, size=1.5)+
		scale_color_manual(values=schools.pal, name='School')+
		scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10))+
		scale_x_discrete(expand = c(0,0.1), breaks=c('fall', 'winter', 'spring'))+
		labs(title=paste0("MAP Average NCE, ", subject.string),
					x="Term",
					y="Average Normal Curve Equivalent"
		)+
		theme_bw()+
		theme(axis.text.x=element_text(angle=90, vjust=0.5, size=8),
					axis.text.y=element_text(size=8),
					axis.title.x=element_text(size=9),
					axis.title.y=element_text(size=9),
					title=element_text(size=10)
		)+
		facet_grid(map.grade ~ year)
}

d.m <- d.s %>% group_by(school, map.grade, year, term, subject) %>% summarize(mean.nce = mean(nce, na.rm=T))
d.m$term <- factor(d.m$term, levels=c('fall', 'winter', 'spring'))

# Subset for math and save graph
d.m.m <- subset(d.m, subject == 'math')
p <- make_multi_year_nce_plot(d.m.m, 'Math')
save_plot_as_pdf(p, 'MAP Average NCE for All Years, All Grades, Math', w=F)

# Subset for reading and save graph
d.m.r <- subset(d.m, subject == 'reading')
p <- make_multi_year_nce_plot(d.m.r, 'Reading')
save_plot_as_pdf(p, 'MAP Average NCE for All Years, All Grades, Reading', w=F)

# Make and save plot with percent on-level for all grades and years
make_multi_year_percent_on_level_plot <- function(d, subject.string) {
	ggplot(d, aes(x=term, y=perc.on.level, color=school))+
		geom_hline(aes(yintercept=50), color='blue', linetype=2)+
		geom_line(aes(group=school), alpha=.8, size=1.5)+
		scale_color_manual(values=schools.pal, name='School')+
		scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, .1), labels=percent)+
		scale_x_discrete(expand = c(0,0.1), breaks=c('fall', 'winter', 'spring'))+
		labs(title=paste0("MAP Percent of Students On-Level, ", subject.string),
					x="Term",
					y="Percent of Students at or Above the 40th National Percentile"
		)+
		theme_bw()+
		theme(axis.text.x=element_text(angle=90, vjust=0.5, size=8),
					axis.text.y=element_text(size=8),
					axis.title.x=element_text(size=9),
					axis.title.y=element_text(size=9),
					title=element_text(size=10)
		)+
		facet_grid(map.grade ~ year)
}

d.m <- d.s %>% group_by(school, map.grade, year, term, subject) %>% summarize(perc.on.level = mean(percentile >= 40, na.rm=T))
d.m$term <- factor(d.m$term, levels=c('fall', 'winter', 'spring'))

# Subset for math and save graph
d.m.m <- subset(d.m, subject == 'math')
p <- make_multi_year_percent_on_level_plot(d.m.m, 'Math')
save_plot_as_pdf(p, 'MAP Percent On-Level for All Years, All Grades, Math', w=F)

# Subset for reading and save graph
d.m.r <- subset(d.m, subject == 'reading')
p <- make_multi_year_percent_on_level_plot(d.m.r, 'Reading')
save_plot_as_pdf(p, 'MAP Percent On-Level for All Years, All Grades, Reading', w=F)


# Save summary table
d.sum <- d.s %>% group_by(school, map.grade, year, term, subject) %>%
	summarize(mean.nce=mean(nce, na.rm=T), perc.on.level = mean(percentile >= 40, na.rm=T))
d.sum.all <- d.s %>% group_by(map.grade, year, term, subject) %>%
	summarize(mean.nce=mean(nce, na.rm=T), perc.on.level = mean(percentile >= 40, na.rm=T))
d.sum.all$school <- rep('All', nrow(d.sum.all))
d.sum <- rbind(d.sum, d.sum.all)
save_df_as_csv(d.sum, 'MAP Summary Table for 15-16 Opening')