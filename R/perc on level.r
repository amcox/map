library(ggplot2)
library(scales)
library(gridExtra)
library(plyr)
library(reshape2)

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
df$grade <- df$map.grade

df$fall.on.level <- cut_percentile_to_on_level(df$fall.percentile)
df$winter.on.level <- cut_percentile_to_on_level(df$winter.percentile)
df$spring.on.level <- cut_percentile_to_on_level(df$spring.percentile)

# Plot of percent of students on level by session
d.on.level <- ddply(df, .(school, grade, subject), summarize,
  fall = (length(fall.on.level[fall.on.level != 'below' & !is.na(fall.on.level)]) / length(fall.on.level)),
  winter = (length(winter.on.level[winter.on.level != 'below' & !is.na(winter.on.level)]) / length(winter.on.level)),
  spring = (length(spring.on.level[spring.on.level != 'below' & !is.na(spring.on.level)]) / length(spring.on.level))
)
d.on.level.all <- ddply(df, .(school, subject), summarize,
  fall = (length(fall.on.level[fall.on.level != 'below' & !is.na(fall.on.level)]) / length(fall.on.level)),
  winter = (length(winter.on.level[winter.on.level != 'below' & !is.na(winter.on.level)]) / length(winter.on.level)),
  spring = (length(spring.on.level[spring.on.level != 'below' & !is.na(spring.on.level)]) / length(spring.on.level))
)
d.on.level.all$grade <- rep("all", nrow(d.on.level.all))
d.on.level <- rbind(d.on.level, d.on.level.all)
d.on.level.m <- melt(d.on.level, id.vars=c("school", "grade", "subject"),
  variable.name="season", value.name="percent.on.level"
)
p <- ggplot(d.on.level.m, aes(x=season, y=percent.on.level))+
  geom_line(aes(color=school, group=school))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.1), labels=percent)+
  labs(title="MAP Percent of Students On-Level by Session 2013-14",
    x="Testing Session",
    y="Percent of Students on or Above Level (>= 40th Percentile)"
  )+
  theme_bw()+
  facet_grid(subject ~ grade)
save_plot_as_pdf(p, "MAP Percents of Students On-Level by Session")

# Plot of growth gap by on level
boxplot_growth_gaps_by_prof_category <- function(d, subj){
  p <- ggplot(data=subset(df, subject == subj & !is.na(fall.on.level)), aes(x=fall.on.level, y=fall.spring.rit.growth.dif))+
  	geom_boxplot()+
    stat_summary(fun.y=mean, geom="point", shape=5)+
    labs(title=paste0("MAP ", simpleCap(subj), " Growth Against Goals, Fall to Spring 2014 by Proficiency Category"),
      y="Difference Between RIT Growth Goal and Actual (Positive is Good)",
      x="Fall RIT Proficiency Category (cut at 40th, 60th percentiles)"
    )+
    theme_bw()+
    facet_grid(grade ~ school, margins=T)
  save_plot_as_pdf(p, paste0("Map ", simpleCap(subj), " Growth Against Goals, Fall to Spring by Proficiency Category"), wide=F)
}
boxplot_growth_gaps_by_prof_category(d, "reading")
boxplot_growth_gaps_by_prof_category(d, "math")

# TODO: Calculation of a win/loss
# df$fall.winter.ggs <- apply(df, 1, cut_growth_status_on_se, 'fall.winter.rit.growth', 'goal.fall.winter', 'winter.se')
# df$fall.winter.percentile.growth <- apply(df, 1, fall_winter_percentile_growth)
# determine_win_loss_for_student <- function(r){
#   
# }