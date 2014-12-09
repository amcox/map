library(ggplot2)
library(gdata)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(scales)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_wide_map_data()
df <- subset(df, map.grade %in% numeric.grades)

# Add growth columns, remove students missing fall or winter
df$fall.winter.rit.growth <- apply(df, 1, fall_winter_rit_growth)
df$fall.winter.rit.growth.dif <- apply(df, 1, fall_winter_rit_growth_dif)
df$fall.winter.percentile.growth <- apply(df, 1, fall_winter_percentile_growth)
df$fall.winter.rit.goal.percent <- apply(df, 1, function(r){
  as.numeric(r[['fall.winter.rit.growth']]) / as.numeric(r[['goal.fall.winter']])
})
df <- subset(df, !is.na(fall.winter.rit.growth))

# Add GEs for fall and winter rits
d.g <- load_ges()
old.col.order <- names(df)
df <- merge(df, d.g, by.x=c("subject", "fall.rit"), by.y=c("subject", "rit"))
df <- rename(df, c("ge"="fall.ge"))
df <- merge(df, d.g, by.x=c("subject", "winter.rit"), by.y=c("subject", "rit"))
df <- rename(df, c("ge"="winter.ge"))
df <- df[,c(old.col.order, "fall.ge", "winter.ge")]
df$fall.ge.gap <- apply(df, 1, function(r) {
  as.numeric(r[['fall.ge']]) - as.numeric(r[['map.grade']])
})
df$fall.winter.ggs <- apply(df, 1, cut_growth_status_on_se,
  'fall.winter.rit.growth', 'goal.fall.winter', 'winter.se'
)
df$tier <- cut(df$fall.ge.gap, c(-999, -.999, -.5, 999),
  labels=c("T3", "T2", "T1"),
  right=FALSE
)

# Make plot of GGS by tier and subject for all grades
p <- make_ggs_plot(df, c("subject", "tier"),
  "fall.winter.ggs", "MAP Gap Growth Status, Fall to Winter, by Tier"
)
save_plot_as_pdf(p,
  "MAP Gap Growth Status, Fall to Winter by Tier", wide=T
)


# Exploratory plots
ggplot(df, aes(x=fall.winter.rit.growth.dif, y=fall.winter.percentile.growth))+
  geom_point(alpha=.3)+
  geom_smooth()+
  scale_x_continuous(breaks=seq(-30, 30, 5))+
  scale_y_continuous(breaks=seq(-100, 100, 10))
  
ggplot(df, aes(x=fall.winter.rit.goal.percent, y=fall.winter.percentile.growth))+
  geom_point(alpha=.3)+
  geom_smooth()+
  scale_x_continuous(labels=percent, breaks=seq(-5,5,.5))+
  theme_bw()
  

