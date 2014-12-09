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
d.g <- load_ges()
df <- merge(df, d.g, by.x=c("subject", "winter.rit"), by.y=c("subject", "rit"))
df <- rename(df, c("ge"="winter.ge"))
df$winter.ge.gap <- apply(df, 1, function(r) {
  as.numeric(r[['winter.ge']]) - (as.numeric(r[['map.grade']]) + 0.5)
})

d.means <- df %.%
  group_by(map.grade) %.%
  summarize(ge.gap.mean=mean(winter.ge.gap, na.rm=T),
    ge.gap.median=median(winter.ge.gap, na.rm=T)
  )

df.p <- df %.%
  group_by(map.grade, subject) %.%
  mutate(percentile=ecdf(winter.ge.gap)(winter.ge.gap))
  
df.top <- subset(df.p, percentile > .45)

d.means.top <- df.top %.%
  group_by(map.grade) %.%
  summarize(ge.gap.mean=mean(winter.ge.gap, na.rm=T),
    ge.gap.median=median(winter.ge.gap, na.rm=T)
  )

d.means$group <- rep("all", nrow(d.means))
d.means.top$group <- rep("top 55%", nrow(d.means.top))
d.means.all <- rbind(d.means, d.means.top)

save_df_as_csv(d.means.all, 'map data for brandon graph')