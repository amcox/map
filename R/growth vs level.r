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

rit_vs_growth_plot <- function(d, subj){
  p <- ggplot(data=d, aes(x=fall.rit, y=fall.spring.rit.growth.dif))+
	geom_point(alpha=.3)+
  geom_smooth()+
  theme_bw()+
  labs(title=paste0("RIT ", subj, " Starting Level 2013-14 vs Growth"),
    x="Fall RIT Score",
    y="Gap from RIT Growth Goal to Actual Growth (Positive is Good)"
  )+
	facet_grid(grade~school, margins=T)
  return(p)
}

save_plot_as_pdf(rit_vs_growth_plot(subset(df, subject=="reading"), "Reading"),
  "MAP Reading Growth vs Starting RIT"
)
save_plot_as_pdf(rit_vs_growth_plot(subset(df, subject=="math"), "Math"),
  "MAP Math Growth vs Starting RIT"
)

percentile_vs_growth_plot <- function(d, subj){
  p <- ggplot(data=d, aes(x=fall.percentile, y=fall.spring.rit.growth.dif))+
	geom_point(alpha=.3)+
  geom_smooth()+
  theme_bw()+
  labs(title=paste0("RIT ", subj, " Starting Level 2013-14 vs Growth"),
    x="Fall National Percentile Score",
    y="Gap from RIT Growth Goal to Actual Growth (Positive is Good)"
  )+
	facet_grid(grade~school, margins=T)
  return(p)
}

save_plot_as_pdf(percentile_vs_growth_plot(subset(df, subject=="reading"), "Reading"),
  "MAP Reading Growth vs Starting Percentile"
)
save_plot_as_pdf(percentile_vs_growth_plot(subset(df, subject=="math"), "Math"),
  "MAP Math Growth vs Starting Percentile"
)