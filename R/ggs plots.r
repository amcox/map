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

# Make GGS plots using the regular ggs
df <- load_wide_map_data()
df <- subset(df, map.grade %in% numeric.grades)
df <- subset(df, subject %in% c("reading", "math"))

df <- add_growth_columns_to_spring(df)
df$grade <- df$map.grade

p <- make_ggs_plot(subset(df, subject=="reading"), c("school", "grade"),
  "fall.spring.ggs", "MAP Reading Gap Growth Status, Fall to Spring 2014"
)
save_plot_as_pdf(p, "MAP Reading Gap Growth Status, Fall to Spring by School-Grade", wide=F)

p <- make_ggs_plot(subset(df, subject=="math"), c("school", "grade"),
  "fall.spring.ggs", "MAP Math Gap Growth Status, Fall to Spring 2014"
)
save_plot_as_pdf(p, "MAP Math Gap Growth Status, Fall to Spring by School-Grade", wide=F)