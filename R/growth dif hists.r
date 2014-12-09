# Saves files of histograms of growth difference from goals
# Currently set to fall.winter, need to manually change to another time period
# in several places.

library(ggplot2)
library(scales)
library(gridExtra)
library(stringr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

growth_dif_hist <- function(d, title){
  p <- ggplot(data=d, aes(x=fall.spring.rit.growth.dif))+
		geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=5)+
		geom_vline(xintercept=mean(d$fall.spring.rit.growth.dif, na.rm=T), color="blue")+
		geom_vline(xintercept=median(d$fall.spring.rit.growth.dif, na.rm=T), color="blue", linetype="longdash")+
    scale_y_continuous(labels=percent, limits=c(0,.5), breaks=seq(0,.5,.1))+
    scale_x_continuous(breaks=seq(-50,50,10), limits=c(-30, 30))+
		theme_bw()+
    labs(title=title)+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank()
		)
  return(p)
}

save_growth_dif_plots_grade_school <- function(d, subj){
  plots <- sapply(levels(interaction(numeric.grades, schools)), function(x){NULL})
  for(g in numeric.grades){
  	for(s in schools){
  		d.sub <- subset(d, grade==g & school==s & subject==subj)
  		plots[[paste(g,s,sep=".")]] <- growth_dif_hist(d.sub, paste(s, g, sep=" "))
  	}
  }
  p <- do.call(arrangeGrob, c(plots, main=paste0("\nMAP ", simpleCap(subj), " 2013-14 Fall to Spring Growth Difference from Goal"),
  												left="\nPercent of Students",
                          sub=paste0("RIT Points Above Fall to Spring Growth Goal\n",
                            "dashed line = median, solid line = mean\n"
                          ),
  												ncol=3)
  )
  save_plot_as_pdf(p, paste0("MAP ", simpleCap(subj), " Fall to Spring 2014 Growth Difference from Goal by Grade-School"), wide=F)
}

save_growth_dif_plots_school <- function(d){
  subjs <- c("reading", "math")
  plots <- sapply(levels(interaction(schools, subjs)), function(x){NULL})
  for(subj in subjs){
  	for(s in schools){
  		d.sub <- subset(d, school==s & subject==subj)
  		plots[[paste(s,subj,sep=".")]] <- growth_dif_hist(d.sub, paste(s, simpleCap(subj), sep=" "))
  	}
  }
  p <- do.call(arrangeGrob, c(plots, main="\nMAP Fall to Spring 2014 Growth Difference from Goal",
  												left="\nPercent of Students",
                          sub=paste0("RIT Points Above Fall to Spring 2014 Growth Goal\n",
                            "dashed line = median, solid line = mean\n"
                          ),
  												ncol=4)
  )
  save_plot_as_pdf(p, "MAP Fall to Spring 2014 Growth Difference from Goal by School")
}

df <- load_wide_map_data()
df <- subset(df, map.grade %in% numeric.grades)
df <- subset(df, subject %in% c("reading", "math"))

df <- add_growth_columns_to_spring(df)
df$grade <- df$map.grade

save_growth_dif_plots_grade_school(df, "reading")
save_growth_dif_plots_grade_school(df, "math")
save_growth_dif_plots_school(df)
