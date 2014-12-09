# Saves files of histograms of national percentile rank
# Currently set to winter.percentile, need to manually change to another time
# period in several places.

library(ggplot2)
library(scales)
library(gridExtra)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

percentile_hist <- function(df, title){
	p <- ggplot(data=df, aes(x=spring.percentile))+
		geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=10)+
		geom_vline(xintercept=mean(df$spring.percentile, na.rm=T), color="blue")+
		geom_vline(xintercept=median(df$spring.percentile, na.rm=T), color="blue", linetype="longdash")+
		scale_y_continuous(labels=percent, limits=c(0,0.5), breaks=seq(0,0.5,.1))+
		scale_x_continuous(breaks=seq(0,100,10), limits=c(0, 100))+
		theme_bw()+
		labs(title=title)+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank(),
          axis.text.x=element_text(angle=90, vjust=0.5)
					)
	return(p)
}

save_percentile_hist_plots_grade_school <- function(d, subj){
  plots <- sapply(levels(interaction(numeric.grades, schools)), function(x){NULL})
  for(g in numeric.grades){
  	for(s in schools){
  		d.sub <- subset(d, grade==g & school==s & subject==subj)
  		plots[[paste(g,s,sep=".")]] <- percentile_hist(d.sub, paste(s, g, sep=" "))
  	}
  }
  p <- do.call(arrangeGrob, c(plots, main=paste0("\nMAP ", simpleCap(subj), " Spring 2014 National Percentile"),
  												left="\nPercent of Students",
                          sub=paste0("National Percentile Rank\n",
                            "dashed line = median, solid line = mean\n"
                          ),
  												ncol=3)
  )
  save_plot_as_pdf(p, paste0("MAP ", simpleCap(subj), " Spring Percentile by Grade-School"), wide=F)
}

save_percentile_hist_plots_school <- function(d){
  subjs <- c("reading", "math")
  plots <- sapply(levels(interaction(schools, subjs)), function(x){NULL})
  for(subj in subjs){
  	for(s in schools){
  		d.sub <- subset(d, school==s & subject==subj)
  		plots[[paste(s,subj,sep=".")]] <- percentile_hist(d.sub, paste(s, simpleCap(subj), sep=" "))
  	}
  }
  p <- do.call(arrangeGrob, c(plots, main="\nMAP Spring 2014 National Percentile",
  												left="\nPercent of Students",
                          sub=paste0("National Percentile Rank\n",
                            "dashed line = median, solid line = mean\n"
                          ),
  												ncol=4)
  )
  save_plot_as_pdf(p, "MAP Spring Percentile by School")
}

df <- load_wide_map_data()
df <- subset(df, map.grade %in% numeric.grades)
df <- subset(df, subject %in% c("reading", "math"))

df <- add_growth_columns_to_spring(df)
df$grade <- df$map.grade

save_percentile_hist_plots_grade_school(df, "reading")
save_percentile_hist_plots_grade_school(df, "math")
save_percentile_hist_plots_school(df)
