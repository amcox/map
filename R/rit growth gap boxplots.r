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

# Run the plot as three big plots with school.grade as the x
p1 <- ggplot(data=df)+
	geom_boxplot(aes(x=interaction(school, grade), y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(aes(x=interaction(school, grade), y=mean(fall.spring.rit.growth.dif, na.rm=TRUE)), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
	labs(title="All")+
	theme_bw()+
	theme(axis.title.x=element_blank(),
				axis.title.y=element_blank(),
        axis.text.x=element_text(size=6)
				)
df.r <- subset(df, df$subject == "reading")
p2 <- ggplot(data=df.r)+
	geom_boxplot(aes(x=interaction(school, grade), y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(aes(x=interaction(school, grade), y=mean(fall.spring.rit.growth.dif, na.rm=TRUE)), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
		labs(title="Reading")+
	theme_bw()+
	theme(axis.title.x=element_blank(),
				axis.title.y=element_blank(),
        axis.text.x=element_text(size=6)
				)
df.m <- subset(df, df$subject == "math")
p3 <- ggplot(data=df.m)+
	geom_boxplot(aes(x=interaction(school, grade), y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(aes(x=interaction(school, grade), y=mean(fall.spring.rit.growth.dif, na.rm=TRUE)), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
		labs(title="Math")+
	theme_bw()+
	theme(axis.title.x=element_blank(),
				axis.title.y=element_blank(),
        axis.text.x=element_text(size=6)
				)
p <- arrangeGrob(p1, p2, p3, ncol=1, main="\nRIT Growth Against Goals, Fall to Spring 2014",
							left="\nDifference Between RIT Growth Goal and Actual",
							sub="School.Grade\n")
save_plot_as_pdf(p, "Map Growth Against Goals, Fall to Spring", wide=F)
              
# Run the plot as faceted with margins and just school as the x, subjects on top
p <- ggplot(data=df)+
	geom_boxplot(aes(x=school, y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(aes(x=school, y=mean(fall.spring.rit.growth.dif, na.rm=TRUE)), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
  labs(title="RIT Growth Against Goals, Fall to Spring 2014",
    y="Difference Between RIT Growth Goal and Actual (Positive is Good)",
    x="School"
  )+
	theme_bw()+
	theme(axis.text.x=element_text(size=6))+
  facet_grid(grade ~ subject, margins=T)
save_plot_as_pdf(p, "Map Growth Against Goals, Fall to Spring by Grade Subject", wide=F)
  
# Run the plot as faceted with margins and just school as the x,
# subjects on side
p <- ggplot(data=df)+
	geom_boxplot(aes(x=school, y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(aes(x=school, y=mean(fall.spring.rit.growth.dif, na.rm=TRUE)), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
  labs(title="RIT Growth Against Goals, Fall to Spring 2014",
    y="Difference Between RIT Growth Goal and Actual (Positive is Good)",
    x="School"
  )+
	theme_bw()+
	theme(axis.text.x=element_text(size=6))+
  facet_grid(subject ~ grade, margins=T)  
save_plot_as_pdf(p, "Map Growth Against Goals, Fall to Spring by Subject Grade")