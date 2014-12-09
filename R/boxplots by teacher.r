# TODO: Need to switch this to pulling teacher informaton from PS OBDC with SQL

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Get teacher names on data
df.bench <- read.csv(file="map-bench.csv", head=TRUE, na.string=c(""))
df.teachers <- df.bench[,c(1,6)]
df <- merge(df, df.teachers)

# Boxplots by teacher of RIT dif
m <- ddply(df, .(home.room), summarise, mean=mean(fall.spring.rit.growth.dif, na.rm=TRUE))
ggplot(data=df)+
	geom_hline(yintercept=0, color="blue")+
	geom_boxplot(aes(x=home.room, y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(data=m, aes(x=home.room, y=mean), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
	labs(title="RIT Growth Against Goals, Fall-Spring, All Subjects",
				x="Teacher",
				y="Difference Between RIT Growth Actual and Goal")+
	theme_bw()+
	theme(
				)+
	coord_flip()

df.s <- subset(df, df$subject == "reading")
m <- ddply(df.s, .(home.room), summarise, mean=mean(fall.spring.rit.growth.dif, na.rm=TRUE))
p.e <- ggplot(data=df.s)+
	geom_hline(yintercept=0, color="blue")+
	geom_boxplot(aes(x=home.room, y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(data=m, aes(x=home.room, y=mean), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
	labs(title="Reading",
				x="Teacher",
				y="Difference Between RIT Growth Actual and Goal")+
	theme_bw()+
	theme(
				)+
	coord_flip()
df.s <- subset(df, df$subject == "math")
m <- ddply(df.s, .(home.room), summarise, mean=mean(fall.spring.rit.growth.dif, na.rm=TRUE))
p.m <- ggplot(data=df.s)+
	geom_hline(yintercept=0, color="blue")+
	geom_boxplot(aes(x=home.room, y=fall.spring.rit.growth.dif), notch=TRUE)+
	geom_point(data=m, aes(x=home.room, y=mean), shape=5)+
	scale_y_continuous(limits=c(-30,30), breaks=seq(-30,30,10))+
	labs(title="Math",
				x="Teacher",
				y="Difference Between RIT Growth Actual and Goal")+
	theme_bw()+
	theme(
				)+
	coord_flip()
grid.arrange(p.e+theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
							p.m+theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
							ncol=2,
							main="\nRIT Growth Against Goals, Fall-Spring",
							left="\nTeacher",
							sub="Difference Between RIT Growth Actual and Goal\n"
)
