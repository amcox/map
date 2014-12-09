# TODO: Need to update

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Benchmark 1 and MAP Fall
df.map <- read.csv(file="map data fall.csv", head=TRUE, na.string=c("", " "))
df.bench <- read.csv(file="bench 1 data.csv", head=TRUE, na.string=c("", " "))
d.map <- df.map[, c("id", "subject", "fall.percentile")]
df <- merge(df.bench, d.map)

ggplot(df, aes(x=percent.correct, y=fall.percentile))+
	geom_point(alpha=.25)+
	geom_smooth()+
	scale_x_continuous(labels=percent, breaks=seq(0, 1, .1))+
	labs(title="Fall MAP Scores by Benchmark 1 Percent Correct, 2013-14",
			x="Percent Correct on Benchmark 1",
			y="National Percentile Rank on Fall MAP Test"
	)+
	theme_bw()+
	facet_grid(subject~grade)
	
ggplot(subset(df, subject == "reading"), aes(x=percent.correct, y=fall.percentile))+
	geom_point(alpha=.25)+
	geom_smooth()+
	scale_x_continuous(labels=percent, breaks=seq(0, 1, .1))+
	labs(title="Fall Reading MAP Scores by Benchmark 1 Percent Correct, 2013-14",
			x="Percent Correct on Benchmark 1",
			y="National Percentile Rank on Fall MAP Test"
	)+
	theme_bw()+
	theme(axis.text.x=element_text(size=6)
	)+
	facet_grid(school~grade)

