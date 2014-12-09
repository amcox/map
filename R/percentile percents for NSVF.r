library(gdata)
library(plyr)
library(reshape2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

cut.quartile <- function(vec) {
	cut(vec, c(-1, 26, 51, 76, 101),
  	labels=c("first", "second", "third", "fourth"), right=FALSE
	)
}

df.map <- subset(load_map_all(), school %in% schools)

props.fall <- ddply(df.map, .(grade, school, subject), function(d){
  prop.table(table(cut.quartile(d$fall.percentile)))
})
props.fall$round <- rep("BOY", nrow(props.fall))

props.winter <- ddply(df.map, .(grade, school, subject), function(d){
  prop.table(table(cut.quartile(d$winter.percentile)))
})
props.winter$round <- rep("Mid-Year", nrow(props.winter))

props <- rbind(props.fall, props.winter)

write.csv(props, "./../Spreadsheets/MAP percentile percents for NSVF.csv", row.names=F)