library(stringr)
library(plyr)
library(dplyr)
update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

library(digest)

# Old system
# df <- read.csv(file="MAP for NSNO.csv", head=TRUE, na.string=c("", " ", "  "))
# 
# df$hash.id <- apply(df, 1, function(r){
#   digest(as.numeric(r[['StudentID']]), algo="sha256")
# })
# df <- df[,c(1:3,5:69)]
# write.csv(df, "ReNEW MAP data for NSNO.csv", na="", row.names=F)

df <- load_all_map_from_raw()
df$hash.id <- apply(df, 1, function(r){
  digest(as.numeric(r[['id']]), algo="sha256")
})
d <- df[, !names(df) %in% c("first.name", "last.name", "id")]
save_df_as_csv(d, "ReNEW MAP data for NSNO, 2015")

df <- load_all_map_from_raw(dir='/2013')
df$hash.id <- apply(df, 1, function(r){
  digest(as.numeric(r[['id']]), algo="sha256")
})
d <- df[, !names(df) %in% c("first.name", "last.name", "id")]
save_df_as_csv(d, "ReNEW MAP data for NSNO, 2013")

df <- load_all_map_from_raw('/2014')
df$hash.id <- apply(df, 1, function(r){
  digest(as.numeric(r[['id']]), algo="sha256")
})
d <- df[, !names(df) %in% c("first.name", "last.name", "id")]
save_df_as_csv(d, "ReNEW MAP data for NSNO, 2014")