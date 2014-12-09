# Does being with us matter?
no.na.length <- function(vec){
	vec <- vec[!is.na(vec)]
	return(length(vec))
}
map.comparison <- function(df){
	y <- rep(NA,17)
	vet <- subset(df, y11==1)
	new <- df[is.na(df$y11),]
	y[1] <- nrow(vet)
	y[2] <- nrow(new)
	t.fall.percentile <- tryCatch({
		t.test(vet$fall.percentile, new$fall.percentile)
		}, error=function(err){
			return(NA)
		}
	)
	if(!is.na(t.fall.percentile)){
		y[3] <- t.fall.percentile$estimate[1]
		y[4] <- t.fall.percentile$estimate[2]
		y[5] <- t.fall.percentile$conf.int[1]
		y[6] <- t.fall.percentile$conf.int[2]
		y[7] <- t.fall.percentile$p.value	
	}
	t.win.percentile <- tryCatch({
		t.test(vet$win.percentile, new$win.percentile)
		}, error=function(err){
			return(NA)
		}
	)
	if(!is.na(t.win.percentile)){
		y[8] <- t.win.percentile$estimate[1]
		y[9] <- t.win.percentile$estimate[2]
		y[10] <- t.win.percentile$conf.int[1]
		y[11] <- t.win.percentile$conf.int[2]
		y[12] <- t.win.percentile$p.value	
	}
	t.growth.dif <- tryCatch({
		t.test(vet$rit.growth.dif, new$rit.growth.dif)
		}, error=function(err){
			return(NA)
		}
	)
	if(!is.na(t.growth.dif)){
		y[13] <- t.growth.dif$estimate[1]
		y[14] <- t.growth.dif$estimate[2]
		y[15] <- t.growth.dif$conf.int[1]
		y[16] <- t.growth.dif$conf.int[2]
		y[17] <- t.growth.dif$p.value	
	}
	names(y) <- c("n.vet", "n.new",
								"fall.percentile.vet.mean", "fall.percentile.new.mean",
								"fall.percentile.dif.low", "fall.percentile.dif.high",
								"fall.percentile.dif.p",
								"win.percentile.vet.mean", "win.percentile.new.mean",
								"win.percentile.dif.low", "win.percentile.dif.high",
								"win.percentile.dif.p",
								"growth.dif.vet.mean", "growth.dif.new.mean",
								"growth.dif.dif.low", "growth.dif.dif.high",
								"growth.dif.dif.p"
								)
	return(y)
}

df.s <- subset(df, grade %in% c(0:2, "K"))
df.sum <- ddply(df.s, .(school, grade), map.comparison)
write.csv(df.sum, "map_summary.csv")
df.sum <- ddply(df.s, .(grade), map.comparison)
write.csv(df.sum, "map_summary_grade.csv")
df.sum <- ddply(df.s, .(school), map.comparison)
write.csv(df.sum, "map_summary_school.csv")