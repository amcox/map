ggs_freqs <- function(data, dimensions, count.column){
	ggs_sums <- function(d, column){
		r <- list()
		r['closed'] <- length(d[[column]][d[[column]]=="closed"])
		r['none'] <- length(d[[column]][d[[column]]=="none"])
		r['opened'] <- length(d[[column]][d[[column]]=="opened"])
		return(data.frame(r))
	}
	return(ddply(data, dimensions, ggs_sums, column=count.column))
}

make_ggs_plot <- function(d, dimensions, count.column, title){
	df.freq <- ggs_freqs(d, dimensions, count.column)
	df.percs <- cbind(df.freq[,1:(length(dimensions))],prop.table(as.matrix(df.freq[,(length(dimensions)+1):ncol(df.freq)]),1))
	df.p.melted <- melt(df.percs, id.vars=dimensions,
												variable.name="category",
												value.name="perc"
	)
	df.f.melted <- melt(df.freq, id.vars=dimensions,
												variable.name="category",
												value.name="n"
	)
	df.total.n <- ddply(df.f.melted, dimensions, summarise, category="closed", n=paste0("n=",sum(n)))
	df.c <- merge(df.total.n, df.p.melted, all=T)
	ggplot(df.c)+
		geom_bar(aes(x=category, y=perc, fill=category), stat="identity")+
		geom_text(aes(x=category, y=1, label=n), size=2.5)+
		scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.1), labels=percent)+
		scale_fill_manual(values=c("#198D33", "#E5E167", "#D16262"))+
		labs(title=title,
      x="Gap Growth Status",
      y="Percent of Students"
		)+
		theme_bw()+
		theme(axis.text.x=element_text(size=6),
					axis.text.y=element_text(size=6)
		)+
		facet_grid(paste0(dimensions[1],"~",dimensions[2]))
}
