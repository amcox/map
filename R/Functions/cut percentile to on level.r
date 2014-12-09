cut_percentile_to_on_level <- function(vec){
  	cut(vec, c(-999, 40, 60.1, 999),
  			labels=c("below", "on", "above"),
        right=FALSE
		)
}