make_wide_map_data <- function(df) {
  # Take a long MAP data file, as produced by load_all_map_from_raw and turn
  # it into wide data with one row per student-subject-year

  # Pull out the goal columns and reduce to just one set of goal values per
  # student-subject-year
  library(stringr)
	library(gdata)
  
  goal.cols <- c("goal.fall.fall", "goal.spring.spring", "goal.fall.spring",
    "goal.fall.winter"
  )
  df.goals <- df[, c("id", "subject", "year", goal.cols)]
  dfgm <- melt(df.goals, id.vars=c("id", "subject", "year"), value.name="value",
    variable.name="goal"
  )
  dfgm <- subset(dfgm, !is.na(value))
  df.goals <- dcast(dfgm, ... ~ goal, mean, na.rm=T)

  # Melt the three score types down
  df.m <- melt(df[, !names(df) %in% goal.cols],
    measure.vars=c("rit", "se", "percentile"),
    variable.name="score.type", value.name="score.value"
  )
  
  # Find the final school and grade for each year-student
  d.sg <- subset(df.m, !is.na(score.value))[, c('term', 'year', 'school', 'id', 'map.grade')]
  d.sg <- unique(d.sg)
  d.sg$term <- factor(d.sg$term)
  d.sg$term <- reorder.factor(d.sg$term, new.order=c('spring', 'winter', 'fall'))
  d.sg <- d.sg[order(d.sg$term), ]
  sg.ref <- d.sg %>% group_by(year, id) %>% summarize(school=school[1], map.grade=map.grade[1])
  
  # Remove the year and grade from the melted df
  df.m <- df.m[, !names(df.m) %in% c('map.grade', 'school')]

  # Create a new name (which will be column names) for score and term
  df.m$score.type <- paste(df.m$term, df.m$score.type, sep=".")
  df.m <- df.m[, !names(df.m) %in% c("term")]

  df.sum <- dcast(df.m, ... ~ score.type)
  
  # Add the goals back in
  d <- merge(df.sum, df.goals, all.x=T)
  # Add the grade and school back in
  d <- merge(d, sg.ref)
  
  # Re-order columns nicely
  first.cols <- c('year', 'district', 'id', 'last.name', 'first.name', 'school', 'map.grade')
  d <- d[, c(first.cols, names(d)[!names(d) %in% first.cols])]
  
  return(d)
}
