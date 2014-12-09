fall_winter_rit_growth <- function(r){
  return(as.numeric(r[["winter.rit"]])-as.numeric(r[["fall.rit"]]))
}

winter_spring_rit_growth <- function(r){
  return(as.numeric(r["spring.rit"])-as.numeric(r["winter.rit"]))
}

fall_spring_rit_growth <- function(r){
  as.numeric(r["spring.rit"]) - as.numeric(r["fall.rit"])
}

fall_winter_rit_growth_dif <- function(r){
  as.numeric(r[['fall.winter.rit.growth']]) - as.numeric(r[['goal.fall.winter']])
}

fall_winter_rit_growth_percent <- function(r){
  as.numeric(r[['fall.winter.rit.growth']]) / as.numeric(r[['goal.fall.winter']])
}

fall_spring_rit_growth_dif <- function(r){
  as.numeric(r[['fall.spring.rit.growth']]) - as.numeric(r[['goal.fall.spring']])
}

fall_spring_rit_growth_percent <- function(r){
  as.numeric(r[['fall.spring.rit.growth']]) / as.numeric(r[['goal.fall.spring']])
}

fall_winter_percentile_growth <- function(r){
  as.numeric(r[['winter.percentile']]) - as.numeric(r[['fall.percentile']])
}

fall_spring_percentile_growth <- function(r){
  as.numeric(r[['spring.percentile']]) - as.numeric(r[['fall.percentile']])
}

cut_growth_status_on_se <- function(r, growth.col.to.cut, goal.col, se.col){
  se <- as.numeric(r[[se.col]])
  cut.val <- as.numeric(r[[growth.col.to.cut]])
  goal.val <- as.numeric(r[[goal.col]])
  if(is.na(se) | is.na(cut.val) | is.na(goal.val)){ return(NA) }
  if(cut.val < (goal.val - se)){
    return("opened")
  }else if(cut.val <= (goal.val + se)){
    return("none")
  }else{
    return("closed")
  }
}

add_growth_columns_to_spring <- function(df) {
  df$fall.winter.rit.growth <- apply(df, 1, fall_winter_rit_growth)
  df$fall.winter.rit.growth.dif <- apply(df, 1, fall_winter_rit_growth_dif)
  df$fall.winter.rit.growth.percent <- apply(df, 1, fall_winter_rit_growth_percent)

  df$fall.spring.rit.growth <- apply(df, 1, fall_spring_rit_growth)
  df$fall.spring.rit.growth.dif <- apply(df, 1, fall_spring_rit_growth_dif)
  df$fall.spring.rit.growth.percent <- apply(df, 1, fall_spring_rit_growth_percent)

  df$winter.spring.rit.growth <- apply(df, 1, winter_spring_rit_growth)

  df$fall.winter.ggs <- apply(df, 1, 'cut_growth_status_on_se', 'fall.winter.rit.growth', 'goal.fall.winter', 'winter.se')
  df$fall.spring.ggs <- apply(df, 1, 'cut_growth_status_on_se', 'fall.spring.rit.growth', 'goal.fall.spring', 'spring.se')
  return(df)
}