extract_map_fields <- function(df.raw) {
  # This takes a raw csv download from the NWEA website, extracts the fields
  # we want, and cleans them up. It returns a data.frame.
  
  # Make a new df to put the processed data
  d <- data.frame(term=rep(NA, nrow(df.raw)), stringsAsFactors=F)

  # Get the year and term from TermName
  term.and.year <- str_split(df.raw$TermName, " ")
  d$term <- sapply(term.and.year, function(x){ tolower(x[1]) })
  d$year <- sapply(term.and.year, function(x){ tolower(x[2]) })

  # Get the district
  d$district <- df.raw$DistrictName

  # Get the school
  d$school <- df.raw$SchoolName
  # Clean up school names
  d$school <- str_replace(d$school, "Accelerated High School City Park", "AHS")
  d$school <- str_replace(d$school, "ReNEW Cultural Arts Academy", "RCAA")
  d$school <- str_replace(d$school, "ReNEW Dolores T. Aaron Elementary", "DTA")
  d$school <- str_replace(d$school, "ReNEW Schaumburg Elementary", "SCH")
  d$school <- str_replace(d$school, "ReNEW Scitech Academy", "STA")

  # Get the student name
  d$last.name <- df.raw$StudentLastName
  d$first.name <- df.raw$StudentFirstName

  # Get the student ID
  d$id <- df.raw$StudentID

  # Get the grade from MAP
  d$map.grade <- as.numeric(str_replace(df.raw$Grade, "K", "0"))

  # Get the subject
  d$subject <- tolower(str_replace(df.raw$Discipline, "Mathematics", "math"))

  # Get the rit score
  d$rit <- df.raw$TestRITScore

  # Get the se
  d$se <- df.raw$TestStandardError

  # Get the percentile
  d$percentile <- df.raw$TestPercentile
			
  # Get growth targets
  d$goal.fall.fall <- df.raw$TypicalFallToFallGrowth
  d$goal.spring.spring <- df.raw$TypicalSpringToSpringGrowth
  d$goal.fall.spring <- df.raw$TypicalFallToSpringGrowth
  d$goal.fall.winter <- df.raw$TypicalFallToWinterGrowth
  
  return(d)
}