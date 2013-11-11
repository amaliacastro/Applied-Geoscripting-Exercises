
# Wageningen University
# Amalia Castro Gómez
# Applied Geoscripting course, Lesson 2: Exercise
# Create a function to detect leap years

is.leap <- function(year) {
  # Detects if a certain year is a leap year.
  #
  # Args:
  #   year: the year to be tested, only valid if entered as numeric.
  #
  # Returns:
  #   The result of testing the year
  if (is.numeric(year)) {  # only numbers
    if (year > 1582) {  # only in the valid range
      if (year %% 400 == 0) {
        result <- TRUE
      } else if (year %% 100 == 0) {
        result <- FALSE
      } else if (year %% 4 == 0) {
        result <- TRUE
      } else {  # if year is in the range but not divisible by 400, 100 or 4
        result <- FALSE
      }
    } else {
      result <- paste ( year," not in the valid range")  # should this be a warning?
    }
  } else { 
    stop("argument of class numeric expected")
  }
  return (result)
}


  




