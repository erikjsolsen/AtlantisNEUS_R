#' String split function
#' by Alex Keth
#' 


str_split_twice <- function(char, min_only){
  patterns <- c(" ", "\t")
  if (all(!stringr::str_detect(string = char, pattern = patterns))) {
    stop("Neither space nor tab present in variable char (string)!")
  }
  for (i in seq_along(patterns)) {
    char <- unlist(stringr::str_split(string = char, pattern = patterns[i]))
  }
  char <- suppressWarnings(as.numeric(char))
  if (all(is.na(char))) stop("No numeric value in variable char (string)!")
  if (min_only) char <- char[min(which(!is.na(char)))]
  return(char)
}
