#' BGM to map script
#' original code from Alexander Keth
#' 

bgm_to_map <- function(bgm_file, bgm_string){
  bgm <- readLines(con = bgm_file)
  proj_in <- stringr::str_split(string = bgm[grep(pattern = "projection", x = bgm)], pattern = " ", n = 2)[[1]][2]
  n_boxes <- str_split_twice(char = bgm[grep(pattern = "nbox", x = bgm)], min_only = T)
  box_strings <- paste0("box", 0:(n_boxes - 1), bgm_string)
  
  for (i in seq_along(box_strings)) {
    if (i == 1) bgm_data <- list()
    bgm_data[[i]] <- bgm[grep(pattern = box_strings[i], x = bgm)]
    # remove entries with "vertmix" (also found with box_strings)
    if (bgm_string == ".vert") bgm_data[[i]] <- bgm_data[[i]][-grep(pattern = "vertmix", x = bgm_data[[i]])]
    bgm_data[[i]] <- lapply(bgm_data[[i]], str_split_twice, min_only = F)
    bgm_data[[i]] <- data.frame(x = as.numeric(sapply(bgm_data[[i]], function(x) x[2])),
                                y = as.numeric(sapply(bgm_data[[i]], function(x) x[3])),
                                polygon = i - 1)
  }
  
  bgm_data <- do.call(rbind, bgm_data)
  
  lat_long <- proj4::project(bgm_data[, 1:2], proj = proj_in, inverse = T)
  
  bgm_data$long <- lat_long$x
  bgm_data$lat <- lat_long$y
  
  return(bgm_data)
}
