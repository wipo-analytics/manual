library(tidyverse)
library(glue)
library(usethis)


# used to download Scribe files (as Markdown) save as text files. import as text fiule
# extract the URLS, download the images and change the numbering.

scribe <- read_lines("scribe/scribe.txt")

# extract the URLS
# https://github.com/mvkorpel/pickURL
source("~/Documents/manual/R/pick_urls.R")

urls <- pick_urls(scribe) %>% 
  tibble(url = .) %>% 
  mutate(image = str_detect(url, ".jpeg")) %>% 
  filter(image == TRUE)

## Create destination and project folders if they dont exist

# note that if jpegs or pngs already exist in these files then 
# you should probably not proceed.

# something is returning NULL in this code
create_folders <- function(dest = NULL, project = NULL) {
  
  
  if(!is.null(dest)) {
    if (!dir.exists(dest)){
      dir.create(dest)
      ui_info("{ui_value(dest)} dir created")
    } else {
      ui_info("{ui_value(dest)} dir exists")
    }
  }

  if(!is.null(project)) {
    if (!dir.exists(paste0(dest, "/", project))){
      dir.create(paste0(dest, "/", project)) # need to paste0 here I think
      ui_info("{ui_value(project)} dir created")
    } else {
      ui_info("{ui_value(project)} dir exists")
    }
  }
  
  path <- paste0(dest, "/", "project")
  
  ftypes <- list.files(path, pattern = ".jpeg")
  
  im <- ".jpeg" %in% ftypes
  
  if(isTRUE(im)) {
    
    usethis::ui_warn("jpegs present in project folder and will be overwritten")
  } else (
    
    usethis::ui_info("No jpegs present in project folder")
    
  )
  
}

create_folders(dest = "images", project = "test")

# download the images

download_image <- function(url = NULL, dest = NULL, project = NULL, fname = NULL) {
 
  
  
  download.file(urls$url, destfile = paste0(dest, "/", project, "/", basename(url)))
                
  }


map(urls, download_image, dest = "images", project = "test")

rename_file <- function(path = NULL, project = NULL, fname = NULL) {
  
  old_files <- list.files(path, pattern="*.jpeg", full.names = TRUE)
  new_files <- paste0(fname, 1:length(old_files), "_", project, ".jpeg")
  file.rename(old_files, paste0(path, "/", new_files))
  
}

rename_file(path = "images/test", project = "plotly", fname = "fig")