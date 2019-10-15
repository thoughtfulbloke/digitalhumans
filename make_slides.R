
library(export)


img_location <- "~/Desktop/webimg" # or replace with path to png folder
slidedeck <- tempfile(tmpdir="~/Desktop") # or replace with path to a specific  .pptx

if (!dir.exists(img_location)){
  dir.create(img_location)
}

# use a function for adding slides to a presentation & saving a standalone png
# to only make images, comment out the graph2ppt line

make_slides <- function(x, img_folder, pptx_path){
  source(x) #creates a ggplot saved as R object "slide"
  graph2ppt(x=slide, file=pptx_path, width=10.03937, height=7.5, append=TRUE)
  png_path <- paste0(img_folder, "/",gsub("\\..*",".png",x))
  ggsave(filename=png_path, plot=slide, width=10.03937,
         height=7.5, units="in", dpi="screen")
}


# doing slides in batches that use similar data so I can clear memory between batches
source("theme_otago.R")
batch1 <- c("slide1.R", "slide2.R","slide3.R","slide4.R")
invisible(lapply(batch1, make_slides, img_folder= img_location, pptx_path=slidedeck))
#clear all memory except the configuration and the function
rm(list=setdiff(ls(), c("img_location","slidedeck", "make_slides")))
source("theme_otago.R")
batch2 <- c("slide5.R", "slide6.R","slide7.R","slide8.R")
invisible(lapply(batch2, make_slides, img_folder= img_location, pptx_path=slidedeck))
rm(list=setdiff(ls(), c("img_location","slidedeck", "make_slides")))
source("theme_otago.R")
batch3 <- c("slide9.R", "slide10.R","slide11.R")
invisible(lapply(batch3, make_slides, img_folder= img_location, pptx_path=slidedeck))

