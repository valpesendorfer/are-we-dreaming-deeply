otsu2d <- function(image){
  
  ## performs image segmentation of grayscale images using 2-dimensional Otsu method
  
  #-------------------------------------------------------------------------------------
  ## Load packages
  
  pkgs <- c('raster','dplyr')
  lapply(pkgs,function(x) tryCatch(find.package(x), error=function(e) install.packages(x)))
  
  
  #-------------------------------------------------------------------------------------
  ## Helper functions
  
  cleanPad <- function(x, r){
    x[(seq_len(nrow(r)) - 1) * ncol(r) + 1.0, c(1, 4, 7)] <- NA
    x[seq_len(nrow(r)) * ncol(r), c(3, 6, 9)]             <- NA
    return(x)
  }
  
  
  #-------------------------------------------------------------------------------------
  ## Argument management
  
  if (missing(image)){ stop('Please specify valid path to input image!\n')
    
  } else if (!file.exists(dem)){stop('Please specify valid path to input image!\n')
    
  } else { image <- raster(image) }
  
  
  #-------------------------------------------------------------------------------------
  ## Processing
  
  
  
  
  
  
}