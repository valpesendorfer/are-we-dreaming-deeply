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
  
  gvals <- getValues(image)
  
  gvals_nei <- getValuesFocal(image,ngb = c(3,3))
  
  gvals_nei <- cleanPad(gvals_nei,image)
  
  gvals_nei <- rowMeans(gvals_nei[,-5],na.rm = T)
  
  
  df <- data.frame(gvals,gvals_nei) %>%
        group_by(gvals,gvals_nei) %>%
        tally() %>%
        mutate(prob = n/ncell(test))
  
  
  
  #-------------------------------------------------------------------------------------
  ### UNDER CONSTRUCTION ###
  #-------------------------------------------------------------------------------------
  
  #TEMPCODE
  
  #df2[sample(1:nrow(df2),1),] # test random thresholds
  
  #mutate(df2,group = ifelse(ivals < 153 & t < 169.5,"C0","C1")) %>% group_by(group) %>% summarise_each(funs(mean,sum))
  
  
  
}