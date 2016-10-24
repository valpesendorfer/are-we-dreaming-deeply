
calcHAND <- function(dem,fdir,facc,thresh,filename = NULL){
  
  #----------------------------------------------------------
  ## Load packages
  
  libs <- c('raster')
  new_p <- setdiff(libs, rownames(installed.packages())) 
  if (length(new_p) != 0 ) install.packages(new_p) 
  
  dmp <- lapply(libs, require, character.only = TRUE)
  
  
  if(!any(unlist(dmp))) warning('Problem occured while loading packages!')
  rm(dmp)
  
  #----------------------------------------------------------
  ## Argument management
  
  # DEM
  
  if (missing(dem)){ stop('Please specify valid (path to) DEM!\n')
    
  } else if (class(dem) != 'RasterLayer') {
    
    if(class(dem) == 'character'){
      
      if(file.exists(dem)){dem <- raster(dem)} else {stop('\n Please specify valid path to DEM!\n')}
      
    } else {stop('\n Please specify either valid path to DEM or RasterLayer in memory!\n')}
    
  }
  
  # D8 Flowdirection
  
  if (missing(fdir)){ stop('Please specify valid (path to) FDIR!\n')
    
  } else if (class(fdir) != 'RasterLayer') {
    
    if(class(fdir) == 'character'){
      
      if(file.exists(fdir)){fdir <- raster(fdir)} else {stop('\n Please specify valid path to FDIR!\n')}
      
    } else {stop('\n Please specify either valid path to FDIR or RasterLayer in memory!\n')}
    
  }
  
  # D8 Flowaccumulation
  
  if (missing(facc)){ stop('Please specify valid (path to) FACC!\n')
    
  } else if (class(facc) != 'RasterLayer') {
    
    if(class(facc) == 'character'){
      
      if(file.exists(facc)){facc <- raster(facc)} else {stop('\n Please specify valid path to FACC!\n')}
      
    } else {stop('\n Please specify either valid path to FACC or RasterLayer in memory!\n')}
    
  }
  
  # Stream-threshold 
  
  if (missing(thresh)){ thresh <-  NULL}
  
  #----------------------------------------------------------
  ## Helper functions
  
  # 
  
  cleanPad <- function(x, r){
    x[(seq_len(nrow(r)) - 1) * ncol(r) + 1.0, c(1, 4, 7)] <- NA
    x[seq_len(nrow(r)) * ncol(r), c(3, 6, 9)]             <- NA
    return(x)
  }
  
  #
  
  multiReclass <- function(x, from, to){
    
    mtch   <- match(x, from)
    pos    <- !is.na(mtch)
    
    x[pos] <- to[mtch[pos]]
    return(x)
  }
  
  #
  
  getFromMat <- function(x, row, col){
    x <- x[nrow(x) * (col - 1) + row]
    return(x)
  }
  
  #----------------------------------------------------------
  ## Processing
  
  # NoData management
  
  dem[dem == -9999] <- NA
  dem[dem < 0 ] <- 0
  
  fdir[fdir == 0 | fdir > 128] <- NA
  
  
  # Calculate stream network from flow accumulation
  
  if (is.null(thresh)){
    
    bool <- TRUE
    
    while (bool){
      
      cat('\n \n')
      
      thresh <- as.numeric(readline(prompt = ' Specify accumulation threshold: \n'))
      
      dnet <- facc > thresh
      
      dev.new()
      
      plot(dnet)
      
      cat('\n')
      
      bool <- !grepl('yes',tolower(readline(prompt = ' Threshold OK? (yes/no): \n')))
      
      dev.off()
      
    }
    
  } else {
    
    
    dnet <- facc > thresh
    
  }
  
  streamIX <- dnet != 0
  smax <- sum(dnet[streamIX])
  
  dnet[streamIX] <- seq_len(smax)
  
  if (exists('bool')){rm(bool)}
  
  #----------------------------------------------------------
  
  # calculte watershed raster
  
  
  fdir_val <- getValues(fdir)
  fdir_val <- multiReclass(fdir_val, to = seq_len(9)[-5], from = c(32, 64, 128, 16, 1, 8, 4, 2))
  
  cat('\n Calculating watersheds ')
  
  while (TRUE){
    dnet_nei <- getValuesFocal(dnet, ngb=c(3,3))
    dnet_nei <- cleanPad(x = dnet_nei, r = dnet)
    cntr_sel <- dnet_nei[, 5] == 0
    if (any(cntr_sel)){
      mem_pos  <- which(cntr_sel)
      
      dnet_sub <- dnet_nei[mem_pos, , drop=FALSE]
      fdir_sub <- fdir_val[mem_pos]
      
      F_ij     <- getFromMat(x   = dnet_sub, 
                             row = seq_len(nrow(dnet_sub)), 
                             col = fdir_sub)
      
      updt     <- F_ij != 0 & !is.na(F_ij)
      if (!any(updt)) break
      dnet_nei[mem_pos[updt], 5] <- F_ij[updt]
      
      dnet[] <- dnet_nei[, 5]
      
      
    } else {
      break
    }
    cat('.')
  }
  cat(' done. \n')
  
  # check if watersheds are ok
  
  dev.new()
  
  plot(dnet)
  
  bool <- !grepl('yes',tolower(readline(prompt = ' Watersheds OK? (yes/no): \n')))
  
  if (bool) {stop('\n Specify new threshold.\n')}
  
  dev.off()
  
  #----------------------------------------------------------
  
  # Calculate final HAND raster
  
  cat('\nCalculating final HAND raster ...')
  
  hand <- multiReclass(dnet,from = 1:smax, to = dem[streamIX])
  
  hand <- dem - hand
  
  cat(' done. \n')
  
  
  if (!is.null(filename)){
    writeRaster(hand, filename=filename, datatype="FLT4S", overwrite=TRUE)
  }
  
  
  plot(hand)
  
  return(hand)
  
}
