gdalMoS <- function(path=getwd(),pattern=NULL,recursive=TRUE,filename=NULL,
                    stck=FALSE,dtype=NULL,nodata=NULL,toTiff=TRUE,verbose=FALSE,dryrun=FALSE){
  
  # Wrapper function which takes multiple input raster and either mosaics or stacks them (if bands)
  # Requires OSGeo4W gdal - datatype strings according to gdal (e.g. UInt16)
  #
  # Valentin Pesendorfer, 2016
  
  
  #------------------------------------------------------------------------------------------------------------
  
  
  # Sysfun switch - thanks Sebastian!
  
  sysfun <- switch(.Platform$OS.type,
                   "unix" = system,
                   "windows" = shell)
  
  #------------------------------------------------------------------------------------------------------------
  ###GDAL###
  
  gdal <- 'C:/OSGeo4W64/OSGeo4W.bat' # Path needs to point to .bat file
  
  #------------------------------------------------------------------------------------------------------------
  
  # Arguments
  
  if(!file.exists(gdal)){stop('Please specify valid GDAL path! Function requires OSGeo4W distribution. \n')
    
  }else{gdal <- paste0(gdal,'; ')} #prepare for usage in function
  
  if(is.null(pattern)){stop('Please specify a pattern using REGEXP \n!')}
  
  if(stck){stck <- '-separate '}else{stck <- NULL}
  
  if(!is.null(nodata)){nodata <- paste0('-srcnodata ',nodata,' ')} 
  
  if(!is.null(dtype)){dtype <- paste0('-ot ',dtype, ' ')} 
  
  
  #------------------------------------------------------------------------------------------------------------
  
  fls <- list.files(path,pattern,full.names = T,recursive)
  
  if(verbose || dryrun){cat('Input files: ',fls,'\n')}
  
  fname <- ifelse(is.null(filename),paste0(path,'/',pattern,'.vrt '),paste0(path,'/',filename,'.vrt '))
  
  cmd1 <- paste0(gdal,'gdalbuildvrt ',stck,nodata,fname,paste0(fls,collapse = ' '))
  
  cat('Writing ',fname,'\n')
  
  if(verbose || dryrun){print(cmd1)}
  
  if(!dryrun){sysfun(cmd1)}
  
  
  if(toTiff){
    
    cmd2 <- paste0(gdal,'gdal_translate ',dtype,fname,' ',gsub('.vrt','.tif',fname))
    
    if(verbose || dryrun){print(cmd2)}
    
    cat('Writing ',gsub('.vrt','.tif',fname),'\n')
    
    if(!dryrun){sysfun(cmd2)}
    
    file.remove(fname)
    
  }
  
}

