
calcThresh <- function(x,nodataMask=NULL,handEM=NULL,filename=NULL,parallel=TRUE,inmemory=TRUE){

# Threshold function for binarizing grayscale images
#
# Code based on implementation by Twele et al. (2016) for detecting flooded areas on Sentinel-1 SAR images 
#
#
# References:
#
# 
#  Kittler, J. and Illingworth, J. 1986. Minimum Error Thresholding. Pattern Recognition 19 (1): 41-47
#
#  Twele, A., Cao, W., Plank, S., Martinis, S. 2016. Sentinel-1-based flood mapping: a fully automated processing chain.
#  International journal of remote sensing 37 (13: 2990-3004)
#
#
#
#
# by Valentin Pesendorfer, 2016
#---------------------------------------------------------------------------------------------------------------------------------------
  
# Helper functions

ielse <- function(cond,ieT,ieF){if(cond) return(ieT) else return(ieF)}

defTileDF <- function(x,tilesize){
  
  xoffset <- ceiling(ncol(x)%%tilesize/2)
  yoffset <- ceiling(nrow(x)%%tilesize/2)
  
  trws <- seq(1+yoffset,nrow(x),tilesize)
  tcls <- seq(1+xoffset,ncol(x),tilesize)
  
  tiles <- expand.grid(rows=1:(length(trws)-1),cols=1:(length(tcls)-1))
  
  df <- tbl_df(data.frame(tileNr = 1:nrow(tiles),brow = trws[tiles$rows],bcol=tcls[tiles$cols]))
  
  
  return(df)
}

calcTileStats <- function(x,df,NDmsk,handEM){
  
  rlist <-  list() #prepare result list
  
  cat('Calculating image statistics: \n \n')
  
  pb <- txtProgressBar(1,nrow(df),1,style=3) # progress bar
  for (i in 1:nrow(df)){
    setTxtProgressBar(pb,i)
    
    val <- getValuesBlock(x,df$brow[i],200,df$bcol[i],200)
    
    if(!is.null(NDmsk)){
    
      mskval <- as.logical(getValuesBlock(NDmsk,df$brow[i],200,df$bcol[i],200))
      val[mskval] <- NA
    }
    
    if(any(is.na(val))){
      rlist[[i]] <- rep(NA,11)
      next}
    
    if(!is.null(handEM)){
    
      handEM_val <- getValuesBlock(handEM,df$brow[i],200,df$bcol[i],200)
    
    }else{handEM_val <- 1} # constant gives every tile 0% excluded pixles in case no HandEM is supplied
    
    
    rlist[[i]] <- c(
      
      mean(val,na.rm=TRUE),
      sd(val,na.rm=TRUE),
      (1-(sum(handEM_val)/length(handEM_val)))*100, # percent of tiles with HAND > 15 m
      sd(c(mean(val[1:50],na.rm=TRUE),mean(val[51:100],na.rm=TRUE),mean(val[101:150],na.rm=TRUE),mean(val[151:200],na.rm=TRUE))),
      mean(val[1:50],na.rm=TRUE),
      sd(val[1:50],na.rm=TRUE),
      mean(val[51:100],na.rm=TRUE),
      sd(val[51:100],na.rm=TRUE),
      mean(val[101:150],na.rm=TRUE),
      sd(val[101:150],na.rm=TRUE),
      mean(val[151:200],na.rm=TRUE),
      sd(val[151:200],na.rm=TRUE))
     
    
  }
  
  close(pb) 
  
  res <- do.call(rbind,rlist)
 
  colnames(res) <- c('Tmean','Tsd','handEMpercent','TCMeanSD','C1mean','C1Sd','C2mean','C2sd','C3mean','C3sd','C4mean','C4sd')
  
  return(data.frame(res))
  
}

calcTileStatsParallel <- function(x,df,NDmsk,handEM){
  
  cat('Calculating image statistics in parallel mode - grab a coffee, this may take some time ...')
 
  #cluster
  cl <- makeCluster(detectCores())    #create a cluster
  registerDoParallel(cl)              #register the cluster
  
  rlist <-  foreach (i = 1:nrow(df), .packages = 'raster') %dopar% {
    
    val <- getValuesBlock(x,df$brow[i],200,df$bcol[i],200)
    
    if(!is.null(NDmsk)){
      
      mskval <- as.logical(getValuesBlock(NDmsk,df$brow[i],200,df$bcol[i],200))
      val[mskval] <- NA
    }
    
    if(any(is.na(val))){
      res <- rep(NA,11)
    }
    
    if(!is.null(handEM)){
      
      handEM_val <- getValuesBlock(handEM,df$brow[i],200,df$bcol[i],200)
      
    }else{handEM_val <- 1} # constant gives every tile 0% excluded pixles in case no HandEM is supplied
    
    res <- c(
      
      mean(val,na.rm=TRUE),
      sd(val,na.rm=TRUE),
      (1-(sum(handEM_val)/length(handEM_val)))*100, # percent of tiles with HAND > 15 m
      sd(c(mean(val[1:50],na.rm=TRUE),mean(val[51:100],na.rm=TRUE),mean(val[101:150],na.rm=TRUE),mean(val[151:200],na.rm=TRUE))),
      mean(val[1:50],na.rm=TRUE),
      sd(val[1:50],na.rm=TRUE),
      mean(val[51:100],na.rm=TRUE),
      sd(val[51:100],na.rm=TRUE),
      mean(val[101:150],na.rm=TRUE),
      sd(val[101:150],na.rm=TRUE),
      mean(val[151:200],na.rm=TRUE),
      sd(val[151:200],na.rm=TRUE))
    
    
    
  }
  
  #stop the cluster
  stopCluster(cl)
  
  cat('done. \n \n')

  res <- do.call(rbind,rlist)
  
  colnames(res) <- c('Tmean','Tsd','handEMpercent','TCMeanSD','C1mean','C1Sd','C2mean','C2sd','C3mean','C3sd','C4mean','C4sd')
  
  return(data.frame(res))
  
}

binarize <- function(x,t,inmemory,filename){
  
  xbin <- raster(x)
  bs <- blockSize(x, minrows = 64)
  
  
  
  if (inmemory){
    
    cat('Producing binary image ... \n')
    
    pb <- txtProgressBar(1,bs$n,1,style=3) # progress bar
  
    vallist <- list()
  
    for (r in 1:bs$n){
    
      valtmp <- getValues(x,row=bs$row[r],nrows = bs$nrows[r])
    
      vallist[[r]] <- (valtmp <= t)*1
      
      setTxtProgressBar(pb,r)
    
    }
    
    close(pb)
    
    
    xbin <- setValues(xbin,unlist(vallist))
    
    
  }else{
    
    
    if(is.null(filename)){filename <- rasterTmpFile();cat('Producing binary image ... \n')
    }else{cat('Producing binary image, writing to file:',filename,' ...\n')}
    
    cat('\n')
    
    pb <- txtProgressBar(1,bs$n,1,style=3) # progress bar
    
    xbin <- writeStart(xbin, filename,datatype='INT1U')
    
    for (r in 1:bs$n){
    
      valtmp <- getValues(x,row=bs$row[r],nrows = bs$nrows[r])
      
      valtmp <- (valtmp <= t)*1
    
      xbin <- writeValues(xbin, valtmp, bs$row[r])
      
      setTxtProgressBar(pb,r)
      
    }
    
    xbin <- writeStop(xbin)
    
    close(pb)
    
  }
  
  
  
  return(xbin)
  
}

binarizeParallel <- function(x,t){
  
  cat('Producing binary image ... \n')
  
  xbin <- raster(x)
  bs <- blockSize(x, minrows = 64)
  
  #cluster
  cl <- makeCluster(detectCores())    #create a cluster
  registerDoParallel(cl)              #register the cluster
  
  vls <- foreach(r = 1:bs$n, .packages = "raster") %dopar% {
    
    tmp <- getValues(x,row = bs$row[r],nrows = bs$nrows[r])
    
    tmp <- (tmp <= t) * 1 # convert logical to numerical
    
  }
  
  xbin <- setValues(xbin,unlist(vls))
  
  #stop the cluster
  stopCluster(cl)
  
  return(xbin)
  
}

#---------------------------------------------------------------------------------------------------------------------------------------

# Libraries
  
libs <- ielse(parallel,c("raster","dplyr","doParallel","foreach"),c("raster","dplyr"))
new_p <- setdiff(libs, rownames(installed.packages())) 
if (length(new_p) != 0 ) install.packages(new_p) 
  
dmp <- lapply(libs, require, character.only = TRUE)
  
  
if(!any(unlist(dmp))) warning('Problem occured while loading packages!')
rm(dmp)
  
#---------------------------------------------------------------------------------------------------------------------------------------

## Argument management and paramters

# SAR greyscale image
if(missing(x)){stop('Please supply valid input raster! \n')
}else{ielse(typeof(x) == "character",x <- raster(x),ielse(typeof(x) == "S4",'',stop('Please supply valid input raster! \n')))}

# NoData mask
if(!is.null(nodataMask)){ielse(typeof(nodataMask) == "character",nodataMask <- raster(nodataMask),ielse(typeof(nodataMask) == "S4",'',stop('Please supply valid nodataMask raster! \n')))}

# HAND exclusionmask
if(!is.null(handEM)){ielse(typeof(handEM) == "character",handEM <- raster(handEM),ielse(typeof(handEM) == "S4",'',stop('Please supply valid raster for the HAND exclusion mask! \n')))}

# tilesize and quantile defaults
tilesize <- 200 
qnt <- 0.95

# filename management
if(!is.null(filename)){
  inmemory <- FALSE
  if(dirname(filename) == '.'){
    filename <- paste0(getwd(),'/',filename)
  }
  
  if(extension(filename) == ''){
    extension(filename)  <- '.tif'
  }
}
#---------------------------------------------------------------------------------------------------------------------------------------

# Processing

while(TRUE){

# create initial tile dataframe

tiledf <- defTileDF(x,tilesize)

# Calculate tile statistics and append

if(parallel){ 
  
  tiledf <- cbind(tiledf,calcTileStatsParallel(x,tiledf,nodataMask,handEM))
  
}else{
  
  tiledf <- cbind(tiledf,calcTileStats(x,tiledf,nodataMask,handEM))  
  
}

invisible(gc())

# filter tiles for threshold computation

tiledf <-filter(na.omit(tiledf),TCMeanSD > quantile(TCMeanSD,qnt) & Tmean < mean(Tmean) & handEMpercent < 20) %>%
	top_n(5,TCMeanSD)


if(nrow(tiledf > 0)){

	break

}else{

	tilesize <- tilesize/2
	qnt <- 0.9

}

}

reslist <- list() # empty list for thresholds


cat('\nComputing threshold ... ')
 
for (tile in 1:nrow(tiledf)){ #loop over best tiles

	vals <- round(getValuesBlock(x,tiledf$brow[tile],tilesize,tiledf$bcol[tile],tilesize))

	tilestats <- data.frame(v=vals) %>% group_by(v) %>% tally() %>% mutate(p=n/(200*200))
	
	tmpmat <- matrix(rep(NA,256*2),ncol=2) #temporary result matrix
	
	for (thresh in 0:255){ # loop through possible thresholds
	
		threshstats <- mutate(tilestats,group = ifelse(v <= thresh,'C1','C2')) %>% group_by(group) %>% summarise_each(funs(mean,sum,sd))
	
		if (nrow(threshstats) < 2){next} # if less then 2 classes continue to next threshold
	
		Jt <- 1+2*(threshstats$p_sum[1]*log(threshstats$v_sd[1]) + threshstats$p_sum[2]*log(threshstats$v_sd[2])) - 
				2*(threshstats$p_sum[1]*log(threshstats$p_sum[1])+threshstats$p_sum[2]*log(threshstats$p_sum[2]))
				
		tmpmat[thresh+1,] <- c(thresh,Jt)
	
	}
	
	reslist[[tile]] <- na.omit(tmpmat)
	

}


localThreshs <- lapply(reslist, function(x) x[which(x[,2] == min(x[,2])),1])


### Currently not implemented:

# Evaluate threshold performance based on SD of L- children

# Histogram merging

# empirically derived TH

###


globalThresh <- mean(unlist(localThreshs))

cat('threshold found! T =',globalThresh,'\n \n')



if(inmemory & parallel & is.null(filename)){
  
  xBinclass <- binarizeParallel(x,globalThresh)

} else {
  

  xBinclass <- binarize(x,globalThresh,inmemory,filename)

}

cat('\n... done.\n')

invisible(gc())

return(xBinclass)

}