
calcThresh <- function(x,hand=NULL,filename=NULL){

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

calcTileStats <- function(x,df){
  
  rlist <-  list() #prepare result list
  
  cat('Calculating image statistics: \n \n')
  
  pb <- txtProgressBar(1,nrow(df),1,style=3) # progress bar
  for (i in 1:nrow(df)){
    setTxtProgressBar(pb,i)
    
    val <- getValuesBlock(x,df$brow[i],200,df$bcol[i],200)
    
    rlist[[i]] <- c(
      
      mean(val),
      sd(val),
      sd(c(mean(val[1:50]),mean(val[51:100]),mean(val[101:150]),mean(val[151:200]))),
      mean(val[1:50]),
      sd(val[1:50]),
      mean(val[51:100]),
      sd(val[51:100]),
      mean(val[101:150]),
      sd(val[101:150]),
      mean(val[151:200]),
      sd(val[151:200]))
     
    
  }
  
  close(pb) 
  
  res <- do.call(rbind,rlist)
  colnames(res) <- c('Tmean','Tsd','TCMeanSD','C1mean','C1Sd','C2mean','C2sd','C3mean','C3sd','C4mean','C4sd')
  
  return(data.frame(res))
  
}

binarize <- function(x,t){
  
  xbin <- raster(x)
  vals <- getValues(x)
  xbin <- setValues(xbin,vals <= t)

  return(xbin)
}

#---------------------------------------------------------------------------------------------------------------------------------------

# Argument management and paramters

if(missing(x)){stop('Please supply valid input raster! \n')
}else{ielse(typeof(x) == "character",x <- raster(x),ielse(typeof(x) == "S4",'',stop('Please supply valid input raster! \n')))}

# HAND currently not implemented

# if(missing(hand)){hand <- NULL
# }else{ielse(typeof(hand) == "character",hand <- raster(hand),hand <- NULL); warning('HAND raster invalid \n')}

tilesize <- 200 # default
qnt <- 0.95
#---------------------------------------------------------------------------------------------------------------------------------------

# Processing

while(TRUE){

# create initial tile dataframe

tiledf <- defTileDF(x,tilesize)

# Calculate tile statistics and append

tiledf <- cbind(tiledf,calcTileStats(x,tiledf))

# filter tiles for threshold computation

tiledf <-filter(tiledf,TCMeanSD > quantile(TCMeanSD,qnt) & Tmean < mean(Tmean)) %>%
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
 
for (tile in 1:5){ #loop over 5 best tiles

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

globalThresh <- mean(unlist(localThreshs))

cat('threshold found! T =',globalThresh)
cat(' - Producing binary image ...')

xBinclass <- binarize(x,globalThresh)


cat('done.\n')

if (!is.null(filename)){
  writeRaster(xBinclass, filename=filename, datatype="INT1U", overwrite=TRUE)
}


return(xBinclass)

}