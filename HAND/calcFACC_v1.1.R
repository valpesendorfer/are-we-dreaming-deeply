# Expects flow direction raster (fdir) encoded like so:

###################
#  32 | 64 | 128  #
#  16 | <> |   1  #
#   8 |  4 |   2  #
###################

calcFACC <- function(fdir, filename = NULL){
  
  require(raster)
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
  
  #
  
  writeToMat <- function(x, row, col, value){
    pos    <- indFromRowCol(x = x, row = row, col = col)
    x[pos] <- value
    return(x)
  }
  # Significantly slower:
  # writeToMat <- function(x, row, col, value){
  #   x[indFromRowCol(x = x, row = row, col = col)] <- value
  #   return(x)
  # }
  #
  
  rowFromInd <- function(x, ind){
    (ind - 1) %% ncol(x) + 1
  }
  
  #
  
  colFromInd <- function(x, ind){
    (ind - 1) %/% nrow(x) + 1
  }
  
  #
  
  indFromRowCol <- function(x, row, col){
    ind      <- (col - 1) * nrow(x) + row 
    sel      <- row < 1 | row > nrow(x) | col < 1 | col > ncol(x)
    ind[sel] <- NA
    ind
  }
  
  #
  
  updateFocal_3x3 <- function(x, r, ind, value){
    if (length(value) == 1) value <- rep(value, length(ind) * 9)
    
    rel_pos <- c(9, 8, 7, 6, 5, 4, 3, 2, 1)
    
    shft    <- c(-ncol(r) + -1:1, -1:1, ncol(r) + -1:1)
    rowX    <- shft + rep(ind, each = length(shft))
    colX    <- rep(rel_pos, length(ind))
    
    do      <- which(rowFromCell(r, cell=rowX) - 
                       rep(rowFromCell(r, ind), each = length(shft)) == 
                       rep(rep(-1:1, each = 3), length(ind)))
    
    
    
    x <- writeToMat(x, row = rowX[do], col = colX[do], value = value[do])
    cleanPad(x = x, r = r)
  }
  
  #-----------------------------------------------------------------------------
  
  cell_id   <- raster(fdir)
  cell_id[] <- seq_len(ncell(fdir))
  
  cell_nei  <- getValuesFocal(cell_id, ngb=c(3, 3))
  cell_nei  <- cleanPad(x = cell_nei, r = cell_id)
  
  fdir_to   <- values(fdir)
  fdir_to[fdir_to == 0 | fdir_to > 128]  <- NA
  fdir_to   <- multiReclass(fdir_to, 
                            from = c(32, 64, 128, 16, 1, 8, 4, 2),
                            to   = seq_len(9)[-5])
  
  fdir_to   <- getFromMat(cell_nei, row = seq_len(nrow(cell_nei)), col = fdir_to)
  
  rm(cell_id, cell_nei)
  invisible(gc())
  
  dir_vec   <- c(2, 4, 8, 1, 0, 16, 128, 64, 32)
  flow_acc  <- rep(0, ncell(fdir))
  
  fdir_nei  <- getValuesFocal(fdir, ngb=c(3, 3))
  fdir_nei  <- cleanPad(x = fdir_nei, r = fdir)

  to_do     <- seq_len(ncell(fdir))
  i         <- 0
  #-------------------------------------------------------------------------------
  while (TRUE){
  
    flow_in                   <- t((t(fdir_nei[to_do, , drop=FALSE]) - dir_vec) == 0)
    flow_in[is.na(flow_in)]   <- FALSE
    
    if (!any(flow_in[, -5])) break
    
    in_N                      <- rowSums(flow_in[, -5])
    no_in                     <- to_do[in_N == 0]
  
    flow_dst                  <- fdir_to[no_in]
    valid_dst                 <- !is.na(flow_dst)
    
    flow_src                  <- no_in[valid_dst]
    flow_dst                  <- flow_dst[valid_dst]
    
    if (length(flow_dst) == 0) break
    
    repeat {
      dups                        <- duplicated(flow_dst)
      do_add                      <- which(!dups)
      
      flow_acc[flow_dst[do_add]]  <- flow_acc[flow_dst[do_add]] + 
        flow_acc[flow_src[do_add]] + 1
      flow_dst                    <- flow_dst[-do_add]
      flow_src                    <- flow_src[-do_add]
  
      if (length(flow_dst) == 0) break
    }
  
    fdir_nei                  <- updateFocal_3x3(x     = fdir_nei, 
                                                 ind   = no_in, 
                                                 value = Inf,
                                                 r     = fdir)
    fdir_to[no_in]            <- NA
    to_do                     <- to_do[in_N != 0]
    if (length(to_do) == 0) break
    i                         <- i + 1
    if (i %% 10 == 0){
      cat(".")
    }
  }
  #-------------------------------------------------------------------------------
  
  resr     <- raster(fdir)
  resr[]   <- flow_acc
  
  if (!is.null(filename)){
    writeRaster(resr, filename=filename, datatype="FLT4S", overwrite=TRUE)
  }
  cat(' done. \n')
  return(resr)
}

