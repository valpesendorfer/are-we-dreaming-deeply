
# remove variables except ... 

rmex <- function(...){
  
  env <- as.environment(-1)
  
  argv <- list(...)
  
  vars <- ls(envir = env)
  
  rm(list = vars[!is.element(vars, c(unlist(argv)),'rmex')],envir = env)
  
  invisible(gc())
  
}
