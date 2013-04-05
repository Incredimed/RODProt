#' Flush the cache.
#' 
#' Flushes the internal RODProt cache. Mainly just used for debugging and testing.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
flush_cache <- function(){
	rm(list=ls(envir=.cacheEnv), envir=.cacheEnv)	
}