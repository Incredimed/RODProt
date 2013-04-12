#' Download Data Package File
#' 
#' Download a file referenced by a Data Package.
#' @param dataPkg The Data Package (as loaded from \code{\link{read_data_package}}) to use.
#' @param file A character string specifying the names of the file to retrieve
#' from the Data Package. At the time of development, it was unclear exactly how 
#' a file would be indexed (see \url{https://github.com/dataprotocols/dataprotocols/issues/32}),
#' thus the package supports a variety of indexing mechanisms for files. The order
#' of precedence is as follows: \enumerate{
#' \item Keys from the \code{files} hash - If the \code{files} value was provided as 
#' a JSON hash instead of an array, the required name prefacing each file will be used
#' as that file's canonical name.
#' \item The \code{id} field within each file will be checked for any exact matches.
#' \item The \code{name} field within each file will be checked for any exact matches.
#' \item The \code{url} and \code{path} fields within each file will be checked for any
#' exact matches.
#' \item The \code{url} and \code{path} fields within each file will be checked for partial
#' matches on the extracted \code{<file>} portion of a \code{/<path>/<file>} reference. The
#' path is extracted using \code{\link{dirname}} and ignored to see if the remainder of the
#' string matches (with the possible exclusion of a prefacing slash).
#' }
#' @param cache Whether or not to cache the file once retrieved. If \code{TRUE}, (default) the 
#' function will check this Data Package to see if the file has already been cached. If 
#' it has, it will simply return the file from the cache. If it has not, the function will
#' retrieve the remote file then save a copy in this package's cache. Note that this will
#' involve storing an extra copy of the data, doubling memory usage. If \code{FALSE}, the 
#' function will retrieve the
#' file remotely, but not store the result in the cache when returning. The third option
#' is to set cache to \code{"flush"}. When this occurs, the function will retrieve the 
#' file remotely, ignoring any pre-existing local copy, and store the result in the cache
#' for future use.
#' @param ... Arguments to be passed on to \link{read_json_table}.
#' @importFrom digest digest
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
get_file <- function(dataPkg, file, cache=TRUE, ...){	
	if (cache != TRUE && cache != FALSE && cache!= "flush"){
		stop("Invalid cache value. Must be TRUE, FALSE, or 'flush'.")
	}
	
	if (file %in% names(dataPkg$files)){
		ind <- which(file == names(dataPkg$files))
	} else if (file %in% lapply(dataPkg$files, "[[", "id")){
		ind <- which(file == lapply(dataPkg$files, "[[", "id"))
	} else if (file %in% lapply(dataPkg$files, "[[", "name")){
		ind <- which(file == lapply(dataPkg$files, "[[", "name"))
	} else{
		paths <- lapply(dataPkg$files, "[[", "path")
		urls <- lapply(dataPkg$files, "[[",  "url")
		
		#both a path and a URL specified somewhere
		if (any(!sapply(paths, is.null) & !sapply(urls, is.null))){
			stop("Cannot specify both a path and a URL for a single file.")
		}
		
		merged <- paths
		merged[!sapply(urls, is.null)] <- urls[!sapply(urls, is.null)]
		
		if (file %in% merged){
			ind <- which(merged == file)
		} else{
			merged <- unlist(merged)
			
			#eliminate any prefaced "./"s
			merged <- gsub("^(\\./)+", "", merged, perl=TRUE)
			
			#eliminate any "./"s mid-line
			merged <- gsub("([^\\w])(\\./)+", "\\1", merged, perl=TRUE)
			
			#trim the dirnames off
			dirs <- dirname(merged)
			dirs[dirs == "."] <- ""
			merged <- substr(merged, nchar(dirs)+1, nchar(merged))
			
			if (file %in% merged || paste("/", file, sep="") %in% merged){
				ind <- which(file == merged | paste("/", file, sep="") == merged)
			} else {
				stop("Couldn't find the named file in this Data Package.")		
			}
		}
	}
	
	if (length(ind) > 1){
		stop ("The file named matches more than one file in the Data Package.")
	}
	
	thisFile <- dataPkg$files[[ind]]
	
	#compute a hash on the list of arguments passed in which will be passed to
	# other functions. We don't really care what they are in this function, but
	# we need to consider them for the purpose of caching. An object created in
	# this function passing a certain set of arguments to a child function can not
	# be guaranteed to be the same as the object with a different set of ... 
	# arguments. So include this hash in our caching key.
	argsHash <- digest(list(...), algo="sha256")
	
	thisPkgHash <- paste(dataPkg$hash, argsHash, sep="-")
	
	if (cache==TRUE && exists(thisPkgHash, envir=.cacheEnv)){
		pkgCache <- get(thisPkgHash, envir=.cacheEnv)		
		fileCache <- pkgCache[[digest(thisFile, algo="sha256")]]
		if(!is.null(fileCache)){
			#requested value from cache and it's available in cache. Serve from cache.
			return(fileCache)
		}
	}
	
	#TODO: handle other types like CSV.
	
	if (!is.null(thisFile$url)){
		if (substr(thisFile$url, 0, 4) == "http"){
			url <- thisFile$url
		} else {
			url <- paste(dataPkg$base, thisFile$url, sep="/")
		}
		
		toReturn <- read_json_table(url, thisFile$schema, ...)
	} else if (!is.null(thisFile$path)){
		toReturn <- read_json_table(paste(dataPkg$base, thisFile$path, sep="/"), 
																thisFile$schema, 																
																...)
	} else{
		stop("File found, but no path or url field specified")
	}	
	
	if (cache=="TRUE" || tolower(cache) == "flush"){
		if (exists(thisPkgHash, envir=.cacheEnv)){
			pkgCache <- get(thisPkgHash, envir=.cacheEnv)				
		} else{
			pkgCache <- list()
		}
				
		pkgCache[[digest(thisFile, algo="sha256")]] <- toReturn
		
		assign(thisPkgHash, pkgCache, envir=.cacheEnv)
	}	
	
	return(toReturn)
	
}



