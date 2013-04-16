#' Download Data Package Resource
#' 
#' Download a resource referenced by a Data Package.
#' @param dataPkg The Data Package (as loaded from \code{\link{read_data_package}}) to use.
#' @param resource A character string specifying the names of the resource to retrieve
#' from the Data Package. At the time of development, it was unclear exactly how 
#' a resource would be indexed (see \url{https://github.com/dataprotocols/dataprotocols/issues/32}),
#' thus the package supports a variety of indexing mechanisms for resources. The order
#' of precedence is as follows: \enumerate{
#' \item Keys from the \code{resources} hash - If the \code{resources} value was provided as 
#' a JSON hash instead of an array, the required name prefacing each resource will be used
#' as that resource's canonical name.
#' \item The \code{id} field within each resource will be checked for any exact matches.
#' \item The \code{name} field within each resource will be checked for any exact matches.
#' \item The \code{url} and \code{path} fields within each resource will be checked for any
#' exact matches.
#' \item The \code{url} and \code{path} fields within each resource will be checked for partial
#' matches on the extracted \code{<resource>} portion of a \code{/<path>/<resource>} reference. The
#' path is extracted using \code{\link{dirname}} and ignored to see if the remainder of the
#' string matches (with the possible exclusion of a prefacing slash).
#' }
#' @param cache Whether or not to cache the resource once retrieved. If \code{TRUE}, (default) the 
#' function will check this Data Package to see if the resource has already been cached. If 
#' it has, it will simply return the resource from the cache. If it has not, the function will
#' retrieve the remote resource then save a copy in this package's cache. Note that this will
#' involve storing an extra copy of the data, doubling memory usage. If \code{FALSE}, the 
#' function will retrieve the
#' resource remotely, but not store the result in the cache when returning. The third option
#' is to set cache to \code{"flush"}. When this occurs, the function will retrieve the 
#' resource remotely, ignoring any pre-existing local copy, and store the result in the cache
#' for future use.
#' @param ... Arguments to be passed on to \link{read_json_table}.
#' @importFrom digest digest
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
get_resource <- function(dataPkg, resource, cache=TRUE, ...){	
	if (cache != TRUE && cache != FALSE && cache!= "flush"){
		stop("Invalid cache value. Must be TRUE, FALSE, or 'flush'.")
	}
	
	if (resource %in% names(dataPkg$resources)){
		ind <- which(resource == names(dataPkg$resources))
	} else if (resource %in% lapply(dataPkg$resources, "[[", "id")){
		ind <- which(resource == lapply(dataPkg$resources, "[[", "id"))
	} else if (resource %in% lapply(dataPkg$resources, "[[", "name")){
		ind <- which(resource == lapply(dataPkg$resources, "[[", "name"))
	} else{
		paths <- lapply(dataPkg$resources, "[[", "path")
		urls <- lapply(dataPkg$resources, "[[",  "url")
		
		#both a path and a URL specified somewhere
		if (any(!sapply(paths, is.null) & !sapply(urls, is.null))){
			stop("Cannot specify both a path and a URL for a single resource.")
		}
		
		merged <- paths
		merged[!sapply(urls, is.null)] <- urls[!sapply(urls, is.null)]
		
		if (resource %in% merged){
			ind <- which(merged == resource)
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
			
			if (resource %in% merged || paste("/", resource, sep="") %in% merged){
				ind <- which(resource == merged | paste("/", resource, sep="") == merged)
			} else {
				stop("Couldn't find the named resource in this Data Package.")		
			}
		}
	}
	
	if (length(ind) > 1){
		stop ("The resource named matches more than one resource in the Data Package.")
	}
	
	thisResource <- dataPkg$resources[[ind]]
	
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
		resourceCache <- pkgCache[[digest(thisResource, algo="sha256")]]
		if(!is.null(resourceCache)){
			#requested value from cache and it's available in cache. Serve from cache.
			return(resourceCache)
		}
	}
	
	#TODO: handle other types like CSV.
	
	if (!is.null(thisResource$url)){
		if (substr(thisResource$url, 0, 4) == "http"){
			url <- thisResource$url
		} else {
			url <- paste(dataPkg$base, thisResource$url, sep="/")
		}
		
		toReturn <- read_json_table(url, thisResource$schema, ...)
	} else if (!is.null(thisResource$path)){
		toReturn <- read_json_table(paste(dataPkg$base, thisResource$path, sep="/"), 
																thisResource$schema, 																
																...)
	} else{
		stop("Resource found, but no path or url field specified")
	}	
	
	if (cache=="TRUE" || tolower(cache) == "flush"){
		if (exists(thisPkgHash, envir=.cacheEnv)){
			pkgCache <- get(thisPkgHash, envir=.cacheEnv)				
		} else{
			pkgCache <- list()
		}
				
		pkgCache[[digest(thisResource, algo="sha256")]] <- toReturn
		
		assign(thisPkgHash, pkgCache, envir=.cacheEnv)
	}	
	
	return(toReturn)
	
}



