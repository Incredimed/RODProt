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
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
get_file <- function(dataPkg, file){	
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
	
	#TODO: handle other types like CSV.
	
	if (!is.null(thisFile$url)){
		if (substr(thisFile$url, 0, 4) == "http"){
			url <- thisFile$url
		} else {
			url <- paste(dataPkg$base, thisFile$url, sep="/")
		}
		
		return(read_json_table(url, thisFile$schema))
	} else if (!is.null(thisFile$path)){
		return(read_json_table(paste(dataPkg$base, thisFile$path, sep="/"), thisFile$schema))
	} else{
		stop("File found, but no path or url field specified")
	}	
	
}



