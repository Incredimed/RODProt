#' Print a dataPackage
#' 
#' Print a list of class \code{dataPackage} in a clean way.
#' @usage \method{print}{dataPackage} (x, ...)
#' @param x The data package as read from \link{read_data_package}.
#' @param ... Ignored. Provided to be consistent with signature of global print function.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
print.dataPackage <- function(x, ...){
	if (!"dataPackage" %in% class(x)){
		stop("The provided package wasn't properly formatted. Read in the package using the read_data_package() function.")
	}
	
	cat("Data Package Title/Name:\n\t")
	cat(x$title, " ('", x$name, "')", sep="")
	if (!is.null(x$version)){
		cat(" -", x$version)
	}
	cat("\n")
	if (!is.null(x$description)){
		cat("Description:\n\t", x$description, "\n")
	}
}