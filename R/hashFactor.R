#' Create a hashFactor
#' 
#' Creates a hashFactor object, which operates in much the same way as a factor,
#' but doesn't require a continuous sequence of integers for the levels.
#' @param x The values to be encoded into a factor
#' @param levels The labels associated with each level of the factor
#' @param codes the integer codes associated with each level of the factor
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @examples
#' fac <- hashFactor(sample(LETTERS[1:5], 10, replace=TRUE), 
#' 	levels=LETTERS[1:5], codes=c(1,5,8,2,6))
#' d <- data.frame(i=11:20, f=fac)
#' @export
hashFactor <- function(x=character(), levels, codes){
	if (length(codes) != length(unique(codes))){
		stop("All of your codes must be unique")
	}
	f <- factor(x, levels)
	attr(f, 'codes') <- codes
	class(f) <- c('hashFactor', class(f))
	f
}

#' Access hashFactor
#' @param x The index specifying which element to extract
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
`[.hashFactor` <- function(x, ...) {
	y <- NextMethod('[')
	attr(y, 'codes') <- attr(x, 'codes')
	y
}

#' Convert a hashFactor to integer
#' @param x hashFactor to be coerced to integer
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
as.integer.hashFactor <- function(x, ...) {
	attr(x,'codes')[unclass(x)]
}