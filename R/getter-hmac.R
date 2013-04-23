#' HMAC getter
#' 
#' Getter which uses the \code{httr} package with HMAC authentication
#' to retrieve objects.
#' @importFrom httr GET
#' @importFrom digest hmac
#' @importFrom RCurl base64Encode
#' @importFrom httr parse_url
#' @export
#' @exportClass HMACGetter
#' @aliases HMACGetter-class
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
HMACGetter <- setRefClass("HMACGetter", 
	fields = list(
		key = "character",
		id = "character"
	),
	methods = list(
	 	initialize = function(id, key, ...){
	 		initFields(id=id, key=key, ...)
	 	},
	 	get = function(uri){
	 		uri <- parse_url(uri)
	 		protocol <- uri$scheme
			
	 		host <- uri$hostname
	 		if (!is.null(uri$port)){
	 			host <- paste(uri$hostname, uri$port, sep=":")
	 		}
	 			
			path <- paste("/", uri$path, sep="")
			
			msTimestamp <- round(as.numeric(as.POSIXlt(Sys.time(), tz="GMT"))*1000)
			
	 		query <- uri$query	 		
	 		if (length(query) > 0){
		 		query <- query[order(names(query))]
		 		query <- paste(paste(names(query), "=", query, sep=""), collapse="&")
	 		}
	 		
			obj <- paste(
				"GET\n",
				host,"\n",
				paste(paste(protocol, "://", host, sep=""),path,sep=""),"\n",
				msTimestamp,"\n",
				query, sep="")
				 		
	 		hmac <- hmac(
	 			key=key, 
	 			object=obj, 
	 			algo="sha1", raw=TRUE)
			
	 		# until https://github.com/QBRC/CasHmac/issues/1 gets fixed
	 		if (is.null(query)){ 
		 		reqURL <- paste(paste(protocol, "://", host, sep=""),
		 										path,sep="")
	 		} else{
	 			reqURL <- paste(paste(protocol, "://", host, sep=""),
	 											path,
	 											"?",query,sep="")
	 		}
	 		
	 		req <- GET(reqURL, add_headers(Date=msTimestamp,
	 		 															 ClientId=id,
	 																	 Signature=base64Encode(hmac)))	 		
			content(req, as="text")
	 	}
	)
)