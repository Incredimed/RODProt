context("get_resource")

test_that("get local file works", {
	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	file <- get_resource(pkg, "data.json", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
	})

test_that("get remote file works", {
	
	pkg <- read_data_package("http://raw.github.com/QBRC/RODProt/master/inst/extdata/datapackage.json")
	file <- get_resource(pkg, "data.json", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
})

test_that("get remote CSV file works", {
  
  pkg <- read_data_package("http://raw.github.com/datasets/house-prices-us/0b5042bbe9a2c2537a3bd708287321396b534710/datapackage.json")
  file <- get_resource(pkg, "national", cache=FALSE, overlook.types=TRUE)
  expect_equal(2, ncol(file))
  expect_equal(27, nrow(file))
})

test_that("mixed URLs and paths works", {
	
	pkg <- read_data_package("../extdata/datapackage.json")
	pkg$resources[[2]]$path <- NULL
	pkg$resources[[2]]$url <- "http://raw.github.com/QBRC/RODProt/3990112d90caa5685771e6039e88a48277e993f5/inst/extdata/data2.json"
	
	file <- get_resource(pkg, "data2.json", cache=FALSE)
	expected <- data.frame(A=7:9, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
	file <- get_resource(pkg, "data.json")
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
})

test_that("Cannot set both URL and path", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$resources[[1]]$url <- "A"
	pkg$resources[[2]]$path <- "B"
	
	expect_error(get_resource(pkg, "A", cache=FALSE), "not specify both")	
})


test_that("Escaped path works properly", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$resources[[1]]$path <- "../extdata/data.json"
		
	file <- get_resource(pkg, "data.json", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Multiple matched names fails", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$resources[[1]]$path <- "data.json"
	pkg$resources[[2]]$path <- "data.json"
	
	expect_error(get_resource(pkg, "data.json", cache=FALSE), "matches more than one")	
})


#### test based on different ways of indexing/naming a file

test_that("Field hash name takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	names(pkg$resources) <- c("A", "B")
	pkg$resources[[2]]$id <- "A"
		
	file <- get_resource(pkg, "A", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Field ID takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$resources[[1]]$id <- "A"
	pkg$resources[[2]]$name <- "A"
	
	file <- get_resource(pkg, "A", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Field Name takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$resources[[1]]$name <- "A"
	pkg$resources[[2]]$url <- "A"
	pkg$resources[[2]]$path <- "A"
	
	file <- get_resource(pkg, "A", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Exact URL/path takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$resources[[1]]$path <- "data.json"
	pkg$resources[[2]]$path <- "../extdata/data.json"
	
	file <- get_resource(pkg, "data.json", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
})

test_that("Trimmed path works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
		
	pkg$resources[[1]]$path <- "../extdata/data.json"
	
	file <- get_resource(pkg, "data.json", cache=FALSE)
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
})



#### Test caching

test_that("No cache when disabled", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	#manually set hash to verify in testing
	pkg$hash <- "testhash"
	
	flush_cache()
	
	expect_true(!exists("testhash", envir=.cacheEnv))
	expect_equal(length(ls(envir=.cacheEnv)),0)
			
 	file <- get_resource(pkg, "data.json", cache=FALSE)

	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
	
	expect_equal(length(ls(envir=.cacheEnv)),0)
	
	flush_cache()
})

test_that("Cache when enabled", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	#manually set hash to verify in testing
	pkg$hash <- "testhash"
	
	expect_equal(length(ls(envir=.cacheEnv)), 0)
	
	file <- get_resource(pkg, "data.json", cache=TRUE)
	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
	
	expect_equal(length(ls(envir=.cacheEnv)),1)
	
	items <- ls(envir=.cacheEnv)
	varHash <- items[grepl("^testhash-", items)]
	pkgCache <- get(varHash, envir=.cacheEnv)
	expect_identical(pkgCache[[1]], file)
	
	flush_cache()
})


test_that("Cache flush works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	#manually set hash to verify in testing
	pkg$hash <- "testhash"
	
	#check that cache is currently empty
	expect_true(!exists("testhash", envir=.cacheEnv))
	
	file <- get_resource(pkg, "data.json", cache=TRUE)
	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
	
	#grab the value out of the cache and check for equality
	items <- ls(envir=.cacheEnv)
	expect_equal(length(items), 1)
	varHash <- items[grepl("^testhash-", items)]
	pkgCache <- get(varHash, envir=.cacheEnv)
	expect_identical(pkgCache[[1]], file)
	
	#corrupt the cache
	pkgCache[[1]][,1] <- 0:2	
	assign(varHash, pkgCache, envir=.cacheEnv)
	
	#check that it's serving the corrupted file
	file <- get_resource(pkg, "data.json", cache=TRUE)
	expect_false(identical(file, expected))
	
	#flush the cache and check that proper file is restored
	file <- get_resource(pkg, "data.json", cache="flush")
	expect_identical(file, expected)
	pkgCache <- get(varHash, envir=.cacheEnv)
	expect_identical(pkgCache[[1]], file)
	
	flush_cache()
})


test_that("Invalid cache throws error", {	
	pkg <- read_data_package("../extdata/datapackage.json")
			
	expect_error(file <- get_resource(pkg, "data.json", cache="foo"), "cache value")
	
	flush_cache()
})

