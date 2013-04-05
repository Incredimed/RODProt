context("get_file")

test_that("get local file works", {
	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	file <- get_file(pkg, "data.json")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
	})

test_that("get remote file works", {
	
	pkg <- read_data_package("http://raw.github.com/QBRC/RODProt/3990112d90caa5685771e6039e88a48277e993f5/inst/extdata/datapackage.json")
	file <- get_file(pkg, "data.json")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
})

test_that("mixed URLs and paths works", {
	
	pkg <- read_data_package("../extdata/datapackage.json")
	pkg$files[[2]]$path <- NULL
	pkg$files[[2]]$url <- "http://raw.github.com/QBRC/RODProt/3990112d90caa5685771e6039e88a48277e993f5/inst/extdata/data2.json"
	
	file <- get_file(pkg, "data2.json")
	expected <- data.frame(Column.A=7:9, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
	file <- get_file(pkg, "data.json")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
	
})

test_that("Cannot set both URL and path", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$files[[1]]$url <- "A"
	pkg$files[[2]]$path <- "B"
	
	expect_error(get_file(pkg, "A"), "not specify both")	
})


test_that("Escaped path works properly", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$files[[1]]$path <- "../extdata/data.json"
		
	file <- get_file(pkg, "data.json")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Multiple matched names fails", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$files[[1]]$path <- "data.json"
	pkg$files[[2]]$path <- "data.json"
	
	expect_error(get_file(pkg, "data.json"), "matches more than one")	
})


#### test based on different ways of indexing/naming a file

test_that("Field hash name takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	names(pkg$files) <- c("A", "B")
	pkg$files[[2]]$id <- "A"
		
	file <- get_file(pkg, "A")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Field ID takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$files[[1]]$id <- "A"
	pkg$files[[2]]$name <- "A"
	
	file <- get_file(pkg, "A")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Field Name takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$files[[1]]$name <- "A"
	pkg$files[[2]]$url <- "A"
	pkg$files[[2]]$path <- "A"
	
	file <- get_file(pkg, "A")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)	
})

test_that("Exact URL/path takes precedence", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	pkg$files[[1]]$path <- "data.json"
	pkg$files[[2]]$path <- "../extdata/data.json"
	
	file <- get_file(pkg, "data.json")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
})

test_that("Trimmed path works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
		
	pkg$files[[1]]$path <- "../extdata/data.json"
	
	file <- get_file(pkg, "data.json")
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(file, expected)
})


