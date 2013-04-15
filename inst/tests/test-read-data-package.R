context("read_data_package")
test_that("local data package works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 6)
	expect_equal(pkg$base, "../extdata")
	expect_equal(pkg$hash, "e8ef7db80494d4c77b384ef43a1595269503d8e0e24070b3b12ccce7a454b1ee")
	})

test_that("remote data package works", {
	url <- "http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata/datapackage.json"
	pkg <- read_data_package(url)
		
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 1)
	expect_equal(pkg$base, "http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata")
	expect_equal(pkg$hash, "8eb868b9c31dee0d066f34adf0d82ef51dd02e40e3458abe8267324af5138819")
})
