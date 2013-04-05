context("read_data_package")
test_that("local data package works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 2)
	expect_equal(pkg$base, "../extdata")
	expect_equal(pkg$hash, "f5dd1c23c953fdf6f92b049342c0ddb516302417edb2c57e4f6147d6e37ccd69")
	})

test_that("remote data package works", {
	
	pkg <- read_data_package("http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 1)
	expect_equal(pkg$base, "http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata")
	expect_equal(pkg$hash, "8eb868b9c31dee0d066f34adf0d82ef51dd02e40e3458abe8267324af5138819")
})
