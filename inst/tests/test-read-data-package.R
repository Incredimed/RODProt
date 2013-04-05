context("read_data_package")
test_that("local data package works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 2)
	expect_equal(pkg$base, "../extdata")
	})

test_that("remote data package works", {
	
	pkg <- read_data_package("http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 1)
	expect_equal(pkg$base, "http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata")
})
