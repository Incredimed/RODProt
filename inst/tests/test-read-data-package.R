context("read_data_package")
test_that("read data package works", {
	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 1)
	})

