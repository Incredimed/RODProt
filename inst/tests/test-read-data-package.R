context("read_data_package")
test_that("local data package works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_true(is.null(pkg$url))
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 2)
	expect_equal(pkg$base, "../extdata")
	expect_equal(pkg$hash, "f1ee19f9b54f5c312e6054d2bd0ea1cae2da2ed763c8a2ada91d17860ed7e9a3")
	})

test_that("remote data package works", {
	url <- "http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata/datapackage.json"
	pkg <- read_data_package(url)
	
	
	expect_equal(pkg$url, url)
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$files), 1)
	expect_equal(pkg$base, "http://raw.github.com/QBRC/RODProt/abbd449be3dc3d40042749f8f895c93b4d504f1b/inst/extdata")
	expect_equal(pkg$hash, "a6b0739e35a27476d27369da3a5f70e42087ed5e0994e9223cd0b9de3d1d1158")
})
