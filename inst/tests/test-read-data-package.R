context("read_data_package")
test_that("local data package works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$resources), 6)
	expect_equal(pkg$base, "../extdata")
	expect_equal(pkg$hash, "293e1320fa4878286e696dd50646e6b27265bdbfa9f6ff6a33b22d570b6d80f1")
	})

test_that("remote data package works", {
	url <- "http://raw.github.com/QBRC/RODProt/758feb1d65527d2125758f0682aae69d9e3f4707/inst/extdata/datapackage.json"
	pkg <- read_data_package(url)
		
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$resources), 6)
	expect_equal(pkg$base, "http://raw.github.com/QBRC/RODProt/758feb1d65527d2125758f0682aae69d9e3f4707/inst/extdata")
	expect_equal(pkg$hash, "f47f8801d0369b28d004faa1741d02ca38be947da5ff18d4afad4c67d9665e29")
})
