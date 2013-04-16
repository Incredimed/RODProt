context("read_data_package")
test_that("local data package works", {	
	pkg <- read_data_package("../extdata/datapackage.json")
	
	expect_equal(pkg$name, "sample-data")
	expect_equal(pkg$title, "Sample Data")
	expect_equal(length(pkg$licenses), 1)
	expect_equal(length(pkg$resources), 6)
	expect_equal(pkg$base, "../extdata")
	expect_equal(pkg$hash, "5f230e4fda0ced36889ac64e699680e9c24be127689582296dd0747f3f5ad016")
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
