context("cast_type")
test_that("basic classes work", {	
	expect_equal("integer", cast_type("integer"))
	expect_equal("character", cast_type("string"))
})

test_that("vectors work", {
	expect_equal(c("integer", "character"), cast_type(c("integer", "string")))
})
