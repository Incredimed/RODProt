context("cast_type")
test_that("basic classes work", {	
	expect_equal(list("integer"), cast_type("integer"))
	expect_equal(list("character"), cast_type("string"))
})

test_that("vectors work", {
	expect_equal(list("integer", "character"), cast_type(c("integer", "string")))
})

test_that("invalid produces null", {
	expect_equal(list(NULL), cast_type("fdsf"))
})