context("read_json_table")

test_that("local file JSON works", {
	#TODO
})

test_that("named JSON works", {
	json <- "{\"fields\":[
		{
			\"id\":\"A\",
			\"label\":\"Column A\",
			\"type\":\"integer\"		
		},{
			\"id\":\"B\",
			\"label\":\"Column B\",
			\"type\":\"string\"		
		}],
		\"data\":[
			{\"A\": 4, \"B\": \"test\"},
			{\"A\": 5, \"B\": \"another\"},
			{\"A\": 6, \"B\": \"final\"}
		]}"
	
	tab <- read_json_table(json)	
	expect_equal(nrow(tab), 3)
	expect_equal(ncol(tab), 2)
	#expect_equal(colnames(tab), c("Column A", "Column B"))
	
	#check classes
	
})

test_that("mixed-array JSON works", {
	json <- "{\"fields\":[
		{
			\"id\":\"A\",
			\"label\":\"Column A\",
			\"type\":\"integer\"		
		},{
			\"id\":\"B\",
			\"label\":\"Column B\",
			\"type\":\"string\"		
		}],
		\"data\":[
			[4, \"test\"],
			[5, \"another\"],
			[6, \"final\"]
		]}"
})

test_that("separate schema works", {
	json <- "{\"fields\":[
		{
			\"id\":\"A\",
			\"label\":\"Column A\",
			\"type\":\"integer\"		
		},{
			\"id\":\"B\",
			\"label\":\"Column B\",
			\"type\":\"string\"		
		}],
		\"data\":{
			

		}"
})

test_that("invalid JSON fails", {
	
})

test_that("Schema-less JSON fails", {
	
})

test_that("data-less JSON works", {
	
})

test_that("remote URL works", {
	#TODO
})