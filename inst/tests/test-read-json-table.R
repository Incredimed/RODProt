context("read_json_table")

test_that("local file JSON works", {
	fl <- tempfile()
		
	json <- "{\"fields\":[
			{
				\"id\":\"A\",
		\"type\":\"integer\"		
	},{
		\"id\":\"B\",
		\"type\":\"string\"		
	}],
		\"data\":[
	{\"A\": 4, \"B\": \"test\"},
	{\"A\": 5, \"B\": \"another\"},
	{\"A\": 6, \"B\": \"final\"}
		]}"
	
	fileCon <- file(fl)
	writeLines(json, con=fileCon)
	close(fileCon)	
	
	tab <- read_json_table(fl)	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)	
	
	unlink(fl)
})

test_that("label-less JSON works", {
	json <- "{\"fields\":[
		{
			\"id\":\"A\",
			\"type\":\"integer\"		
		},{
			\"id\":\"B\",
			\"type\":\"string\"		
		}],
		\"data\":[
			{\"A\": 4, \"B\": \"test\"},
			{\"A\": 5, \"B\": \"another\"},
			{\"A\": 6, \"B\": \"final\"}
		]}"
	
	tab <- read_json_table(json)	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)	
})

test_that("labeled JSON works", {
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
			{\"B\": \"another\", \"A\": 5},
			{\"A\": 6, \"B\": \"final\"}
		]}"
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
		
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
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
})

test_that("combined array and named JSON works", {
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
			{\"A\":4, \"B\":\"test\"},
			{\"B\":\"another\", \"A\":5},
			[6, \"final\"]
		]}"
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
	
})

test_that("separate schema works", {
	#TODO: 
	expect_equal(0, 1, info="Test not yet implemented.")
})

test_that("invalid JSON fails", {
	#TODO: 
	expect_equal(0, 1, info="Test not yet implemented.")
})

test_that("Schema-less JSON works", {
	#TODO: 
	expect_equal(0, 1, info="Test not yet implemented.")
})

test_that("Invalid types produce explicit warning", {
	#TODO: 
	expect_equal(0, 1, info="Test not yet implemented.")
})

test_that("data-less JSON produces empty data.frame", {
	#TODO: 
	expect_equal(0, 1, info="Test not yet implemented.")
})

test_that("remote URL works", {
	#TODO: 
	expect_equal(0, 1, info="Test not yet implemented.")
})