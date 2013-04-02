context("read_json_table")

test_that("local file JSON works", {
	fl <- tempfile()
		
	json <- '{"fields":[
			{
				"id":"A",
		"type":"integer"		
	},{
		"id":"B",
		"type":"string"		
	}],
		"data":[
	{"A": 4, "B": "test"},
	{"A": 5, "B": "another"},
	{"A": 6, "B": "final"}
		]}'
	
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
	json <- '{"fields":[
		{
			"id":"A",
			"type":"integer"		
		},{
			"id":"B",
			"type":"string"		
		}],
		"data":[
			{"A": 4, "B": "test"},
			{"A": 5, "B": "another"},
			{"A": 6, "B": "final"}
		]}'
	
	tab <- read_json_table(json)	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)	
})

test_that("labeled JSON works", {
	json <- '{"fields":[
		{
			"id":"A",
			"label":"Column A",
			"type":"integer"		
		},{
			"id":"B",
			"label":"Column B",
			"type":"string"		
		}],
		"data":[
			{"A": 4, "B": "test"},
			{"B": "another", "A": 5},
			{"A": 6, "B": "final"}
		]}'
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
		
})

test_that("reverse-ordered fields and data works", {
	json <- '{"data":[
	[4, "test"],
	[5, "another"],
	[6, "final"]
	],
	"fields":[
		{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]
	}'
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
	})


test_that("mixed-array JSON works", {
	json <- '{"fields":[
		{
			"id":"A",
			"label":"Column A",
			"type":"integer"		
		},{
			"id":"B",
			"label":"Column B",
			"type":"string"		
		}],
		"data":[
			[4, "test"],
			[5, "another"],
			[6, "final"]
		]}'
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
})

test_that("combined array and named JSON works", {
	json <- '{"fields":[
		{
			"id":"A",
			"label":"Column A",
			"type":"integer"		
		},{
			"id":"B",
			"label":"Column B",
			"type":"string"		
		}],
		"data":[
			{"A":4, "B":"test"},
			{"B":"another", "A":5},
			[6, "final"]
		]}'
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
	
})

test_that("separate schema with nested keys works", {
	schema <- '{"fields":[
		{
			"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]}'
	data <- '{"data":[
	[4, "test"],
	[5, "another"],
	[6, "final"]
	]}'
	
	tab <- read_json_table(data, schema)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
})

test_that("separate schema without nested keys works", {
	schema <- '[
		{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]'
	data <- '[
	[4, "test"],
	[5, "another"],
	[6, "final"]
	]'
	
	tab <- read_json_table(data, schema)	
	expected <- data.frame(Column.A=4:6, 
												 Column.B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
	})



test_that("invalid schema fails", {
	schema <- '{"a":[
		{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]}'
	data <- '[
	[4, "test"],
	[5, "another"],
	[6, "final"]
	]'
	
	expect_error(read_json_table(data, schema),
							 "named.*fields")
})

test_that("un-schema'd name column warns", {
	json <- '{"fields":[
		{
			"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}],
	"data":[
{"A":4, "C":"test"},
{"B":"another", "A":5}
	]}'
	
	expect_warning(tab <- read_json_table(json),
								 "ignored.*C")
	expected <- data.frame(Column.A=4:5, 
												 Column.B=c(NA, "another"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)	
})

test_that("invalid data fails separate", {
	schema <- '[
		{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]'
	data <- '{"test":[
	[4, "test"],
	[5, "another"],
	[6, "final"]
	]}'
		
	expect_error(read_json_table(data, schema),
							 ".*data.*named.*")
})

test_that("invalid data fails merged", {
	json <- '{"fields":[
{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}],
	"data":{"test":
	[4, "test"],
	[5, "another"],
	[6, "final"]
	]}}'
	
	expect_error(read_json_table(json),
							 "[Ee]rror.*fromJSON.*content")
})

test_that("mismatched data & schema fails", {
	schema <- '[
{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]'
	data <- '[
	[4, "test", 1],
	[5, "another", 2],
	[6, "final", 3]
	]'
	
	expect_error(read_json_table(data, schema),
							 ".*[Nn]umber.*columns.*match.*")
})

test_that("mixed-length data array fails", {
	schema <- '[
{
	"id":"A",
	"label":"Column A",
	"type":"integer"		
},{
	"id":"B",
	"label":"Column B",
	"type":"string"		
}]'
	data <- '[
	[4, "test"],
	[5, "another", 2],
	[6]
	]'
	
	expect_error(read_json_table(data, schema),
							 ".*same.*number.*columns.*")
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