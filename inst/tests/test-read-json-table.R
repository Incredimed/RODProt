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

test_that("data-less JSON produces empty data.frame", {
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
	"data":[]}'
	
	tab <- read_json_table(json)	
	expected <- data.frame(Column.A=integer(), 
												 Column.B=character(), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
})

test_that("missing data produces empty data.frame", {
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
	
	tab <- read_json_table(schema=schema)	
	expected <- data.frame(Column.A=integer(), 
												 Column.B=character(), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)
	})

test_that("Schema-less array JSON works", {
	dataJSON <- '{
	"data":[
[4, "test"],
[5, "another"],
[6, "final"]
	]}'
	
	tab <- read_json_table(dataJSON)	
	expected <- data.frame(C1=4:6, 
												 C2=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	
	#order of columns is (theoretically) non-deterministic. Just check content, not identical.
	expect_true(all(colnames(tab) %in% c("C1", "C2")))
	expect_equal(nrow(tab), 3)
	expect_equal(ncol(tab), 2)
	
	#classes will not be guaranteed to be right with no schema.
	expect_equal(as.character(tab$C1), as.character(expected$C1))
	expect_equal(as.character(tab$C2), as.character(expected$C2))
})

test_that("Schema-less list JSON works", {
	dataJSON <- '{
	"data":[
{"A": 4, "B": "test"},
{"A": 5, "B": "another"},
{"A": 6, "B": "final"}
	]}'
	
	tab <- read_json_table(dataJSON)	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	
	#order of columns is (theoretically) non-deterministic. Just check content, not identical.
	expect_true(all(colnames(tab) %in% c("A", "B")))
	expect_equal(nrow(tab), 3)
	expect_equal(ncol(tab), 2)
	
	#classes will not be guaranteed to be right with no schema.
	expect_equal(as.character(tab$A), as.character(expected$A))
	expect_equal(as.character(tab$B), as.character(expected$B))	
	
	
	dataJSON <- '{
	"data":[
{"A": 4, "B": "test"},
{"C": 5, "B": "another"},
{"A": 6}
	]}'
	
	tab <- read_json_table(dataJSON)	
	expected <- data.frame(A=c(4,NA,6), 
												 B=c("test", "another", NA), 
												 C=c(NA, 5, NA),
												 stringsAsFactors=FALSE)
	
	#order of columns is (theoretically) non-deterministic. Just check content, not identical.
	expect_true(all(colnames(tab) %in% c("A", "B", "C")))
	expect_equal(nrow(tab), 3)
	expect_equal(ncol(tab), 3)
	
	#classes will not be guaranteed to be right with no schema.
	expect_equal(as.character(tab$A), as.character(expected$A))
	expect_equal(as.character(tab$B), as.character(expected$B))	
	expect_equal(as.character(tab$C), as.character(expected$C))	
})

test_that("Invalid types produces error", {
	json <- '{"fields":[
		{
			"id":"A",
	"type":"foobar"		
},{
	"id":"B",
	"type":"string"		
}],
	"data":[
{"A": 4, "B": "test"},
{"A": 5, "B": "another"},
{"A": 6, "B": "final"}
	]}'
	
	expect_error(read_json_table(json),
							 "foobar.*not supported.*overlook\\.types")
	
})


test_that("Overlook invalid type produces no issues", {
	json <- '{"fields":[
{
	"id":"A",
	"type":"foobar"		
},{
	"id":"B",
	"type":"string"		
}],
	"data":[
{"A": 4, "B": "test"},
{"A": 5, "B": "another"},
{"A": 6, "B": "final"}
	]}'
	
	tab <- read_json_table(json, overlook.types=TRUE)
	expected <- data.frame(A=as.character(4:6),
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)	
	})

test_that("remote URL works", {
  tab <- read_json_table("http://raw.github.com/QBRC/RODProt/63cd96f91ccdae6e8358393211ea3d88870c8eb0/data/mixed.json")
  expected <- data.frame(Column.A=4:6,
                         Column.B=c("test", "another", "final"), 
                         stringsAsFactors=FALSE)
  expect_identical(tab, expected)
})

test_that("separate remote URL works", {
  tab <- read_json_table(content="http://raw.github.com/QBRC/RODProt/cd95fe955ae3e97cab528540874361b08ebc2aa9/data/data.json",
                         schema="http://raw.github.com/QBRC/RODProt/cd95fe955ae3e97cab528540874361b08ebc2aa9/data/schema.json")
  expected <- data.frame(Column.A=4:6,
                         Column.B=c("test", "another", "final"), 
                         stringsAsFactors=FALSE)
  expect_identical(tab, expected)
})