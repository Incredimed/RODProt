context("read_json_table")

test_that("local file works", {
	fl <- tempfile()
		
	csv <- '4,"test"
5,"another"
6,"final"'
	
  schema <- '{"fields":[
    {
			"id":"A",
			"type":"integer"		
		},{
			"id":"B",
			"type":"string"		
		}]}'
  
	fileCon <- file(fl)
	writeLines(csv, con=fileCon)
	close(fileCon)	
	
	tab <- read_schemaed_csv(fl, schema=schema)	
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
		}]}'
	
	csv <- '4,"test"
5,"another"
6,"final"'
	
	tab <- read_schemaed_csv(content=csv, schema=json)	
	expected <- data.frame(A=4:6, 
												 B=c("test", "another", "final"), 
												 stringsAsFactors=FALSE)
	expect_identical(tab, expected)	
})


test_that("invalid schema fails", {
  json <- '{"a":[
  	{
			"id":"A",
			"type":"integer"		
		},{
			"id":"B",
			"type":"string"		
		}]}'
  
  csv <- '4,"test"
5,"another"
6,"final"'
	
	expect_error(read_schemaed_csv(content=csv, schema=json),
							 "named.*fields")
})

