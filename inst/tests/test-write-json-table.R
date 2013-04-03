context("write_json_table")

test_that("unnamed characeter data.frame can be written and read", {
  data <- data.frame(a=letters[1:3], 
                     b=LETTERS[2:4],
                     stringsAsFactors=FALSE)
  
  txt <- ""
  con <- textConnection("txt", "w", local=TRUE)  
  write_json_table(data, con)  
  close(con)
  
  parsed <- read_json_table(txt)  
  expect_equal(parsed, data)  
})

test_that("unnamed numeric data.frame can be written and read", {
  data <- data.frame(a=1:3, 
                     b=2:4,
                     stringsAsFactors=FALSE)
  
  txt <- ""
  con <- textConnection("txt", "w", local=TRUE)  
  write_json_table(data, con)  
  close(con)
  
  parsed <- read_json_table(txt)  
  expect_equal(parsed, data)  
})

test_that("unnamed mixed data.frame can be written and read", {
  data <- data.frame(a=LETTERS[1:3], 
                     b=2:4,
                     c=round(rnorm(3),5),
                     d=c(TRUE, FALSE, FALSE),                     
                     stringsAsFactors=FALSE)
  
  txt <- ""
  con <- textConnection("txt", "w", local=TRUE)  
  write_json_table(data, con)  
  close(con)
  
  #check that the elements aren't named
  expect_equal(names(fromJSON(txt)$data[[1]]), NULL)
    
  parsed <- read_json_table(txt)  
  expect_equal(parsed, data)  
})

test_that("named mixed data.frame can be written and read", {
	data <- data.frame(a=LETTERS[1:3], 
										 b=2:4,
										 c=round(rnorm(3),5),
										 d=c(TRUE, FALSE, FALSE),                     
										 stringsAsFactors=FALSE)
	
	txt <- ""
	con <- textConnection("txt", "w", local=TRUE)  
	write_json_table(data, con, named=TRUE)  
	close(con)
	
	#check that the elements are named
	expect_true(all(names(fromJSON(txt)$data[[1]]) %in% letters[1:4]))
	expect_equal(length(names(fromJSON(txt)$data[[1]])), 4)
	
	parsed <- read_json_table(txt)  
	expect_equal(parsed, data)  
})