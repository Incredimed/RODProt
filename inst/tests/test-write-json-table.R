context("read_json_table")

test_that("characeter data.frame can be written and read", {
  data <- data.frame(a=letters[1:3], 
                     b=LETTERS[2:4],
                     stringsAsFactors=FALSE)
  
  txt <- ""
  con <- textConnection("txt", "w")  
  write_json_table(data, con)  
  close(con)
  
  parsed <- read_json_table(txt)  
  expect_equal(parsed, data)  
})

test_that("numeric data.frame can be written and read", {
  data <- data.frame(a=1:3, 
                     b=2:4,
                     stringsAsFactors=FALSE)
  
  txt <- ""
  con <- textConnection("txt", "w")  
  write_json_table(data, con)  
  close(con)
  
  parsed <- read_json_table(txt)  
  expect_equal(parsed, data)  
})

test_that("mixed data.frame can be written and read", {
  data <- data.frame(a=LETTERS[1:3], 
                     b=2:4,
                     c=rnorm(3),
                     d=c(TRUE, FALSE, FALSE),                     
                     stringsAsFactors=FALSE)
  
  txt <- ""
  con <- textConnection("txt", "w")  
  write_json_table(data, con)  
  close(con)
  
  parsed <- read_json_table(txt)  
  expect_equal(parsed, data)  
})