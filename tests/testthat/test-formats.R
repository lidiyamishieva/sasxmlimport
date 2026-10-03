test_that("numeric SAS VALUE definitions are parsed correctly", {
  statement <-
    'value CL_271_ 1="Very dissatisfied" 2="Somewhat dissatisfied" 3="Somewhat satisfied" 4="Very satisfied"'
  
  result <- sasxmlimport:::.parse_value_statement(statement)
  
  expect_equal(
    result$format,
    rep("CL_271_", 4)
  )
  
  expect_equal(
    result$code,
    c("1", "2", "3", "4")
  )
  
  expect_equal(
    result$label,
    c(
      "Very dissatisfied",
      "Somewhat dissatisfied",
      "Somewhat satisfied",
      "Very satisfied"
    )
  )
})
