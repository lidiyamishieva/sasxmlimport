test_that("OpenClinica table and variable names can be cleaned", {
  path <- tempfile("sas_xml_clean_names_")
  dir.create(path)
  
  data_xml <- '
<SPROTIGR>
  <IG_TEST>
    <SubjectID>001</SubjectID>
    <I_TEST_DATE_1234>2026-09-16</I_TEST_DATE_1234>
    <I_TEST_VALUE_5678>3</I_TEST_VALUE_5678>
    <I_TEST_LONG_NAME>example</I_TEST_LONG_NAME>
  </IG_TEST>
</SPROTIGR>
'
  
  writeLines(
    data_xml,
    file.path(path, "SAS_DATA.xml")
  )
  
  map_xml <- '
<SXLEMAP version="1.2">
  <TABLE name="_TEST_9999">
    <TABLE-PATH>/SPROTIGR/IG_TEST</TABLE-PATH>

    <COLUMN Name="SubjectID">
      <PATH>/SPROTIGR/IG_TEST/SubjectID</PATH>
      <TYPE>character</TYPE>
      <DATATYPE>string</DATATYPE>
      <LENGTH>20</LENGTH>
    </COLUMN>

    <COLUMN Name="_TEST_DATE_1234">
      <PATH>/SPROTIGR/IG_TEST/I_TEST_DATE_1234</PATH>
      <TYPE>numeric</TYPE>
      <DATATYPE>date</DATATYPE>
      <LENGTH>8</LENGTH>
    </COLUMN>

    <COLUMN Name="_TEST_VALUE_5678">
      <PATH>/SPROTIGR/IG_TEST/I_TEST_VALUE_5678</PATH>
      <TYPE>numeric</TYPE>
      <DATATYPE>integer</DATATYPE>
      <LENGTH>8</LENGTH>
    </COLUMN>

    <COLUMN Name="_TEST_LONG_NAME">
      <PATH>/SPROTIGR/IG_TEST/I_TEST_LONG_NAME</PATH>
      <TYPE>character</TYPE>
      <DATATYPE>string</DATATYPE>
      <LENGTH>50</LENGTH>
    </COLUMN>
  </TABLE>
</SXLEMAP>
'
  
  writeLines(
    map_xml,
    file.path(path, "SAS_MAP.xml")
  )
  
  result <- import_sas_xml(
    path,
    apply_formats = FALSE,
    clean_names = TRUE,
    verbose = FALSE
  )
  
  expect_named(result$typed, "TEST")
  expect_named(result$display, "TEST")
  
  expect_equal(
    names(result$typed[["TEST"]]),
    c(
      "SubjectID",
      "TEST_DATE",
      "TEST_VALUE",
      "TEST_LONG_NAME"
    )
  )
  
  expect_true("TEST_DATE" %in% names(result$typed[["TEST"]]))
  expect_true("TEST_LONG_NAME" %in% names(result$typed[["TEST"]]))
  
  expect_type(result$typed[["TEST"]]$SubjectID, "character")
  expect_s3_class(result$typed[["TEST"]]$TEST_DATE, "Date")
  expect_type(result$typed[["TEST"]]$TEST_VALUE, "integer")
  
  expect_equal(result$name_map$tables$original, "_TEST_9999")
  expect_equal(result$name_map$tables$clean, "TEST")
  
  expect_equal(
    result$name_map$columns$column_original,
    c(
      "SubjectID",
      "_TEST_DATE_1234",
      "_TEST_VALUE_5678",
      "_TEST_LONG_NAME"
    )
  )
  
  expect_equal(
    result$name_map$columns$column_clean,
    c(
      "SubjectID",
      "TEST_DATE",
      "TEST_VALUE",
      "TEST_LONG_NAME"
    )
  )
  
  expect_equal(unique(result$metadata$table), "TEST")
  
  expect_equal(
    result$metadata$column,
    c(
      "SubjectID",
      "TEST_DATE",
      "TEST_VALUE",
      "TEST_LONG_NAME"
    )
  )
  
  expect_equal(result$checks$overview$table, "TEST")
  expect_equal(unique(result$checks$conversion$table), "TEST")
  
  expect_equal(
    result$checks$conversion$variable,
    c(
      "SubjectID",
      "TEST_DATE",
      "TEST_VALUE",
      "TEST_LONG_NAME"
    )
  )
})


test_that("name cleaning does not silently create duplicate table names", {
  tables <- list(
    `_TEST_1234` = data.frame(x = 1),
    `_TEST_5678` = data.frame(x = 2)
  )
  
  expect_error(
    sasxmlimport:::.create_name_map(tables),
    "duplicate"
  )
})
