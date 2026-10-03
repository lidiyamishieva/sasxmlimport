test_that("SAS XML export is imported correctly", {
  path <- tempfile("sas_xml_test_")
  dir.create(path)
  
  data_xml <- '
<SPROTIGR>
  <IG_TEST>
    <SubjectID>001</SubjectID>
    <I_DATE>2026-09-16</I_DATE>
    <I_COUNT>3</I_COUNT>
    <I_VALUE>1.5</I_VALUE>
    <I_YN>j</I_YN>
  </IG_TEST>

  <IG_TEST>
    <SubjectID>002</SubjectID>
    <I_DATE>2026-09-17</I_DATE>
    <I_COUNT>4</I_COUNT>
    <I_VALUE>2.5</I_VALUE>
    <I_YN>n</I_YN>
  </IG_TEST>
</SPROTIGR>
'
  
  writeLines(
    data_xml,
    file.path(path, "SAS_DATA.xml")
  )
  
  map_xml <- '
<SXLEMAP version="1.2">
  <TABLE name="_TEST">
    <TABLE-PATH>/SPROTIGR/IG_TEST</TABLE-PATH>

    <COLUMN Name="SubjectID">
      <PATH>/SPROTIGR/IG_TEST/SubjectID</PATH>
      <TYPE>character</TYPE>
      <DATATYPE>string</DATATYPE>
      <LENGTH>20</LENGTH>
    </COLUMN>

    <COLUMN Name="DATE">
      <PATH>/SPROTIGR/IG_TEST/I_DATE</PATH>
      <TYPE>numeric</TYPE>
      <DATATYPE>date</DATATYPE>
      <LENGTH>8</LENGTH>
    </COLUMN>

    <COLUMN Name="COUNT">
      <PATH>/SPROTIGR/IG_TEST/I_COUNT</PATH>
      <TYPE>numeric</TYPE>
      <DATATYPE>integer</DATATYPE>
      <LENGTH>8</LENGTH>
    </COLUMN>

    <COLUMN Name="VALUE">
      <PATH>/SPROTIGR/IG_TEST/I_VALUE</PATH>
      <TYPE>numeric</TYPE>
      <DATATYPE>double</DATATYPE>
      <LENGTH>8</LENGTH>
    </COLUMN>

    <COLUMN Name="YN">
      <PATH>/SPROTIGR/IG_TEST/I_YN</PATH>
      <TYPE>character</TYPE>
      <DATATYPE>string</DATATYPE>
      <LENGTH>1</LENGTH>
    </COLUMN>
  </TABLE>
</SXLEMAP>
'
  
  writeLines(
    map_xml,
    file.path(path, "SAS_MAP.xml")
  )
  
  format_sas <- '
proc format;

value $CL_YN_
  "j" = "yes"
  "n" = "no"
;

run;

data _TEST;
set _TEST;

format YN $CL_YN_.;

run;
'
  
  writeLines(
    format_sas,
    file.path(path, "SAS_FORMAT.sas")
  )
  
  result <- import_sas_xml(
    path,
    verbose = FALSE
  )
  
  expect_named(
    result,
    c(
      "typed",
      "display",
      "metadata",
      "name_map",
      "formats",
      "checks"
    )
  )
  
  expect_true("_TEST" %in% names(result$typed))
  expect_equal(nrow(result$typed[["_TEST"]]), 2L)
  expect_equal(ncol(result$typed[["_TEST"]]), 5L)
  
  expect_type(result$typed[["_TEST"]]$SubjectID, "character")
  expect_s3_class(result$typed[["_TEST"]]$DATE, "Date")
  expect_type(result$typed[["_TEST"]]$COUNT, "integer")
  expect_type(result$typed[["_TEST"]]$VALUE, "double")
  expect_type(result$typed[["_TEST"]]$YN, "character")
  
  expect_equal(
    result$typed[["_TEST"]]$YN,
    c("j", "n")
  )
  
  expect_equal(
    result$display[["_TEST"]]$YN,
    c("yes", "no")
  )
  
  expect_equal(nrow(result$metadata), 5L)
  
  expect_equal(
    result$metadata$column,
    c("SubjectID", "DATE", "COUNT", "VALUE", "YN")
  )
  
  expect_equal(
    result$metadata$datatype,
    c("string", "date", "integer", "double", "string")
  )
  
  expect_true(is.list(result$name_map))
  expect_named(result$name_map, c("tables", "columns"))
  expect_equal(result$name_map$tables$original, "_TEST")
  expect_equal(result$name_map$tables$clean, "TEST")
  
  expect_equal(
    result$name_map$columns$column_original,
    c("SubjectID", "DATE", "COUNT", "VALUE", "YN")
  )
  
  expect_equal(
    result$name_map$columns$column_clean,
    c("SubjectID", "DATE", "COUNT", "VALUE", "YN")
  )
  
  expect_equal(
    unname(result$checks$summary["n_type_conversion_problems"]),
    0
  )
  
  expect_equal(
    unname(result$checks$summary["n_unmapped_format_variables"]),
    0
  )
})


test_that("SAS XML export can be imported without a format file", {
  path <- tempfile("sas_xml_no_format_")
  dir.create(path)
  
  data_xml <- '
<SPROTIGR>
  <IG_TEST>
    <SubjectID>001</SubjectID>
  </IG_TEST>
</SPROTIGR>
'
  
  writeLines(
    data_xml,
    file.path(path, "SAS_DATA.xml")
  )
  
  map_xml <- '
<SXLEMAP version="1.2">
  <TABLE name="_TEST">
    <TABLE-PATH>/SPROTIGR/IG_TEST</TABLE-PATH>

    <COLUMN Name="SubjectID">
      <PATH>/SPROTIGR/IG_TEST/SubjectID</PATH>
      <TYPE>character</TYPE>
      <DATATYPE>string</DATATYPE>
      <LENGTH>20</LENGTH>
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
    verbose = FALSE
  )
  
  expect_equal(result$typed, result$display)
  expect_null(result$formats$assignments)
  expect_null(result$formats$values)
  expect_equal(result$typed[["_TEST"]]$SubjectID, "001")
})
