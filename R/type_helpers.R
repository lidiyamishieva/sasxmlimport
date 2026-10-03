.parse_sas_date <- function(x, date_formats) {
  x <- trimws(x)
  x[x == ""] <- NA_character_
  
  result <- as.Date(rep(NA_character_, length(x)))
  remaining <- which(!is.na(x))
  
  for (format in date_formats) {
    if (length(remaining) == 0L) {
      break
    }
    
    parsed <- suppressWarnings(
      as.Date(x[remaining], format = format)
    )
    
    ok <- !is.na(parsed)
    result[remaining[ok]] <- parsed[ok]
    remaining <- remaining[!ok]
  }
  
  result
}


.convert_table_types <- function(
    df,
    table_name,
    metadata,
    date_formats
) {
  meta <- metadata[
    metadata$table == table_name,
    ,
    drop = FALSE
  ]
  
  idx <- match(names(df), meta$column)
  
  if (anyNA(idx)) {
    missing_variables <- names(df)[is.na(idx)]
    
    stop(
      "The following variable(s) in table '",
      table_name,
      "' were not found in the XMLMap: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  datatypes <- tolower(meta$datatype[idx])
  out <- df
  
  for (i in seq_along(out)) {
    datatype <- datatypes[i]
    
    if (is.na(datatype) || !nzchar(datatype)) {
      next
    }
    
    if (datatype == "string") {
      out[[i]] <- as.character(out[[i]])
      
    } else if (datatype == "integer") {
      out[[i]] <- suppressWarnings(
        as.integer(out[[i]])
      )
      
    } else if (datatype == "double") {
      out[[i]] <- suppressWarnings(
        as.numeric(out[[i]])
      )
      
    } else if (datatype == "date") {
      out[[i]] <- .parse_sas_date(
        out[[i]],
        date_formats = date_formats
      )
    }
  }
  
  out
}


.check_type_conversion <- function(
    raw_tables,
    typed_tables,
    metadata
) {
  checks <- lapply(
    names(raw_tables),
    function(table_name) {
      raw <- raw_tables[[table_name]]
      converted <- typed_tables[[table_name]]
      
      meta <- metadata[
        metadata$table == table_name,
        ,
        drop = FALSE
      ]
      
      idx <- match(names(raw), meta$column)
      
      data.frame(
        table = rep(table_name, length(raw)),
        variable = names(raw),
        datatype = meta$datatype[idx],
        newly_NA = vapply(
          seq_along(raw),
          function(i) {
            sum(
              !is.na(raw[[i]]) &
                is.na(converted[[i]])
            )
          },
          integer(1)
        ),
        stringsAsFactors = FALSE
      )
    }
  )
  
  check <- do.call(rbind, checks)
  rownames(check) <- NULL
  
  problems <- check[
    !is.na(check$datatype) &
      tolower(check$datatype) != "string" &
      check$newly_NA > 0L,
    ,
    drop = FALSE
  ]
  
  list(
    all = check,
    problems = problems
  )
}