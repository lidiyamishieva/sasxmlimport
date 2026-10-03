.clean_sas_name <- function(x) {
  x <- sub("^_+", "", x)
  sub("_[0-9]+$", "", x)
}

.create_name_map <- function(tables) {
  original_tables <- names(tables)
  clean_tables <- .clean_sas_name(original_tables)
  
  if (anyDuplicated(clean_tables)) {
    duplicated_names <- unique(
      clean_tables[
        duplicated(clean_tables) |
          duplicated(clean_tables, fromLast = TRUE)
      ]
    )
    
    stop(
      "Cleaning table names would create duplicate name(s): ",
      paste(duplicated_names, collapse = ", "),
      ". Original names have been preserved."
    )
  }
  
  table_map <- data.frame(
    original = original_tables,
    clean = clean_tables,
    stringsAsFactors = FALSE
  )
  
  column_map <- do.call(
    rbind,
    lapply(seq_along(tables), function(i) {
      original_columns <- names(tables[[i]])
      clean_columns <- .clean_sas_name(original_columns)
      
      if (anyDuplicated(clean_columns)) {
        duplicated_names <- unique(
          clean_columns[
            duplicated(clean_columns) |
              duplicated(clean_columns, fromLast = TRUE)
          ]
        )
        
        stop(
          "Cleaning column names in table '",
          original_tables[i],
          "' would create duplicate name(s): ",
          paste(duplicated_names, collapse = ", "),
          ". Original names have been preserved."
        )
      }
      
      data.frame(
        table_original = original_tables[i],
        table_clean = clean_tables[i],
        column_original = original_columns,
        column_clean = clean_columns,
        stringsAsFactors = FALSE
      )
    })
  )
  
  rownames(column_map) <- NULL
  
  list(
    tables = table_map,
    columns = column_map
  )
}


.apply_clean_names <- function(tables, name_map) {
  out <- tables
  
  for (i in seq_along(out)) {
    table_name <- names(out)[i]
    
    map <- name_map$columns[
      name_map$columns$table_original == table_name,
      ,
      drop = FALSE
    ]
    
    idx <- match(names(out[[i]]), map$column_original)
    
    if (anyNA(idx)) {
      stop(
        "Could not match all columns while cleaning names in table '",
        table_name,
        "'."
      )
    }
    
    names(out[[i]]) <- map$column_clean[idx]
  }
  
  idx <- match(names(out), name_map$tables$original)
  
  if (anyNA(idx)) {
    stop("Could not match all table names while cleaning names.")
  }
  
  names(out) <- name_map$tables$clean[idx]
  
  out
}