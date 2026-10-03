.read_sas_formats <- function(format_file) {
  sas_lines <- readLines(
    format_file,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  sas_text <- paste(sas_lines, collapse = "\n")
  
  statements <- unlist(
    strsplit(sas_text, ";", fixed = TRUE),
    use.names = FALSE
  )
  
  statements <- trimws(statements)
  statements <- statements[nzchar(statements)]
  
  format_map <- .parse_format_assignments(statements)
  format_values <- .parse_value_definitions(statements)
  
  list(
    assignments = format_map,
    values = format_values
  )
}


.parse_format_assignments <- function(statements) {
  current_table <- NA_character_
  result <- list()
  k <- 0L
  
  for (statement in statements) {
    if (grepl("^data\\s+", statement, ignore.case = TRUE)) {
      current_table <- sub(
        "^data\\s+([^[:space:]]+).*$",
        "\\1",
        statement,
        ignore.case = TRUE
      )
    }
    
    if (grepl("^format\\s+", statement, ignore.case = TRUE)) {
      match_object <- regexec(
        "^format\\s+([^[:space:]]+)\\s+([^[:space:]]+)$",
        statement,
        ignore.case = TRUE
      )
      
      pieces <- regmatches(statement, match_object)[[1]]
      
      if (length(pieces) == 3L) {
        k <- k + 1L
        
        result[[k]] <- data.frame(
          table = current_table,
          variable = pieces[2],
          format = sub("\\.$", "", pieces[3]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(result) == 0L) {
    return(NULL)
  }
  
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}


.parse_value_statement <- function(statement) {
  format_name <- sub(
    "^value\\s+([^[:space:]]+).*$",
    "\\1",
    statement,
    ignore.case = TRUE
  )
  
  pair_pattern <-
    "(\"(?:\"\"|[^\"])*\"|[^[:space:]=]+)\\s*=\\s*\"((?:\"\"|[^\"])*)\""
  
  hits <- regmatches(
    statement,
    gregexpr(pair_pattern, statement, perl = TRUE)
  )[[1]]
  
  if (length(hits) == 0L) {
    return(NULL)
  }
  
  values <- lapply(hits, function(hit) {
    match_object <- regexec(pair_pattern, hit, perl = TRUE)
    pieces <- regmatches(hit, match_object)[[1]]
    
    code <- pieces[2]
    
    if (startsWith(code, '"') && endsWith(code, '"')) {
      code <- substring(code, 2L, nchar(code) - 1L)
    }
    
    code <- gsub('""', '"', code, fixed = TRUE)
    label <- gsub('""', '"', pieces[3], fixed = TRUE)
    
    data.frame(
      format = format_name,
      code = code,
      label = label,
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, values)
  rownames(out) <- NULL
  out
}


.parse_value_definitions <- function(statements) {
  value_statements <- statements[
    grepl("^value\\s+", statements, ignore.case = TRUE)
  ]
  
  if (length(value_statements) == 0L) {
    return(NULL)
  }
  
  values <- lapply(value_statements, .parse_value_statement)
  
  values <- values[
    !vapply(values, is.null, logical(1))
  ]
  
  if (length(values) == 0L) {
    return(NULL)
  }
  
  out <- do.call(rbind, values)
  rownames(out) <- NULL
  out
}


.apply_sas_formats <- function(
    typed_tables,
    format_map,
    format_values
) {
  display_tables <- typed_tables
  
  if (is.null(format_map) || is.null(format_values)) {
    return(
      list(
        data = display_tables,
        coverage = NULL
      )
    )
  }
  
  exists <- mapply(
    function(table, variable) {
      table %in% names(typed_tables) &&
        variable %in% names(typed_tables[[table]])
    },
    format_map$table,
    format_map$variable,
    USE.NAMES = FALSE
  )
  
  if (any(!exists)) {
    warning(
      sum(!exists),
      " FORMAT assignment(s) could not be matched ",
      "to imported variables.",
      call. = FALSE
    )
  }
  
  active_formats <- format_map[
    exists,
    ,
    drop = FALSE
  ]
  
  if (nrow(active_formats) == 0L) {
    return(
      list(
        data = display_tables,
        coverage = NULL
      )
    )
  }
  
  coverage <- do.call(
    rbind,
    lapply(seq_len(nrow(active_formats)), function(i) {
      table <- active_formats$table[i]
      variable <- active_formats$variable[i]
      format <- active_formats$format[i]
      
      x <- typed_tables[[table]][[variable]]
      
      observed <- unique(
        as.character(x[!is.na(x)])
      )
      
      allowed <- format_values$code[
        format_values$format == format
      ]
      
      data.frame(
        table = table,
        variable = variable,
        format = format,
        n_observed_codes = length(observed),
        n_unmapped_codes = length(setdiff(observed, allowed)),
        stringsAsFactors = FALSE
      )
    })
  )
  
  rownames(coverage) <- NULL
  
  for (i in seq_len(nrow(active_formats))) {
    table <- active_formats$table[i]
    variable <- active_formats$variable[i]
    format <- active_formats$format[i]
    
    lookup <- format_values[
      format_values$format == format,
      ,
      drop = FALSE
    ]
    
    if (nrow(lookup) == 0L) {
      next
    }
    
    label_lookup <- stats::setNames(
      lookup$label,
      lookup$code
    )
    
    x <- typed_tables[[table]][[variable]]
    
    y <- unname(
      label_lookup[as.character(x)]
    )
    
    missing_label <- !is.na(x) & is.na(y)
    y[missing_label] <- as.character(x[missing_label])
    y[is.na(x)] <- NA_character_
    
    display_tables[[table]][[variable]] <- y
  }
  
  list(
    data = display_tables,
    coverage = coverage
  )
}
