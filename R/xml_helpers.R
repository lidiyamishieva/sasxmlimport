
.get_xml_attribute <- function(node, attribute) {
  attrs <- xml2::xml_attrs(node)
  
  if (length(attrs) == 0L) {
    return(NA_character_)
  }
  
  idx <- match(
    tolower(attribute),
    tolower(names(attrs))
  )
  
  if (is.na(idx)) {
    return(NA_character_)
  }
  
  unname(attrs[[idx]])
}


.get_xml_value <- function(node, element) {
  x <- xml2::xml_find_first(
    node,
    paste0("./", element)
  )
  
  if (inherits(x, "xml_missing")) {
    return(NA_character_)
  }
  
  trimws(xml2::xml_text(x))
}


.get_table_name <- function(table_node) {
  name <- .get_xml_attribute(
    table_node,
    "name"
  )
  
  if (is.na(name) || !nzchar(name)) {
    stop("A TABLE definition has no name attribute.")
  }
  
  name
}


.get_column_name <- function(column_node) {
  name <- .get_xml_attribute(
    column_node,
    "name"
  )
  
  if (!is.na(name) && nzchar(name)) {
    return(name)
  }
  
  path <- .get_xml_value(
    column_node,
    "PATH"
  )
  
  if (is.na(path) || !nzchar(path)) {
    stop("A COLUMN has neither a name attribute nor a usable PATH.")
  }
  
  sub("^.*[/@]", "", path)
}


.read_xml_table <- function(
    table_node,
    data_xml
) {
  table_path <- .get_xml_value(
    table_node,
    "TABLE-PATH"
  )
  
  if (is.na(table_path) || !nzchar(table_path)) {
    stop("A TABLE definition has no usable TABLE-PATH.")
  }
  
  rows <- xml2::xml_find_all(
    data_xml,
    table_path
  )
  
  columns <- xml2::xml_find_all(
    table_node,
    "./COLUMN"
  )
  
  if (length(columns) == 0L) {
    return(data.frame())
  }
  
  column_names <- vapply(
    columns,
    .get_column_name,
    character(1)
  )
  
  column_paths <- vapply(
    columns,
    .get_xml_value,
    character(1),
    element = "PATH"
  )
  
  relative_paths <- vapply(
    column_paths,
    function(path) {
      prefix <- paste0(table_path, "/")
      
      if (!is.na(path) && startsWith(path, prefix)) {
        paste0(
          "./",
          substring(path, nchar(prefix) + 1L)
        )
      } else {
        path
      }
    },
    character(1)
  )
  
  out <- lapply(
    relative_paths,
    function(path) {
      if (is.na(path) || !nzchar(path)) {
        return(rep(NA_character_, length(rows)))
      }
      
      nodes <- xml2::xml_find_first(
        rows,
        path
      )
      
      xml2::xml_text(
        nodes,
        trim = TRUE
      )
    }
  )
  
  names(out) <- column_names
  
  out <- as.data.frame(
    out,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  out[out == ""] <- NA
  
  out
}


.extract_xml_metadata <- function(tables) {
  metadata <- lapply(
    tables,
    function(table_node) {
      columns <- xml2::xml_find_all(
        table_node,
        "./COLUMN"
      )
      
      if (length(columns) == 0L) {
        return(NULL)
      }
      
      data.frame(
        table = rep(
          .get_table_name(table_node),
          length(columns)
        ),
        column = vapply(
          columns,
          .get_column_name,
          character(1)
        ),
        type = vapply(
          columns,
          .get_xml_value,
          character(1),
          element = "TYPE"
        ),
        datatype = vapply(
          columns,
          .get_xml_value,
          character(1),
          element = "DATATYPE"
        ),
        length = vapply(
          columns,
          .get_xml_value,
          character(1),
          element = "LENGTH"
        ),
        stringsAsFactors = FALSE
      )
    }
  )
  
  metadata <- metadata[
    !vapply(metadata, is.null, logical(1))
  ]
  
  if (length(metadata) == 0L) {
    return(
      data.frame(
        table = character(),
        column = character(),
        type = character(),
        datatype = character(),
        length = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  result <- do.call(
    rbind,
    metadata
  )
  
  rownames(result) <- NULL
  
  result
}