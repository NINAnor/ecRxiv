# All unction are taken from duckspatial package GitHub repo #############
# These are all the functions needed for ddbs_intersection_uraq() to run #



########################################################################
############################utils__assert_fun###########################
##https://github.com/Cidree/duckspatial/blob/main/R/utils_assert_fun.R##
########################################################################

assert_logic <- function(arg, ref = "quiet") { # nocov start
  
  if (!is.logical(arg)) {
    cli::cli_abort(
      "{.arg {ref}} must be either TRUE or FALSE.",
      .frame = parent.frame()
    )
  }
} # nocov end


assert_xy <- function(xy, ref = "x") { # nocov start
  
  valid_types <- inherits(xy, "sf") || 
    inherits(xy, "duckspatial_df") ||
    inherits(xy, "tbl_sql") ||
    inherits(xy, "tbl_lazy") ||
    is.character(xy)
  
  if (!valid_types) {
    cli::cli_abort(
      "{.arg {ref}} must be an sf object, duckspatial_df, tbl_lazy, or a table name string.",
      .frame = parent.frame()
    )
  }
} # nocov end

assert_name <- function(name = parent.frame()$name, ref = "name") { # nocov start
  
  if (!is.null(name) && (!is.character(name) || length(name) != 1)) {
    cli::cli_abort(
      "{.arg {ref}} must be a single character string or NULL.",
      .frame = parent.frame()
    )
  }
} # nocov end


assert_geometry_column <- function(geom, name_list) { # nocov start
  if (length(geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{name_list$query_name}>.")
} # nocov end


##########################################################################
############################utils_not_exported############################
##https://github.com/Cidree/duckspatial/blob/main/R/utils_not_exported.R##
##########################################################################

normalize_spatial_input <- function(x, conn = NULL, geom_col = NULL) {
  # 1. sf: pass through
  if (inherits(x, "sf")) return(x)
  
  # 2. duckspatial_df: already normalized
  if (inherits(x, "duckspatial_df")) return(x)
  
  # 3. tbl_duckdb_connection: coerce to duckspatial_df
  if (inherits(x, "tbl_duckdb_connection")) {
    return(as_duckspatial_df(x, geom_col = geom_col))
  }
  
  # 4. character: verify table/view exists
  if (is.character(x)) {
    if (is.null(conn)) {
      cli::cli_abort("{.arg conn} required when using character table names.")
    }
    if (!table_exists(conn, x) & !arrow_view_exists(conn, x)) {
      cli::cli_abort("Table or view {.val {x}} does not exist in connection.")
    }
    return(x)
  }
  
  # Unsupported type - let downstream handle/error
  x
}


get_mode <- function(mode, name) { # nocov start
  
  ## If name is not NULL, the geospatial operations will create a new table,
  ## so we are interested in the duckspatial version (i.e. not add ST_AsWKB())
  ## This is the same as ignoring the option
  if (!is.null(name)) {
    return("duckspatial")
  } 
  
  ## Get the current option
  if (is.null(mode)) {
    mode <- getOption("duckspatial.mode", "duckspatial")
  }
  
  return(mode)
  
} # nocov end


resolve_spatial_connections <- function(
    x, 
    y, 
    conn = NULL, 
    conn_x = NULL, 
    conn_y = NULL, 
    quiet = FALSE) {
  
  cleanup_funs <- list()
  add_cleanup <- function(fn) {
    cleanup_funs <<- c(cleanup_funs, list(fn))
  }
  
  # 1. Resolve source connections
  # If not provided, try to extract from objects
  # Note: Character inputs will return NULL from get_conn_from_input
  source_conn_x <- conn_x %||% get_conn_from_input(x)
  source_conn_y <- conn_y %||% get_conn_from_input(y)
  source_conn_ds <- attr(x, "source_conn")
  
  # 2. Determine target connection
  # Priority: explicit conn > conn_x > conn_y > default
  target_conn <- if (!is.null(conn)) {
    conn
  } else if (!is.null(source_conn_x)) {
    source_conn_x
  } else if (!is.null(source_conn_y)) {
    source_conn_y
  } else if (!is.null(source_conn_ds)) {
    source_conn_ds
  } else {
    ddbs_default_conn()
  }
  
  
  # 2.1 Validate target connection
  if (!DBI::dbIsValid(target_conn)) {
    cli::cli_abort("Target connection is not valid. Please provide a valid DuckDB connection.")
  }
  
  # 2.2 Warn if conn_x and conn_y differ but no explicit conn was provided
  if (is.null(conn) && 
      !is.null(source_conn_x) && 
      !is.null(source_conn_y) && 
      !identical(source_conn_x, source_conn_y)) {
    cli::cli_warn(c(
      "{.arg x} and {.arg y} come from different DuckDB connections.",
      "i" = "Using {.arg x}'s connection as the target. Provide {.arg conn} to override."
    ))
  }
  
  # 3. Handle imports if source connections differ from target
  
  # Check x
  # We only import x if it HAS a source connection that is different from target
  if (!is.null(source_conn_x) && !identical(target_conn, source_conn_x)) {
    # Need to import x
    cli::cli_warn(c(
      "{.arg x} and the target connection are different.",
      "i" = "Importing {.arg x} to the target connection.",
      "i" = "This may require materializing data."
    ))
    
    x_to_import <- x
    if (is.character(x)) {
      x_to_import <- tryCatch({
        tbl_obj <- dplyr::tbl(source_conn_x, x)
        suppressWarnings(as_duckspatial_df(tbl_obj))
      }, error = function(e) {
        tryCatch(dplyr::tbl(source_conn_x, x), error = function(ex) x)
      })
    }
    
    res <- import_view_to_connection(target_conn, source_conn_x, x_to_import, quiet = quiet)
    x <- res$name
    
    add_cleanup(function() {
      tryCatch(DBI::dbExecute(target_conn, glue::glue("DROP VIEW IF EXISTS {res$name}")), error = function(e) NULL)
      if (is.function(res$cleanup)) tryCatch(res$cleanup(), error = function(e) NULL)
    })
  }
  
  # Check y
  # We only import y if it HAS a source connection that is different from target
  if (!is.null(source_conn_y) && !identical(target_conn, source_conn_y)) {
    # Need to import y
    cli::cli_warn(c(
      "{.arg y} and the target connection are different.",
      "i" = "Importing {.arg y} to the target connection.",
      "i" = "This may require materializing data."
    ))
    
    y_to_import <- y
    if (is.character(y)) {
      y_to_import <- tryCatch({
        tbl_obj <- dplyr::tbl(source_conn_y, y)
        suppressWarnings(as_duckspatial_df(tbl_obj))
      }, error = function(e) {
        tryCatch(dplyr::tbl(source_conn_y, y), error = function(ex) y)
      })
    }
    
    res <- import_view_to_connection(target_conn, source_conn_y, y_to_import, quiet = quiet)
    y <- res$name
    
    add_cleanup(function() {
      tryCatch(DBI::dbExecute(target_conn, glue::glue("DROP VIEW IF EXISTS {res$name}")), error = function(e) NULL)
      if (is.function(res$cleanup)) tryCatch(res$cleanup(), error = function(e) NULL)
    })
  }
  
  list(
    conn = target_conn,
    x = x,
    y = y,
    cleanup = function() {
      for (fn in cleanup_funs) fn()
    }
  )
}


get_query_list <- function(x, conn) {
  
  if (inherits(x, "sf")) {
    temp_view_name <- ddbs_temp_view_name()
    duckspatial::ddbs_write_table(conn = conn, data = x, name = temp_view_name,
                                  quiet = TRUE, temp_view = TRUE)
    x_list <- get_query_name(temp_view_name)
    x_list$cleanup <- function() {
      tryCatch(DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")), error = function(e) NULL)
      tryCatch(duckdb::duckdb_unregister_arrow(conn, temp_view_name), error = function(e) NULL)
    }
    x_list$owned <- FALSE   # created here, caller should not clean up
    return(x_list)
    
  } else if (inherits(x, "duckspatial_df")) {
    source_table <- attr(x, "source_table")
    if (!is.null(source_table)) {
      remote_name_result <- tryCatch(dbplyr::remote_name(x), error = function(e) NULL)
      if (!is.null(remote_name_result) &&
          !inherits(remote_name_result, "sql") &&
          as.character(remote_name_result) == source_table) {
        result <- get_query_name(source_table)
        result$cleanup <- function() NULL
        result$owned <- TRUE
        return(result)
      }
    }
    ## Test duckdb 1.5
    x_list <- get_query_name(source_table)
    if (!is.null(x_list$table_name)) {
      x_list$cleanup <- function() NULL
      x_list$owned <- TRUE
      return(x_list)
    }
    ## Modified by dplyr verbs: render to a new temp view
    temp_view_name <- ddbs_temp_view_name()
    query_sql <- dbplyr::sql_render(x, con = conn)
    DBI::dbExecute(conn, glue::glue(
      "CREATE OR REPLACE TEMPORARY VIEW {temp_view_name} AS {query_sql}"
    ))
    x_list <- get_query_name(temp_view_name)
    x_list$cleanup <- function() {
      tryCatch(DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")), error = function(e) NULL)
    }
    x_list$owned <- TRUE
    
    return(x_list)
    
  } else if (inherits(x, "tbl_lazy")) {
    temp_view_name <- ddbs_temp_view_name()
    query_sql <- dbplyr::sql_render(x)
    DBI::dbExecute(conn, glue::glue(
      "CREATE OR REPLACE TEMPORARY VIEW {temp_view_name} AS {query_sql}"
    ))
    x_list <- get_query_name(temp_view_name)
    x_list$cleanup <- function() {
      tryCatch(DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")), error = function(e) NULL)
    }
    x_list$owned <- TRUE
    return(x_list)
    
  } else if (inherits(x, "data.frame")) {
    temp_view_name <- ddbs_temp_view_name()
    duckdb::duckdb_register(conn, temp_view_name, x)
    x_list <- get_query_name(temp_view_name)
    x_list$cleanup <- function() {
      tryCatch(DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")), error = function(e) NULL)
      tryCatch(duckdb::duckdb_unregister_arrow(conn, temp_view_name), error = function(e) NULL)
    }
    x_list$owned <- TRUE
    return(x_list)
    
  } else {
    ## Character table name: pre-existing, never clean up
    x_list <- get_query_name(x)
    x_list$cleanup <- function() NULL
    x_list$owned <- TRUE
    return(x_list)
  }
}

get_query_name <- function(name) {  # nocov start
  if (length(name) == 2) {
    table_name <- name[2]
    schema_name <- name[1]
    query_name <- paste0(name, collapse = ".")
  } else {
    table_name   <- name
    schema_name <- "main"
    query_name <- name
  }
  list(
    table_name = table_name,
    schema_name = schema_name,
    query_name = query_name
  )
} # nocov end

validate_xy_crs <- function(
    crs_x,
    crs_y,
    conn,
    x_list,
    y_list
) { # nocov start
  if (!is.null(crs_x) && !is.null(crs_y)) {
    if (!crs_equal(crs_x, crs_y)) {
      cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
    }
  } else {
    assert_crs(conn, x_list$query_name, y_list$query_name)
  }
} # nocov end

crs_equal <- function(crs1, crs2) {  # nocov start
  if (is.null(crs1) || is.null(crs2)) return(FALSE)
  isTRUE(sf::st_crs(crs1) == sf::st_crs(crs2))
}  # nocov end

build_geom_query <- function(fun, name, crs, mode) { # nocov start
  ## Get mode
  if (is.null(name) && mode == "sf") {
    ## If not creating a table, fallback to BLOB
    glue::glue("ST_AsWKB({fun})")
  } else {
    ## When creating a table in a connection, we preserve the CRS
    ## in the geometry column
    geom_field <- duckdb_geometry_type(conn = NULL, crs)
    glue::glue("{fun}::{geom_field}")
  }
} # nocov end

create_duckdb_table <- function(
    conn,
    name,
    query,
    overwrite,
    quiet
) { # nocov start
  ## Convenient names of table and/or schema.table
  name_list <- get_query_name(name)
  
  ## Overwrite handling
  overwrite_table(name_list$query_name, conn, quiet, overwrite)
  
  ## Create and execute the query
  tmp.query <- glue::glue("
      CREATE TABLE {name_list$query_name} AS
      {query}
  ")
  DBI::dbExecute(conn, tmp.query)
  feedback_query(quiet)
  return(invisible(TRUE))
} # nocov end


overwrite_table <- function(x, conn, quiet, overwrite) { # nocov start
  if (overwrite) {
    DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {x};"))
    if (isFALSE(quiet)) cli::cli_alert_info("Table <{x}> dropped")
  }
} # nocov end

feedback_query <- function(quiet) { # nocov start
  if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
} # nocov end


ddbs_handle_query <- function(
    query, 
    conn, 
    mode = NULL, 
    crs = NULL,
    x_geom = "geometry",
    fun_group = 1,
    units = NULL
) { # nocov start
  
  # First, handle simple data frames
  crs_is_na <- is.null(crs) || (inherits(crs, "crs") && is.na(crs)) || (is.atomic(crs) && all(is.na(crs)))
  if (crs_is_na && length(x_geom) == 0) {
    
    ## Create the table
    view_name <- ddbs_temp_table_name()
    DBI::dbExecute(
      conn, 
      glue::glue("CREATE TEMP TABLE {view_name} AS {query};")
    )
    
    ## Return a lazy table
    return(dplyr::tbl(conn, view_name))
    
  }
  
  # Resolve mode type: parameter > global option > default
  if (is.null(mode)) {
    mode <- getOption("duckspatial.mode", "duckspatial")
  }
  
  # Validate mode type
  valid_modes <- c("duckspatial", "sf")
  if (!mode %in% valid_modes) {
    cli::cli_abort(
      "{.arg mode} must be one of {.val {valid_modes}}, not {.val {mode}}."
    )
  }
  
  # Handle based on mode type
  if (mode == "sf") {
    
    ## Get the query as a data frame
    data_tbl <- DBI::dbGetQuery(conn, query)
    
    ## Manage sf output depending on the function group
    ## - Group 1: functions that return a normal {sf} object (most of the funs)
    ## - Group 2: functions that return a vector (units or not units)
    if (fun_group == 1) {
      
      ## Convert to sf object
      data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
        x_geom     = x_geom
      )
      return(data_sf)
      
    } else if (fun_group == 2) {
      
      ## Return units/non-units vector
      if (is.null(units)) {
        return(data_tbl[, 1])
      } else {
        return(units::as_units(data_tbl[, 1], units))
      }
      
    }
    
  } else {
    # mode == "duckspatial"
    # Create a view name and the query
    view_name <- ddbs_temp_table_name()
    query <- glue::glue("
      CREATE TEMP TABLE {view_name} AS
      {query};
    ")
    # on.exit(DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {view_name}")))
    
    # Create the view
    DBI::dbExecute(conn, query)
    
    # Open lazily as duckspatial_df
    lazy_tbl <- duckdb::tbl_function(conn, view_name)
    
    result <- new_duckspatial_df(
      lazy_tbl, 
      crs = crs, 
      geom_col = x_geom, 
      source_table = view_name,
      source_conn = conn
    )
    
    return(result)
  }
  # nocov end
}

ddbs_temp_table_name <- function() { # nocov start
  paste0("temp_table_", gsub("-", "_", uuid::UUIDgenerate()))
} # nocov end

ddbs_checkpoint_if_possible <- function(conn) {
  if (!DBI::dbIsValid(conn)) {
    return(invisible(FALSE))
  }
  
  ok <- tryCatch({
    DBI::dbExecute(conn, "FORCE CHECKPOINT")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  invisible(ok)
}



#######################################################################
############################crs_persistence############################
##https://github.com/Cidree/duckspatial/blob/main/R/crs_persistence.R##
#######################################################################

duckdb_geometry_type <- function(conn, x) {
  lit <- crs_to_duckdb_literal(x)
  if (identical(lit$kind, "none")) {
    return("GEOMETRY")
  }
  
  paste0("GEOMETRY(", ddbs_quote_sql_string(conn, lit$literal), ")")
}

crs_to_duckdb_literal <- function(x) {
  if (is.null(x)) {
    return(list(literal = NA_character_, kind = "none"))
  }
  
  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    return(list(literal = NA_character_, kind = "none"))
  }
  
  parsed <- sf::st_crs(crs, parameters = TRUE)
  srid <- parsed$srid
  if (!is.null(srid) && length(srid) > 0 && !is.na(srid)) {
    return(list(literal = as.character(srid), kind = "authority"))
  }
  
  wkt <- crs$wkt
  if (is.null(wkt) || length(wkt) == 0 || is.na(wkt) || !nzchar(wkt)) {
    return(list(literal = NA_character_, kind = "none"))
  }
  
  list(literal = wkt, kind = "wkt")
}

ddbs_quote_sql_string <- function(conn, x) {
  if (!is.null(conn)) {
    return(as.character(DBI::dbQuoteString(conn, x)))
  }
  
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}


######################################################################
############################duckspatial_df############################
##https://github.com/Cidree/duckspatial/blob/main/R/duckspatial_df.R##
######################################################################



new_duckspatial_df <- function(
    x, 
    crs = NULL, 
    geom_col = NULL, 
    source_table = NULL,
    source_conn = NULL,
    create_view = FALSE
) {
  # Avoid double wrapping
  if (is_duckspatial_df(x)) return(x)
  
  # This will manage dplyr methods
  # Maybe move to duckspatial_df.tbl_duckdb_connection in the future
  if (inherits(x, "tbl_sql") && is.null(source_table) && !is.null(source_conn)) {
    
    # Here we won't have a source table, so we will need to create it
    if (create_view) {
      which <- "VIEW"
      source_table <- ddbs_temp_view_name()
    } else {
      which <- "TABLE"
      source_table <- ddbs_temp_table_name()
    }
    
    # Use sql_render to extract the query
    inner_query <- dbplyr::sql_render(x)
    
    # Create the table that will be returned as source_table
    # This executes the dplyr verb
    DBI::dbExecute(
      source_conn,
      glue::glue("
        CREATE OR REPLACE TEMP {which} {source_table} AS
        ({inner_query});"
      )
    )
    
    # Handle as a lazy duckdb table in the next step
    x <- dplyr::tbl(source_conn, source_table)
  }
  
  if (!inherits(x, "tbl_sql")) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_sql} (lazy DuckDB table). Use {.fn as_duckspatial_df} for other objects.")
  }
  
  # If geometry column is not provided, use geom by default
  geom_col <- geom_col %||% "geom"
  
  # Prepend our class
  structure(
    x,
    class = c("duckspatial_df", class(x)),
    sf_column = geom_col, # Keeping attribute name as sf_column for compatibility
    crs = if (inherits(crs, "crs")) crs else sf::st_crs(crs),
    source_table = source_table,
    source_conn = source_conn
  )
}
