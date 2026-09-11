#' Do spatial intersection while choosing columns to return
#'
#' This function does a spatial intersection between two vector datasets
#' A and B. It allows the users to choose whether to return the columns
#' from A, B or both. This function extends the code of ddbs_intersection
#' function from duckspatial (https://github.com/Cidree/duckspatial/blob/main/R/ddbs_ops_binary.R).

#' @param x is an input spatial data. Can be: a duckspatial_df object, an sf object,
#' a tbl_lazy from dplyr, or a character string naming a table/view in conn.
#' 
#' @param y is an input spatial data. Can be: a duckspatial_df object, an sf object,
#' a tbl_lazy from dplyr, or a character string naming a table/view in conn.
#' 
#' @param conn is a connection to a DuckDB database. If NULL, the function runs 
#' on a temporary DuckDB database.
#' 
#' @param conn_x is the connection to the DuckDB database holding the input x.
#' 
#' @param conn_y is the connection to the DuckDB database holding the input x.
#' 
#' @param name is the character string of length one specifiying the name of 
#' the table to be written in conn. If NULL (the default), the function returns 
#' the result as an sf object.
#' 
#' @param mode helps to control the return type. It is a character string of value
#' (1) "duckspatial"(default): Lazyspatial data frame backed bydbplyr/DuckDB; or 
#' (2) "sf": Eagerly collected sf object (uses memory).
#' Can also be replaced by a ddbs_options(mode = "...") or per-function via 
#' this argument. Per-function overrides global setting.
#' 
#' @param keep_cols is a character. It allows to choose which columns to keep 
#' in the resulting table: columns of "x", "y", or "both". Default is "x".
#' 
#' @param overwrite determines whether to overwrite the table is already existing.
#' It is a bolean: TRUE or FALSE (default). This argument is ignored when name is NULL.
#' 
#' @quiet is a logical value. If TRUE, suppresses any informational messages. 
#' Defaults to FALSE.
#' 
#' @return a vector with the fylke number and the fylke name
#' 
#' @export
#'
#' @examples

ddbs_intersection_tcd <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    keep_cols = "x",
    overwrite = FALSE,
    quiet = FALSE) {
  
  
  # 0. Validate inputs
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  
  
  # 1. Prepare inputs
  
  ## 1.1. Resolve conn_x/conn_y defaults from 'conn' for character inputs
  if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
  if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn
  
  ## 1.2. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn_x)
  y <- normalize_spatial_input(y, conn_y)
  
  ## 1.3. Pre-extract attributes
  crs_x    <- ddbs_crs(x, conn_x)
  crs_y    <- ddbs_crs(y, conn_y)
  sf_col_x <- attr(x, "sf_column")
  sf_col_y <- attr(y, "sf_column")
  mode     <- get_mode(mode, name)
  
  ## 1.3. Resolve spatial connections and handle imports
  resolve_res <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
  # NOTE: Inline connection resolution logic was replaced by resolve_spatial_connections()
  # helper (defined in db_utils_not_exported.R) to maintain consistency with ddbs_join
  # and other two-input spatial functions. See tests/testthat/test-resolve_connections.R
  # for regression tests covering cross-connection scenarios.
  target_conn <- resolve_res$conn
  x           <- resolve_res$x
  y           <- resolve_res$y
  
  ## 1.4. register cleanup of the connection
  if (any(is.null(conn_x), is.null(conn_y))) {
    on.exit(resolve_res$cleanup(), add = TRUE)   
  }
  
  ## 1.5. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)
  y_list <- get_query_list(y, target_conn)
  on.exit(y_list$cleanup(), add = TRUE)
  
  ## 1.6. Validate the CRS of x and y
  validate_xy_crs(
    crs_x = crs_x,
    crs_y = crs_y,
    conn = target_conn,
    x_list = x_list,
    y_list = y_list
  )
  
  
  # 2. Prepare the query
  
  ## 2.1. Get names of geometry columns (use saved sf_col_x/y from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
  assert_geometry_column(x_geom, x_list)
  assert_geometry_column(y_geom, y_list)
  
  ## 2.2. Build the base query
  st_function <- glue::glue("ST_Intersection(v1.{x_geom}, v2.{y_geom})")
  
  if(keep_cols == "x") {
  base.query <- glue::glue("
        SELECT 
            v1.* REPLACE({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM 
            {x_list$query_name} v1,
            {y_list$query_name} v2
        WHERE 
            ST_Intersects(v2.{y_geom}, v1.{x_geom});
    ")
  }else if(keep_cols == "y"){
    base.query <- glue::glue("
        SELECT 
            v2.* REPLACE({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM 
            {x_list$query_name} v1,
            {y_list$query_name} v2
        WHERE 
            ST_Intersects(v2.{y_geom}, v1.{x_geom});
    ")
  }else if(keep_cols == "both"){
    
    # Retrieve column names
    cols_x <- DBI::dbGetQuery(target_conn, glue::glue("DESCRIBE {x_list$query_name}"))$column_name
    cols_y <- DBI::dbGetQuery(target_conn, glue::glue("DESCRIBE {y_list$query_name}"))$column_name
    
    # Identify common names
    cols_x <- setdiff(cols_x, x_geom)
    cols_y <- setdiff(cols_y, y_geom)
    clashes <- base:::intersect(cols_x, cols_y)
    
    # Define base.query
    if(length(clashes) == 0){
      base.query <- glue::glue("
        SELECT 
            v1.* REPLACE({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom}),
            v2.* EXCLUDE ({y_geom})
        FROM 
            {x_list$query_name} v1,
            {y_list$query_name} v2
        WHERE 
            ST_Intersects(v2.{y_geom}, v1.{x_geom});
    ")
      
    }else{
      
      exclude_x <- paste(c(x_geom, clashes), collapse = ", ")
      exclude_y <- paste(c(y_geom, clashes), collapse = ", ")
      
      clash_x <- paste(glue::glue('v1.{clashes} AS "{clashes}.x"'), collapse = ", ")
      clash_y <- paste(glue::glue('v2.{clashes} AS "{clashes}.y"'), collapse = ", ")
      
      glue::glue(
        "v1.* EXCLUDE ({exclude_x}),
        v2.* EXCLUDE ({exclude_y}),
        {build_geom_query(st_function, name, crs_x, mode)} AS {x_geom}, {clash_x}, {clash_y}")
      
    }
    
    
  }else{
    message('keep_cols does not have the correct value. Choose from "x", "y", or "both"')
  }
  
  
  # 3. Table creation if name is provided, or 
  # create duckspatial_df or sf object if name is NULL
  if (!is.null(name)) {
    create_duckdb_table(
      conn      = target_conn,
      name      = name,
      query     = base.query,
      overwrite = overwrite,
      quiet     = quiet
    )
  } else {
    ddbs_handle_query(
      query  = base.query,
      conn   = target_conn,
      mode   = mode,
      crs    = crs_x,
      x_geom = x_geom
    )
  }
}






#' @rdname ddbs_binary_funs
#' @export
ddbs_difference <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
  
  
  # 0. Validate inputs
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  
  
  # 1. Prepare inputs
  
  ## 1.1. Resolve conn_x/conn_y defaults from 'conn' for character inputs
  if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
  if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn
  
  ## 1.2. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn_x)
  y <- normalize_spatial_input(y, conn_y)
  
  ## 1.3. Pre-extract attributes
  crs_x    <- ddbs_crs(x, conn_x)
  crs_y    <- ddbs_crs(y, conn_y)
  sf_col_x <- attr(x, "sf_column")
  sf_col_y <- attr(y, "sf_column")
  mode     <- get_mode(mode, name)
  
  ## 1.3. Resolve spatial connections and handle imports
  resolve_res <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
  # NOTE: Inline connection resolution logic was replaced by resolve_spatial_connections()
  # helper (defined in db_utils_not_exported.R) to maintain consistency with ddbs_join
  # and other two-input spatial functions. See tests/testthat/test-resolve_connections.R
  # for regression tests covering cross-connection scenarios.
  target_conn <- resolve_res$conn
  x           <- resolve_res$x
  y           <- resolve_res$y
  
  ## 1.4. register cleanup of the connection
  if (any(is.null(conn_x), is.null(conn_y))) {
    on.exit(resolve_res$cleanup(), add = TRUE)   
  }
  
  ## 1.5. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)
  y_list <- get_query_list(y, target_conn)
  on.exit(y_list$cleanup(), add = TRUE)
  
  ## 1.6. Validate the CRS of x and y
  validate_xy_crs(
    crs_x = crs_x,
    crs_y = crs_y,
    conn = target_conn,
    x_list = x_list,
    y_list = y_list
  )
  
  
  # 2. Prepare the query
  
  ## 2.1. Get names of geometry columns (use saved sf_col_x/y from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
  assert_geometry_column(x_geom, x_list)
  assert_geometry_column(y_geom, y_list)
  
  ## 2.2. Build base query
  st_function <- glue::glue("{x_geom}")
  base.query <- glue::glue("
        WITH diff_geom AS (
            SELECT 
                v1.* REPLACE (
                    ST_Difference(
                        ST_MakeValid(v1.{x_geom}),
                        ST_MakeValid(v2.{y_geom})
                    ) AS {x_geom}
                )
            FROM 
                {x_list$query_name} v1, 
                {y_list$query_name} v2
            WHERE NOT ST_IsEmpty(
                ST_Difference(
                    ST_MakeValid(v1.{x_geom}),
                    ST_MakeValid(v2.{y_geom})
                )
            )
        )
        SELECT 
            * REPLACE ({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM diff_geom;
    ")
  
  
  # 3. Table creation if name is provided, or 
  # create duckspatial_df or sf object if name is NULL
  if (!is.null(name)) {
    create_duckdb_table(
      conn      = target_conn,
      name      = name,
      query     = base.query,
      overwrite = overwrite,
      quiet     = quiet
    )
  } else {
    ddbs_handle_query(
      query  = base.query,
      conn   = target_conn,
      mode   = mode,
      crs    = crs_x,
      x_geom = x_geom
    )
  }
  
}