#' Programmable pivot with tidyverse
#'
#' Build pivoted summaries programmatically: arbitrary groupings, multiple
#' measures, multiple aggregations, and optional pivot-wider.
#'
#' @param data A data.frame or tibble
#' @param groups Character vector of grouping columns (0..n)
#' @param values Character vector of measure columns to summarise (0..n)
#' @param funs Either a named list of functions or a character vector of
#'   built-in names: c("sum","mean","median","min","max","sd","var",
#'   "count","n_distinct"). You can also pass custom lambdas, e.g.
#'   list(p90 = ~quantile(.x, 0.9, na.rm = TRUE)).
#' @param value_funs Optional named list specifying functions per column, e.g.
#'   list(mpg = c("mean", "sd"), hp = "sum"). Takes precedence over values/funs
#'   when provided. Each list element should be a column name with a vector of
#'   function names to apply to that column.
#' @param wide_by Optional single grouping column name to spread across columns
#' @param na_rm Logical, whether to remove NAs for numeric functions
#' @return A data.frame/tibble of aggregated results
#' @examples
#' # Basic: cyl x gear, mean/sd of mpg & hp, plus a row count
#' pivot_agg(mtcars,
#'           groups = c("cyl","gear"),
#'           values = c("mpg","hp"),
#'           funs   = c("mean","sd","count"))
#'
#' # Pivot wider across 'gear' to create columns per gear
#' pivot_agg(mtcars,
#'           groups = c("cyl","gear"),
#'           values = "mpg",
#'           funs   = c("mean","count"),
#'           wide_by = "gear")
#'
#' # Custom quantile and IQR
#' pivot_agg(mtcars,
#'           groups = "cyl",
#'           values = "qsec",
#'           funs   = list(p90 = ~quantile(.x, 0.9, na.rm = TRUE), iqr = IQR))
#'
#' # Column-specific functions: mean & sd for mpg, sum for hp, count for gear
#' pivot_agg(mtcars,
#'           groups = "cyl",
#'           value_funs = list(mpg = c("mean", "sd"), hp = "sum", gear = "count"))
#'
#' # No groups: dataset-level summaries
#' pivot_agg(mtcars, groups = character(), values = c("mpg","hp"), funs = c("mean","sd"))
#'
pivot_agg <- function(data,
                      groups = character(),
                      values = character(),
                      funs   = c("sum"),
                      value_funs = NULL,
                      wide_by = NULL,
                      na_rm = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(all(groups %in% names(data)))
  
  # Handle new value_funs parameter - takes precedence over values/funs
  if (!is.null(value_funs)) {
    stopifnot(is.list(value_funs))
    stopifnot(all(names(value_funs) %in% names(data)))
    # Convert value_funs to values and funs for backward compatibility
    values <- names(value_funs)
    # Create a mapping of column to functions for later use
    col_fun_map <- value_funs
  } else {
    stopifnot(all(values %in% names(data)))
    col_fun_map <- NULL
  }

  # Map character function names to actual functions
  map_fun <- function(f) {
    if (is.function(f)) return(f)
    switch(
      f,
      sum         = function(x) sum(x, na.rm = na_rm),
      mean        = function(x) mean(x, na.rm = na_rm),
      median      = function(x) stats::median(x, na.rm = na_rm),
      min         = function(x) min(x, na.rm = na_rm),
      max         = function(x) max(x, na.rm = na_rm),
      sd          = function(x) stats::sd(x, na.rm = na_rm),
      var         = function(x) stats::var(x, na_rm = na_rm),
      count       = function(x) dplyr::n(),      # ignores x; row count
      n_distinct  = function(x) dplyr::n_distinct(x),
      stop("Unsupported function name: ", f)
    )
  }

  # Handle column-specific functions vs. applying all functions to all columns
  if (!is.null(col_fun_map)) {
    # Column-specific function mapping
    df <- data
    if (length(groups)) df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(groups)))
    
    # Build summarise expressions for each column-function combination
    summary_list <- list()
    want_count <- FALSE
    
    for (col_name in names(col_fun_map)) {
      col_funs <- col_fun_map[[col_name]]
      for (fun_name in col_funs) {
        if (fun_name == "count") {
          want_count <- TRUE
        } else {
          mapped_fun <- map_fun(fun_name)
          result_name <- paste0(fun_name, "_", col_name)
          summary_list[[result_name]] <- substitute(FUN(COL), 
                                                   list(FUN = mapped_fun, COL = as.name(col_name)))
        }
      }
    }
    
    if (length(summary_list) > 0) {
      df <- dplyr::summarise(df, !!!summary_list, .groups = "drop")
    } else if (length(groups)) {
      df <- dplyr::summarise(df, .groups = "drop")
    }
  } else {
    # Original behavior: apply all functions to all values
    fun_list <- if (is.list(funs)) funs else rlang::set_names(lapply(funs, map_fun), funs)
    
    # Separate value-based funs from count-like
    value_fun_list <- fun_list[setdiff(names(fun_list), c("count"))]
    want_count <- "count" %in% names(fun_list)

    df <- data
    if (length(groups)) df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(groups)))

    # Apply value-based summaries if values were provided
    if (length(values) && length(value_fun_list)) {
      df <- dplyr::summarise(
        df,
        dplyr::across(dplyr::all_of(values),
                      .fns = value_fun_list,
                      .names = "{.fn}_{.col}"),
        .groups = "drop"
      )
    } else if (length(groups)) {
      # If no `values`, but we grouped, drop the grouping to keep unique rows
      df <- dplyr::summarise(df, .groups = "drop")
    }
  }

  # Add a count column if requested
  if (want_count) {
    # Recompute by groups to get n() per group
    if (length(groups)) {
      counts <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
        dplyr::summarise(count_rows = dplyr::n(), .groups = "drop")
      df <- dplyr::left_join(df, counts, by = groups)
    } else {
      df$count_rows <- nrow(data)
    }
  }

  # Optional wide reshape on a chosen group dimension
  if (!is.null(wide_by)) {
    stopifnot(wide_by %in% groups)
    id_cols <- setdiff(groups, wide_by)
    df <- tidyr::pivot_wider(
      df,
      names_from = dplyr::all_of(wide_by),
      values_from = setdiff(names(df), groups),
      names_sep = " = "
    )
    # Reorder with id cols first
    df <- df[, c(id_cols, setdiff(names(df), id_cols)), drop = FALSE]
  }

  df
}

#' Fast pivot using data.table (for larger data)
#'
#' @param data A data.frame
#' @param groups Character vector of grouping columns
#' @param values Character vector of measure columns
#' @param funs Character vector of function names (sum, mean, ...), or actual
#'   function names available in data.table j (sum, mean, median, sd, var, min, max,
#'   uniqueN, length). "count" maps to length.
#' @return data.table/data.frame of results
#' @examples
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   dt_pivot(mtcars, groups = c("cyl","gear"), values = c("mpg","hp"), funs = c("mean","sd","count"))
#' }
#'
#' # NOTE: dt_pivot is minimalist; prefer pivot_agg for custom lambdas & wide reshaping
#'
dt_pivot <- function(data, groups = character(), values = character(), funs = c("sum")) {
  requireNamespace("data.table", quietly = TRUE)
  DT <- data.table::as.data.table(data)

  # Map user-friendly names
  fun_map <- function(nm) switch(nm,
    count = "length",
    n_distinct = "uniqueN",
    nm
  )
  funs2 <- vapply(funs, fun_map, character(1))

  by_expr <- if (length(groups)) groups else NULL

  # Build j-expression list, naming as fn_col
  j_list <- list()
  for (v in values) {
    for (fn in funs2) {
      # expression: fn(v)
      j_list[[paste0(fn, "_", v)]] <- substitute(FN(V), list(FN = as.name(fn), V = as.name(v)))
    }
  }
  # If only count requested and no values, count rows per group
  if (!length(values) && any(funs2 == "length")) {
    j_list[["count_rows"]] <- quote(length(.I))
  }

  res <- DT[, j_list, by = by_expr]
  data.table::setDF(res)
  res
}
