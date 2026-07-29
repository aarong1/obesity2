library(duckdb)

# DuckDB Helper Functions ----
# Initialize DuckDB connection and create schema for organizing runs
initialize_past_populations_db <- function(db_path = "./past_populations_db/past_populations.duckdb", 
                                          schema_name = "population_runs") {
  # Ensure directory exists
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
    cat(sprintf("Created database directory: %s\n", db_dir))
  }
  
  # Shutdown any existing DuckDB instances to avoid read-only issues
  tryCatch({
    duckdb::duckdb_shutdown(duckdb::duckdb())
  }, error = function(e) {
    # Ignore errors if no instance exists
  })
  
  # Connect to DuckDB with explicit read_only = FALSE
  drv <- duckdb::duckdb()
  con <- dbConnect(drv, dbdir = db_path, read_only = FALSE)
  
  # Create schema if it doesn't exist
  tryCatch({
    dbExecute(con, sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema_name))
    cat(sprintf("Schema '%s' ready\n", schema_name))
  }, error = function(e) {
    warning(sprintf("Could not create schema: %s", e$message))
  })
  
  cat(sprintf("Connected to DuckDB at: %s (write mode)\n", db_path))
  
  return(con)
}

# Generate date-stamped table name for a specific run
generate_run_table_name <- function(run_number, timestamp = NULL, schema = "population_runs") {
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  table_name <- sprintf("%s.past_populations_%s_run%d", schema, timestamp, run_number)
  return(table_name)
}

# List all run tables in the database
list_run_tables <- function(con, schema = "population_runs", pattern = NULL) {
  # Get all tables in schema
  query <- sprintf("SELECT table_name FROM information_schema.tables WHERE table_schema = '%s' ORDER BY table_name DESC", schema)
  tables <- dbGetQuery(con, query)$table_name
  
  # Apply pattern filter if provided
  if (!is.null(pattern)) {
    tables <- grep(pattern, tables, value = TRUE)
  }
  
  return(tables)
}

# Parse table name to extract metadata
parse_table_name <- function(table_name) {
  # Extract timestamp and run number from table name
  # Format: past_populations_YYYYMMDD_HHMMSS_runN
  pattern <- "past_populations_(\\d{8}_\\d{6})_run(\\d+)"
  matches <- regmatches(table_name, regexec(pattern, table_name))
  
  if (length(matches[[1]]) == 3) {
    return(list(
      timestamp = matches[[1]][2],
      run = as.integer(matches[[1]][3]),
      full_name = table_name
    ))
  } else {
    return(NULL)
  }
}

# Check if table exists and get its schema
get_table_schema <- function(con, table_name = "past_populations") {
  tables <- dbListTables(con)
  
  if (table_name %in% tables) {
    # Get column names and types
    schema_query <- sprintf("PRAGMA table_info('%s')", table_name)
    schema <- dbGetQuery(con, schema_query)
    return(schema)
  } else {
    return(NULL)
  }
}

# Verify column consistency before writing
verify_column_consistency <- function(con, new_data, table_name = "past_populations") {
  existing_schema <- get_table_schema(con, table_name)
  
  if (is.null(existing_schema)) {
    # Table doesn't exist yet, so it's consistent
    cat(sprintf("Table '%s' does not exist yet. Will create on first write.\n", table_name))
    return(TRUE)
  }
  
  # Get column names from existing table
  existing_cols <- existing_schema$name
  new_cols <- names(new_data)
  
  # Check for missing columns in new data
  missing_cols <- setdiff(existing_cols, new_cols)
  # Check for extra columns in new data
  extra_cols <- setdiff(new_cols, existing_cols)
  
  if (length(missing_cols) > 0) {
    warning(sprintf("New data is missing columns: %s", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }
  
  if (length(extra_cols) > 0) {
    warning(sprintf("New data has extra columns: %s", paste(extra_cols, collapse = ", ")))
    return(FALSE)
  }
  
  cat("Column consistency check passed.\n")
  return(TRUE)
}

# Write population data to DuckDB (append to single table)
write_population_to_db <- function(con, 
                                   population_data, 
                                   table_name = "population_runs.past_populations") {
  # Parse schema and table
  parts <- strsplit(table_name, "\\.")[[1]]
  if (length(parts) == 2) {
    schema <- parts[1]
    table_simple <- parts[2]
    full_table_name <- table_name
  } else {
    schema <- NULL
    table_simple <- table_name
    full_table_name <- table_name
  }
  
  # Check if table exists
  if (!is.null(schema)) {
    check_query <- sprintf(
      "SELECT COUNT(*) as cnt FROM information_schema.tables WHERE table_schema = '%s' AND table_name = '%s'",
      schema, table_simple
    )
  } else {
    check_query <- sprintf(
      "SELECT COUNT(*) as cnt FROM information_schema.tables WHERE table_name = '%s'",
      table_simple
    )
  }
  
  table_exists <- dbGetQuery(con, check_query)$cnt > 0
  
  # Write data to DuckDB
  tryCatch({
    if (table_exists) {
      # Append to existing table using SQL INSERT
      # First write to a temporary table
      temp_table <- paste0("temp_", gsub("[^a-zA-Z0-9]", "_", as.character(Sys.time())))
      dbWriteTable(con, temp_table, population_data, temporary = TRUE, overwrite = TRUE)
      
      # Insert from temp table to target table
      insert_query <- sprintf("INSERT INTO %s SELECT * FROM %s", full_table_name, temp_table)
      dbExecute(con, insert_query)
      
      # Drop temp table
      dbExecute(con, sprintf("DROP TABLE %s", temp_table))
      
      cat(sprintf("Appended %d rows to table '%s'\n", nrow(population_data), full_table_name))
    } else {
      # Create new table
      if (!is.null(schema)) {
        dbWriteTable(con, c(schema, table_simple), population_data, overwrite = FALSE)
      } else {
        dbWriteTable(con, table_simple, population_data, overwrite = FALSE)
      }
      cat(sprintf("Created new table '%s' with %d rows\n", full_table_name, nrow(population_data)))
    }
  }, error = function(e) {
    stop(sprintf("Error writing to database table '%s': %s", full_table_name, e$message))
  })
  
  return(invisible(full_table_name))
}

# DuckDB Evaluation and Analysis Functions ----
# Get list of runs with metadata
get_available_runs <- function(con, schema = "population_runs") {
  tables <- list_run_tables(con, schema)
  
  runs_info <- lapply(tables, function(t) {
    info <- parse_table_name(t)
    if (!is.null(info)) {
      # Get row count for this table
      count_query <- sprintf("SELECT COUNT(*) as cnt FROM %s.%s", schema, t)
      row_count <- tryCatch(
        dbGetQuery(con, count_query)$cnt,
        error = function(e) NA
      )
      info$row_count <- row_count
      info$table_name <- sprintf("%s.%s", schema, t)
      return(as.data.frame(info))
    }
    return(NULL)
  })
  
  # Combine into data frame
  runs_df <- do.call(rbind, Filter(Negate(is.null), runs_info))
  return(runs_df)
}

# Query data from specific runs
query_runs <- function(con, run_numbers = NULL, timestamp_pattern = NULL, 
                      schema = "population_runs", sql_filter = NULL) {
  # Get available runs
  available_runs <- get_available_runs(con, schema)
  
  # Filter by run numbers if specified
  if (!is.null(run_numbers)) {
    available_runs <- available_runs[available_runs$run %in% run_numbers, ]
  }
  
  # Filter by timestamp pattern if specified
  if (!is.null(timestamp_pattern)) {
    available_runs <- available_runs[grepl(timestamp_pattern, available_runs$timestamp), ]
  }
  
  if (nrow(available_runs) == 0) {
    warning("No matching runs found")
    return(data.frame())
  }
  
  # Build UNION query across selected tables
  queries <- sapply(available_runs$table_name, function(tbl) {
    if (!is.null(sql_filter)) {
      sprintf("SELECT * FROM %s WHERE %s", tbl, sql_filter)
    } else {
      sprintf("SELECT * FROM %s", tbl)
    }
  })
  
  union_query <- paste(queries, collapse = " UNION ALL ")
  
  cat(sprintf("Querying %d run table(s)...\n", nrow(available_runs)))
  result <- dbGetQuery(con, union_query)
  
  return(result)
}
# Connect to existing database (read-only by default for safety)
connect_to_database <- function(db_path = "./past_populations.duckdb", read_only = TRUE) {
  if (!file.exists(db_path)) {
    stop(sprintf("Database file does not exist at: %s", db_path))
  }
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
  cat(sprintf("Connected to database at: %s (read_only=%s)\n", db_path, read_only))
  return(con)
}

# Get basic database statistics (works with single table or across schema)
get_db_summary <- function(con, table_name = NULL, schema = "population_runs") {
  if (is.null(table_name)) {
    # Query across all tables in schema
    tables <- list_run_tables(con, schema)
    if (length(tables) == 0) {
      stop("No run tables found in schema")
    }
    queries <- sapply(tables, function(t) sprintf("SELECT * FROM %s.%s", schema, t))
    union_query <- paste(queries, collapse = " UNION ALL ")
    
    query <- sprintf("
      SELECT 
        COUNT(*) as total_rows,
        COUNT(DISTINCT run) as num_runs,
        COUNT(DISTINCT year) as num_years,
        MIN(year) as min_year,
        MAX(year) as max_year
      FROM (%s) AS all_data
    ", union_query)
  } else {
    query <- sprintf("
      SELECT 
        COUNT(*) as total_rows,
        COUNT(DISTINCT run) as num_runs,
        COUNT(DISTINCT year) as num_years,
        MIN(year) as min_year,
        MAX(year) as max_year
      FROM %s
    ", table_name)
  }
  
  return(dbGetQuery(con, query))
}

# Get population counts by run and year
get_population_counts <- function(con, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      run,
      year,
      COUNT(*) as population_count
    FROM %s
    GROUP BY run, year
    ORDER BY run, year
  ", table_name)
  
  return(dbGetQuery(con, query))
}

# Get morbidity prevalence over time
get_morbidity_prevalence <- function(con, morbidity, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      run,
      year,
      COUNT(*) as total_population,
      SUM(CASE WHEN %s > 0 THEN 1 ELSE 0 END) as cases,
      ROUND(100.0 * SUM(CASE WHEN %s > 0 THEN 1 ELSE 0 END) / COUNT(*), 2) as prevalence_pct
    FROM %s
    GROUP BY run, year
    ORDER BY run, year
  ", morbidity, morbidity, table_name)
  
  return(dbGetQuery(con, query))
}

# Get morbidity incidence (new cases) for a specific time period
get_morbidity_incidence <- function(con, morbidity, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      run,
      year,
      COUNT(*) as total_population,
      SUM(CASE WHEN %s = year THEN 1 ELSE 0 END) as new_cases,
      ROUND(100.0 * SUM(CASE WHEN %s = year THEN 1 ELSE 0 END) / COUNT(*), 2) as incidence_pct
    FROM %s
    GROUP BY run, year
    ORDER BY run, year
  ", morbidity, morbidity, table_name)
  
  return(dbGetQuery(con, query))
}

# Get stratified analysis (by age, sex, deprivation, etc.)
get_stratified_prevalence <- function(con, morbidity, stratify_by, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      run,
      year,
      %s,
      COUNT(*) as total_population,
      SUM(CASE WHEN %s > 0 THEN 1 ELSE 0 END) as cases,
      ROUND(100.0 * SUM(CASE WHEN %s > 0 THEN 1 ELSE 0 END) / COUNT(*), 2) as prevalence_pct
    FROM %s
    GROUP BY run, year, %s
    ORDER BY run, year, %s
  ", stratify_by, morbidity, morbidity, table_name, stratify_by, stratify_by)
  
  return(dbGetQuery(con, query))
}

# Get demographic summary
get_demographic_summary <- function(con, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      run,
      year,
      sex,
      COUNT(*) as count,
      ROUND(AVG(age), 1) as mean_age,
      ROUND(MEDIAN(age), 1) as median_age,
      MIN(age) as min_age,
      MAX(age) as max_age
    FROM %s
    GROUP BY run, year, sex
    ORDER BY run, year, sex
  ", table_name)
  
  return(dbGetQuery(con, query))
}

# Extract data for specific run(s) and year(s)
extract_population_data <- function(con, runs = NULL, years = NULL, 
                                   table_name = "past_populations") {
  where_clauses <- c()
  
  if (!is.null(runs)) {
    where_clauses <- c(where_clauses, sprintf("run IN (%s)", paste(runs, collapse = ", ")))
  }
  if (!is.null(years)) {
    where_clauses <- c(where_clauses, sprintf("year IN (%s)", paste(years, collapse = ", ")))
  }
  
  where_clause <- if (length(where_clauses) > 0) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }
  
  query <- sprintf("SELECT * FROM %s %s", table_name, where_clause)
  
  return(dbGetQuery(con, query))
}

# Compare intervention vs non-intervention
compare_interventions <- function(con, morbidity, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      intervention,
      year,
      COUNT(*) as total_population,
      SUM(CASE WHEN %s > 0 THEN 1 ELSE 0 END) as cases,
      ROUND(100.0 * SUM(CASE WHEN %s > 0 THEN 1 ELSE 0 END) / COUNT(*), 2) as prevalence_pct
    FROM %s
    GROUP BY intervention, year
    ORDER BY intervention, year
  ", morbidity, morbidity, table_name)
  
  return(dbGetQuery(con, query))
}

# Get death statistics
get_death_statistics <- function(con, table_name = "past_populations") {
  query <- sprintf("
    SELECT 
      run,
      year,
      COUNT(*) as total_population,
      SUM(CASE WHEN death IS NOT NULL AND death != 0 THEN 1 ELSE 0 END) as deaths,
      ROUND(100.0 * SUM(CASE WHEN death IS NOT NULL AND death != 0 THEN 1 ELSE 0 END) / COUNT(*), 2) as mortality_rate_pct
    FROM %s
    GROUP BY run, year
    ORDER BY run, year
  ", table_name)
  
  return(dbGetQuery(con, query))
}

# Get multimorbidity analysis (people with multiple conditions)
get_multimorbidity_counts <- function(con, morbidities, table_name = "past_populations") {
  morbidity_sum <- paste(sprintf("(CASE WHEN %s > 0 THEN 1 ELSE 0 END)", morbidities), collapse = " + ")
  
  query <- sprintf("
    SELECT 
      run,
      year,
      (%s) as num_conditions,
      COUNT(*) as population_count,
      ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY run, year), 2) as percentage
    FROM %s
    GROUP BY run, year, num_conditions
    ORDER BY run, year, num_conditions
  ", morbidity_sum, table_name)
  
  return(dbGetQuery(con, query))
}

# Example usage:
# 
# === Writing Data (in main model run) ===
# db_info <- initialize_past_populations_db("./past_populations_db/past_populations.duckdb")
# con <- db_info$con
# timestamp <- db_info$timestamp
# 
# # Write each run to its own date-stamped table
# for (run in 1:10) {
#   table_name <- write_population_to_db(con, current_population, run_number = run, timestamp = timestamp)
#   cat(sprintf("Run %d written to: %s\n", run, table_name))
# }
# dbDisconnect(con, shutdown=TRUE)
#
# === Analysis Examples ===
# con <- connect_to_past_populations()
# 
# # List all available runs
# runs <- get_available_runs(con)
# print(runs)
# 
# # Query specific runs
# data_runs_1_to_3 <- query_runs(con, run_numbers = c(1, 2, 3))
# 
# # Query runs from a specific date
# todays_runs <- query_runs(con, timestamp_pattern = "20260110")
# 
# # Get summary across all runs
# summary <- get_db_summary(con, schema = "population_runs")
# 
# # Query specific table for a single run
# specific_table <- "population_runs.past_populations_20260110_143022_run1"
# stroke_prev <- get_morbidity_prevalence(con, "stroke", specific_table)
# 
# dbDisconnect(con, shutdown=TRUE)