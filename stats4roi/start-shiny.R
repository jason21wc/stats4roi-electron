# Diagnostic version to understand the environment

print("=== R Startup Diagnostics ===")
print(paste("Working directory:", getwd()))
print(paste("R_HOME:", Sys.getenv("R_HOME")))
print(paste("RHOME:", Sys.getenv("RHOME")))
print(paste("R_HOME_DIR:", Sys.getenv("R_HOME_DIR")))
print(paste("R_LIBS:", Sys.getenv("R_LIBS")))
print(paste("R_LIBS_USER:", Sys.getenv("R_LIBS_USER")))
print(paste("R_LIBS_SITE:", Sys.getenv("R_LIBS_SITE")))
print(paste("R_LIB_PATHS:", Sys.getenv("R_LIB_PATHS")))
print(paste("RE_SHINY_PATH:", Sys.getenv("RE_SHINY_PATH")))
print(paste("RE_SHINY_PORT:", Sys.getenv("RE_SHINY_PORT")))

# Try to use the environment variable first
env_lib <- Sys.getenv("R_LIB_PATHS")
if (env_lib != "") {
  print(paste("Setting library path from R_LIB_PATHS to:", env_lib))
  .libPaths(env_lib)
} else {
  print("R_LIB_PATHS is empty, trying current directory approach")
  lib_path <- file.path(getwd(), "r-mac", "library")
  print(paste("Trying library path:", lib_path))
  if (dir.exists(lib_path)) {
    .libPaths(lib_path)
  }
}

print(paste("Final library paths:", paste(.libPaths(), collapse = " | ")))

# List what's in the first library path
first_lib <- .libPaths()[1]
if (dir.exists(first_lib)) {
  packages_found <- list.dirs(first_lib, full.names = FALSE, recursive = FALSE)
  print(paste("Found", length(packages_found), "packages in library"))
  print(paste("First 10 packages:", paste(head(packages_found, 10), collapse = ", ")))
}

# Now try to load shiny
print("Attempting to load shiny...")
success <- require(shiny, quietly = FALSE)
if (!success) {
  stop("Could not load shiny package")
}

print("SUCCESS: Shiny loaded!")

# Get configuration
app_path <- Sys.getenv("RE_SHINY_PATH")
if (app_path == "") {
  app_path <- "shiny"
}

port <- as.integer(Sys.getenv("RE_SHINY_PORT"))
if (is.na(port)) {
  port <- 7777
}

print(paste("Starting app from:", app_path))
print(paste("On port:", port))

# Start the app
shiny::runApp(
  app_path,
  host = "127.0.0.1",
  launch.browser = FALSE,
  port = port
)
