# Custom package installation script for stats4ROI
print("Installing packages for stats4ROI...")

# Set CRAN repository
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Core packages that we know stats4ROI needs
packages_to_install <- c(
  "shiny",
  "tidyverse",
  "DT",
  "shinyWidgets",
  "datamods",
  "emmeans",
  "nlme",
  "svglite",
  "agop",
  "ggh4x",
  "bayestestR",
  "bayesboot",
  "car",
  "rhandsontable",
  "htmlwidgets",
  "devtools",
  "remotes"
)

# Install each package
for (pkg in packages_to_install) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    print(paste("Installing", pkg, "..."))
    install.packages(pkg, quiet = FALSE)
  } else {
    print(paste("Package", pkg, "already installed"))
  }
}

print("Base packages installed. Now installing GitHub packages...")

# Install GitHub packages
if (!require("lolcat", quietly = TRUE)) {
  print("Installing lolcat from GitHub...")
  remotes::install_github("burrm/lolcat", quiet = FALSE)
}

# propagate MUST be Steve's Shiny fork, not CRAN.
#
# Do NOT gate this on `!require("propagate")`. CRAN propagate frequently arrives
# as a transitive dependency, which makes require() succeed and silently skips
# the fork install — the runtime then ships CRAN propagate while this script
# reports success. That is exactly how v4.2.0 regressed. Gate on WHICH propagate
# is installed, not on whether one is.
propagate_is_fork <- function() {
  isTRUE(tryCatch(
    utils::packageDescription("propagate")$RemoteUsername == "ProfessorPeregrine",
    error = function(e) FALSE
  ))
}

if (!propagate_is_fork()) {
  print("Installing the ProfessorPeregrine fork of propagate from GitHub...")
  remotes::install_github("ProfessorPeregrine/propagate",
                          upgrade = "never", force = TRUE, quiet = FALSE)
  if (!propagate_is_fork()) {
    stop("propagate is still not the ProfessorPeregrine fork after install. ",
         "Scatterplot CI/PI rendering will be wrong. ",
         "Run ./ensure-propagate-fork.sh, which handles the FLIBS override the ",
         "bundled R needs.")
  }
} else {
  print("propagate is already the ProfessorPeregrine fork")
}

print("All packages installed successfully!")
