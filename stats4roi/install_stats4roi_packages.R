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

if (!require("propagate", quietly = TRUE)) {
  print("Installing propagate from GitHub...")  
  remotes::install_github("ProfessorPeregrine/propagate", quiet = FALSE)
}

print("All packages installed successfully!")
