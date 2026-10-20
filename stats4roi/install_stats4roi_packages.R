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

# lolcat MUST be the pinned commit, not merely present.
#
# stats4ROI v4.36 calls functions added after lolcat's 2.0.2 version bump
# (power/sample.size.cor.pearson.r.onesample.exact), so neither presence nor the
# version number identifies a usable build. Gate on the installed commit SHA.
# Bump LOLCAT_SHA when Steve's upstream needs a newer lolcat.
LOLCAT_SHA <- "f0b8af6d3385b21c39fa9446bb241435a2abd31c"

lolcat_is_pinned <- function() {
  isTRUE(tryCatch(
    utils::packageDescription("lolcat")$RemoteSha == LOLCAT_SHA,
    error = function(e) FALSE
  ))
}

if (!lolcat_is_pinned()) {
  print(paste("Installing lolcat from GitHub at", substr(LOLCAT_SHA, 1, 7), "..."))
  remotes::install_github(paste0("burrm/lolcat@", LOLCAT_SHA),
                          upgrade = "never", force = TRUE, quiet = FALSE)
  if (!lolcat_is_pinned()) {
    stop("lolcat is not the pinned commit ", LOLCAT_SHA, " after install. ",
         "Sample Size/Power Pearson r calculations will fail.")
  }
} else {
  print("lolcat is already the pinned commit")
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
