# =============================================================================
# check_metadata_stats.R
#
# Validates MARCH2025_OBJECTIVES_20042026.xlsx against the summary statistics
# documented in the metadata file (MARCH2025_OBJECTIVES_metadata.xlsx).
#
# Checks performed for each column:
#   - Row count / total records
#   - Non-null count and % populated (vs. expected)
#   - Unique value count (vs. expected)
#   - Min / Max for numeric columns
#   - Controlled vocabulary membership for categorical columns
#
# Usage:
#   Rscript check_metadata_stats.R
#   (or source() from RStudio)
#
# Dependencies: readxl, dplyr
# Install if needed:
#   install.packages(c("readxl", "dplyr"))
# =============================================================================

library(readxl)
library(dplyr)

# --------------------------------------------------------------------------- #
# 0. Configuration
# --------------------------------------------------------------------------- #

DATA_FILE     <- "MARCH2025_OBJECTIVES_20042026.xlsx"
DATA_SHEET    <- "MARCH2025_OBJECTIVES"
EXPECTED_ROWS <- 979L

# --------------------------------------------------------------------------- #
# 1. Load data
# --------------------------------------------------------------------------- #

cat("Loading data from:", DATA_FILE, "\n\n")
df <- read_excel(DATA_FILE, sheet = DATA_SHEET)

# --------------------------------------------------------------------------- #
# 2. Helper functions
# --------------------------------------------------------------------------- #

PASS <- "\u2713 PASS"
FAIL <- "\u2717 FAIL"

check <- function(label, condition, detail = "") {
  status <- if (isTRUE(condition)) PASS else FAIL
  msg <- sprintf("  [%s] %s", status, label)
  if (nchar(detail) > 0) msg <- paste0(msg, " -- ", detail)
  cat(msg, "\n")
  invisible(isTRUE(condition))
}

n_nonnull   <- function(x) sum(!is.na(x))
n_unique    <- function(x) length(unique(x[!is.na(x)]))
pct         <- function(n, total) round(100 * n / total, 1)

section <- function(col_name) {
  cat(sprintf("\n-- %s %s\n", col_name, strrep("-", max(0, 50 - nchar(col_name)))))
}

# --------------------------------------------------------------------------- #
# 3. Global checks
# --------------------------------------------------------------------------- #

cat("=============================================================\n")
cat(" MARCH2025_OBJECTIVES Dataset — Metadata Validation Report\n")
cat("=============================================================\n\n")

cat("GLOBAL\n")
actual_rows <- nrow(df)
check("Row count == 979",
      actual_rows == EXPECTED_ROWS,
      sprintf("found %d", actual_rows))

actual_cols <- ncol(df)
check("Column count == 28",
      actual_cols == 28L,
      sprintf("found %d", actual_cols))

# --------------------------------------------------------------------------- #
# 4. Per-column checks
# --------------------------------------------------------------------------- #

# --- polyID_unique -----------------------------------------------------------
section("polyID_unique")
x <- df$polyID_unique
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("979 unique values",            n_unique(x)  == 979,  sprintf("%d unique",   n_unique(x)))
check("Min == 1",                     min(x, na.rm=TRUE) == 1,  sprintf("min = %g", min(x, na.rm=TRUE)))
check("Max == 1541",                  max(x, na.rm=TRUE) == 1541, sprintf("max = %g", max(x, na.rm=TRUE)))

# --- combined_polyID_og ------------------------------------------------------
section("combined_polyID_og")
x <- df$combined_polyID_og
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("965 unique values",            n_unique(x)  == 965,  sprintf("%d unique",   n_unique(x)))
check("Min == 16",                    min(x, na.rm=TRUE) == 16,   sprintf("min = %g", min(x, na.rm=TRUE)))
check("Max == 22147",                 max(x, na.rm=TRUE) == 22147, sprintf("max = %g", max(x, na.rm=TRUE)))

# --- combined_trtYear --------------------------------------------------------
section("combined_trtYear")
x <- df$combined_trtYear
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("28 unique values",             n_unique(x)  == 28,   sprintf("%d unique",   n_unique(x)))
check("Min == 1991",                  min(x, na.rm=TRUE) == 1991, sprintf("min = %g", min(x, na.rm=TRUE)))
check("Max == 2018",                  max(x, na.rm=TRUE) == 2018, sprintf("max = %g", max(x, na.rm=TRUE)))

# --- combined_post_fr --------------------------------------------------------
section("combined_post_fr")
x <- df$combined_post_fr
valid_vals <- c("Y", "N")
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("2 unique values",              n_unique(x)  == 2,    sprintf("%d unique",   n_unique(x)))
check("Only values: Y, N",
      all(x[!is.na(x)] %in% valid_vals),
      sprintf("found: %s", paste(sort(unique(x[!is.na(x)])), collapse=", ")))

# --- combined_trtID ----------------------------------------------------------
section("combined_trtID")
x <- df$combined_trtID
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("976 unique values",            n_unique(x)  == 976,  sprintf("%d unique",   n_unique(x)))

# --- combined_ActnDsc --------------------------------------------------------
section("combined_ActnDsc")
x <- df$combined_ActnDsc
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("165 unique values",            n_unique(x)  == 165,  sprintf("%d unique",   n_unique(x)))

# --- combined_data_source ----------------------------------------------------
section("combined_data_source")
x <- df$combined_data_source
valid_vals <- c("LTDL", "WRI")
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("2 unique values",              n_unique(x)  == 2,    sprintf("%d unique",   n_unique(x)))
check("Only values: LTDL, WRI",
      all(x[!is.na(x)] %in% valid_vals),
      sprintf("found: %s", paste(sort(unique(x[!is.na(x)])), collapse=", ")))

# --- combined_Trt_ID ---------------------------------------------------------
section("combined_Trt_ID")
x <- df$combined_Trt_ID
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("977 unique values",            n_unique(x)  == 977,  sprintf("%d unique",   n_unique(x)))

# --- combined_Objectives -----------------------------------------------------
section("combined_Objectives")
x <- df$combined_Objectives
check("236 non-null records (24%)",   n_nonnull(x) == 236,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("226 unique values",            n_unique(x)  == 226,  sprintf("%d unique",   n_unique(x)))

# --- combined_Treatment_Type -------------------------------------------------
section("combined_Treatment_Type")
x <- df$combined_Treatment_Type
check("279 non-null records (29%)",   n_nonnull(x) == 279,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("114 unique values",            n_unique(x)  == 114,  sprintf("%d unique",   n_unique(x)))

# --- combined_Trt_T_S --------------------------------------------------------
section("combined_Trt_T_S")
x <- df$combined_Trt_T_S
check("279 non-null records (29%)",   n_nonnull(x) == 279,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("68 unique values",             n_unique(x)  == 68,   sprintf("%d unique",   n_unique(x)))

# --- combined_Trt_T_M --------------------------------------------------------
section("combined_Trt_T_M")
x <- df$combined_Trt_T_M
valid_categories <- c("Herbicide/Weeds/Chemical", "Vegetation/Soil Manipulation",
                      "Seeding", "Prescribed Burn")
check("279 non-null records (29%)",   n_nonnull(x) == 279,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("22 unique values",             n_unique(x)  == 22,   sprintf("%d unique",   n_unique(x)))
# Values are semicolon-delimited combos; check all components belong to vocab
all_components <- unlist(strsplit(x[!is.na(x)], ";"))
all_components <- trimws(all_components)
unexpected <- setdiff(all_components, valid_categories)
check("All components in controlled vocabulary",
      length(unexpected) == 0,
      if (length(unexpected) == 0) "OK"
      else sprintf("unexpected: %s", paste(unexpected[1:min(3,length(unexpected))], collapse=", ")))

# --- combined_Planned_Implementation -----------------------------------------
section("combined_Planned_Implementation")
x <- df$combined_Planned_Implementation
check("250 non-null records (26%)",   n_nonnull(x) == 250,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("230 unique values",            n_unique(x)  == 230,  sprintf("%d unique",   n_unique(x)))

# --- combined_Actual_Implementation ------------------------------------------
section("combined_Actual_Implementation")
x <- df$combined_Actual_Implementation
check("193 non-null records (20%)",   n_nonnull(x) == 193,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("187 unique values",            n_unique(x)  == 187,  sprintf("%d unique",   n_unique(x)))

# --- combined_Trt_Concerns ---------------------------------------------------
section("combined_Trt_Concerns")
x <- df$combined_Trt_Concerns
check("273 non-null records (28%)",   n_nonnull(x) == 273,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("245 unique values",            n_unique(x)  == 245,  sprintf("%d unique",   n_unique(x)))

# --- combined_Trt_Concerns_Desc ----------------------------------------------
section("combined_Trt_Concerns_Desc")
x <- df$combined_Trt_Concerns_Desc
check("262 non-null records (27%)",   n_nonnull(x) == 262,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("248 unique values",            n_unique(x)  == 248,  sprintf("%d unique",   n_unique(x)))

# --- Objective_notes1 --------------------------------------------------------
section("Objective_notes1")
x <- df$Objective_notes1
check("266 non-null records (27%)",   n_nonnull(x) == 266,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("38 unique values",             n_unique(x)  == 38,   sprintf("%d unique",   n_unique(x)))

# --- Objective_notes2 --------------------------------------------------------
section("Objective_notes2")
x <- df$Objective_notes2
check("217 non-null records (22%)",   n_nonnull(x) == 217,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("165 unique values",            n_unique(x)  == 165,  sprintf("%d unique",   n_unique(x)))

# --- OBJECTIVE ---------------------------------------------------------------
section("OBJECTIVE")
x <- df$OBJECTIVE
valid_components <- c("increase_PFG","decrease_PFG","increase_SHR","decrease_SHR",
                      "increase_TRE","decrease_TRE","increase_AFG","decrease_AFG",
                      "null_AFG","multi_directional",
                      # allow title-case variants observed in data
                      "Increase_PFG","Decrease_PFG","Increase_SHR","Decrease_SHR",
                      "Increase_TRE","Decrease_TRE","Increase_AFG","Decrease_AFG",
                      "Null_AFG","Multidirectional","Multidirectional ")
check("921 non-null records (94%)",   n_nonnull(x) == 921,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("45 unique values",             n_unique(x)  == 45,   sprintf("%d unique",   n_unique(x)))
all_obj_parts <- trimws(unlist(strsplit(x[!is.na(x)], ",")))
unexpected_obj <- setdiff(all_obj_parts, valid_components)
check("All components in controlled vocabulary",
      length(unexpected_obj) == 0,
      if (length(unexpected_obj) == 0) "OK"
      else sprintf("%d unexpected component(s); first: %s",
                   length(unexpected_obj),
                   paste(unexpected_obj[1:min(5, length(unexpected_obj))], collapse=", ")))

# --- combined_FeatureID ------------------------------------------------------
section("combined_FeatureID")
x <- df$combined_FeatureID
check("0 non-null records (100% missing)",
      n_nonnull(x) == 0,
      sprintf("%d non-null", n_nonnull(x)))

# --- Initials ----------------------------------------------------------------
section("Initials")
x <- df$Initials
check("978 non-null records",         n_nonnull(x) == 978,  sprintf("%d non-null", n_nonnull(x)))
check("27 unique values",             n_unique(x)  == 27,   sprintf("%d unique",   n_unique(x)))

# --- NOTES from classifier (HML or WR) ---------------------------------------
section("NOTES from classifier (HML or WR)")
x <- df[["NOTES from classifier (HML or WR)"]]
check("88 non-null records (9%)",     n_nonnull(x) == 88,   sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("87 unique values",             n_unique(x)  == 87,   sprintf("%d unique",   n_unique(x)))

# --- combined_Project.Narrative ----------------------------------------------
section("combined_Project.Narrative")
x <- df$combined_Project.Narrative
check("678 non-null records (69%)",   n_nonnull(x) == 678,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("663 unique values",            n_unique(x)  == 663,  sprintf("%d unique",   n_unique(x)))

# --- combined_Final.Methods --------------------------------------------------
section("combined_Final.Methods")
x <- df$combined_Final.Methods
check("700 non-null records (72%)",   n_nonnull(x) == 700,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("693 unique values",            n_unique(x)  == 693,  sprintf("%d unique",   n_unique(x)))

# --- combined_ActionDescription ----------------------------------------------
section("combined_ActionDescription")
x <- df$combined_ActionDescription
check("700 non-null records (72%)",   n_nonnull(x) == 700,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("147 unique values",            n_unique(x)  == 147,  sprintf("%d unique",   n_unique(x)))

# --- combined_TreatmentTypeDescription ---------------------------------------
section("combined_TreatmentTypeDescription")
x <- df$combined_TreatmentTypeDescription
check("245 non-null records (25%)",   n_nonnull(x) == 245,  sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("73 unique values",             n_unique(x)  == 73,   sprintf("%d unique",   n_unique(x)))

# --- combined_HerbicideDescription -------------------------------------------
section("combined_HerbicideDescription")
x <- df$combined_HerbicideDescription
valid_herbicides <- c("2 4-D;Tordon","Arsenal","Curtail","Garlon 3A","Garlon 4",
                      "Milestone","Milestone;Grazon P+D","Milestone;Plateau",
                      "Plateau","Rodeo","Roundup","Spike","Tordon 22k")
check("24 non-null records (2%)",     n_nonnull(x) == 24,   sprintf("%d non-null (%g%%)", n_nonnull(x), pct(n_nonnull(x), EXPECTED_ROWS)))
check("13 unique values",             n_unique(x)  == 13,   sprintf("%d unique",   n_unique(x)))
check("Only known herbicide values",
      all(x[!is.na(x)] %in% valid_herbicides),
      sprintf("found: %s", paste(sort(unique(x[!is.na(x)])), collapse=", ")))

# --- combined_TREATMENT_ASSIGNMENT -------------------------------------------
section("combined_TREATMENT_ASSIGNMENT")
x <- df$combined_TREATMENT_ASSIGNMENT
check("All 979 records non-null",     n_nonnull(x) == 979,  sprintf("%d non-null", n_nonnull(x)))
check("152 unique values",            n_unique(x)  == 152,  sprintf("%d unique",   n_unique(x)))

# --------------------------------------------------------------------------- #
# 5. Summary
# --------------------------------------------------------------------------- #

cat("\n=============================================================\n")
cat(" Validation complete.\n")
cat("=============================================================\n")
