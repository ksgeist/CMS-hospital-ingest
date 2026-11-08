## -------------------------------------------------------------------------- ##
## Author:     K.S. Geist
## Date:       13 October 2026
## Purpose:    Source module to clean and join either a list (cmsData) or CSV 
##             files from a current CMS Provider Data API download
## TO RUN:
## source(file = "cleanJoinCMS.R")

## DEVELOPER NOTES:
# Includes a switch to indicate if the user wants to load the files or use cmsData
# Keeps snake_case column names
# No imputation for suppressed CMS counts
# Keeps outcome selection explicit (e.g., PN for pneumonia, AMI for heart attack)
# Pivots configured long-format measure tables (not all are long form)
# Keeps predictor selection, encoding, modeling, and spatial analysis downstream
# Output is a single dataframe ready for downstream work
## -------------------------------------------------------------------------- ##

# SETUP ----------------------------------------------------------------------
# Install pacman if it's missing:
if (!requireNamespace("pacman", quietly = TRUE)) { install.packages("pacman") }

## Load the packages needed
pacman::p_load(tidyverse,
               janitor,
               jsonlite,
               purrr,
               httr2
)

## Allow slower requests more time
options(timeout = 300)

# GLOBAL ENVIROMENT ---------------------------------------------------------

## Set the patterns for missing and suppressed values; allows easier changes if
## CMS ever changes these in the future

cmsMissings <- c(
  "", "N/A", "NA", "NULL", ".", "Not Available", "Not Applicable",
  "Not Available.", "Not Available", "NULL", "Not Reported"
)

cmsSuppressionPattern <- regex(
  "too few to report|number of cases too small|small numbers",
  ignore_case = TRUE
)

geoFields <- c("facility_name", "address", "citytown",
               "state", "zip_code", "countyparish", "telephone_number")

## Set the filepath for where the CSV files live
filepath <- file.path(getwd(), "rawHospitalCMS_files")

# HELPER FUNCTIONS -----------------------------------------------------------

## 1. Get the Footnotes as a Lookup Table ------------------------------------
## We can also get the current lookup table for footnote values from
## https://data.cms.gov/provider-data/dataset/y9us-9xdf, which we have to get
## from API again

## Get the footnote lookup table:
getFootnoteLU <- function(offset = 0L, 
                          limit = 1000L,
                          writeCSV = F) {

  id <- "y9us-9xdf"
  baseURL <- paste0(
    "https://data.cms.gov/provider-data/api/1/datastore/query/",
    id,
    "/0"
  )  
  
  pageURL <- paste0(baseURL,
    "?offset=", as.integer(offset),
    "&limit=", as.integer(limit)
  )

  ## Use fromJSON() URL connection; make sure to increase the timeout if needed
  ## in global env, but this is so small it should be fine

  page <- jsonlite::fromJSON(pageURL, simplifyVector = TRUE)

  if (is.null(page$results)) {
    stop(
      "CMS response did not contain a `results` field. Top-level fields: ",
      paste(names(page), collapse = ", "),
      ". Check the dataset ID and API response schema."
    )
  }

  ## Optional local copies for user reference
  if(writeCSV) {
    readr::write_csv(page, file = paste0(filepath, "footnote-lookup-table.csv"))
  }
  
return(tibble::as_tibble(page$results))
  
}

suppressionFootnoteValues <- getFootnoteLU() %>% 
  filter(str_detect(footnote_text, cmsSuppressionPattern)) %>%
  pull(footnote) %>%
  as.character() %>%
  unique()

## 2. Minimally clean datsets for missing and suppressed values -------------- 
## Normalize known missing-value tokens while preserving suppression as a
## separate indicator column. Suppressed source values become NA, but the
## *_suppressed flag retains the fact that CMS suppressed the value in case it 
## is needed in downstream analyses for the user

cleanCMS <- function(data, 
                     addSuppressionFlags = TRUE,
                     suppressionFootnotes = suppressionFootnoteValues) {
  
  # --- Arguments ------------------------------ #
  ## data:                dataset to process
  ## addSuppressionFlags: indicates whether new columns should be made to indicate
  ##                      whether the data were suppressed and that's why it's 
  ##                      missing; necessary for missing analyses
  ## suppressionFootnotes: flags from footnotes to indicate that data were suppressed
  # -------------------------------------------- #
  
  ## Error handling; stop if it's not a dataframe
  stopifnot(is.data.frame(data))
  
  dat <- data

  ## Identify footnote columns
  footnoteCols <- names(dat)[
    str_detect(names(dat), "footnote$")
  ]
  
  ## Use vapply to ensure that grab which character columns are indeed character
  ## type; this will help us find the ones that have suppressed values because
  ## otherwise numeric columns will show up as character type at this stage
  charCols <- names(dat)[vapply(dat, is.character, logical(1))] 
  ## BUT NOT FACILITY ID OR OTHER IDENTIFYING INFO
  charCols <- setdiff(charCols, c("facility_id", geoFields, "measure_id", "measure_name"))

  ## Store value-based suppression indicators before changing values; using lapply()
  ## loops over all the columns in charCols and looks for whether there is any
  ## of the suppression patterns in cmsSuppressionPattern; returns logical
  valueSuppression <- set_names(
    lapply(charCols, function(c) {
      x <- dat[[c]]
      ## excludes values that are already missing + checks for a matches to suppression patterns
      !is.na(x) & str_detect(x, cmsSuppressionPattern)
    }),
    charCols
  )

  ## Initialize/update flags for values that explicitly contain suppression phrases
  if (addSuppressionFlags) {   ## Only if user indicates it
    ## For each of the character columns:
    for (c in charCols) {
        ## New name of the flagged column
        flagName <- paste0(c, "_suppressed")
        ## Identify if it's a suppressed column
        dat[[flagName]]  <- valueSuppression[[c]]
    }
  }

  ## Now check whether it's a footnote labeled suppression value:
  for (f in footnoteCols) {
        footnoteValue <- as.character(dat[[f]])
        ## Create logical vector indicating which rows have a footnote code in suppressionFootnotes
        footnoteSuppressed <- (
          !is.na(footnoteValue) &
            footnoteValue %in% suppressionFootnotes
        )
  
     ## If the footnote column is paired with a value column,
     ## e.g. number_of_discharges_footnote, flag that value column but "footnote"
     ## stays "footnote"
     valueName <- str_remove(f, "_footnote$")
      
     ## if the value name isn't the same as the footnote and it's an existing column:  
      if (valueName != f && valueName %in% names(dat)) {
        ## add the "_suppressed" at the end of the column so it doesn't overwrite
        flagName <- paste0(valueName, "_suppressed")

        ## if flag creation is enabled, this checks whether the corresponding
        ## flag already exists; if not, initializes to FALSE
          if (addSuppressionFlags) {
            if (!flagName %in% names(dat)) {
                dat[[flagName]] <- FALSE
            }
            ## Combines the existing flag with the new footnote-based flag to
            ## allow a value to be flagged by its text, its footnote, or both
            dat[[flagName]] <- (
              coalesce(as.logical(dat[[flagName]]), FALSE) |
                footnoteSuppressed
            )
          }

          ## A suppression footnote means the paired value is not usable
          ## as an observed value; so replaces it as NA
          dat[[valueName]][footnoteSuppressed] <- NA

          ## -------------- ##
          ## This is the alternative path for the group footnote columns,
          ## like in hospital_general_information:
          } else if (str_detect(f, "_group_footnote$")) {
            
            ## Group-level footnote has no directly matching value column.
            ## Example: te_group_footnote -> te_group_suppressed
            groupName <- str_remove(f, "_footnote$")
            flagName <- paste0(groupName, "_suppressed")
            
            ## Create or update a group-level suppression flag
            if (addSuppressionFlags) {
              if (!flagName %in% names(dat)) {
                dat[[flagName]] <- FALSE
              }
              
              dat[[flagName]] <- (
                coalesce(as.logical(dat[[flagName]]), FALSE) |
                  footnoteSuppressed
              )
            }
            
          ## -------------- ##
          ## This is the alternative path when the column is named simply footnote:
          } else  if (f == "footnote") {
       
           ## Split multi-code footnotes such as "1,29" into individual codes
           footnoteCodes <- str_split(
             as.character(dat[[f]]),
             "\\s*,\\s*"
           )
           
           ## Check whether ANY code in each row is a suppression code and return
           ## logical if it is
           footnoteSuppressed <- vapply(
             footnoteCodes,
             function(codes) {
               any(str_trim(codes) %in% as.character(suppressionFootnotes))
             },
             logical(1)
           )
       
           ## Rows with a missing footnote are not suppressed by this rule
           footnoteSuppressed[is.na(dat[[f]])] <- FALSE
           
           ## Create or update the generic footnote suppression flag
           if (addSuppressionFlags) {
             
             flagName <- "footnote_suppressed"
             
             if (!flagName %in% names(dat)) {
               dat[[flagName]] <- FALSE
             }
             
             dat[[flagName]] <- (
               coalesce(as.logical(dat[[flagName]]), FALSE) |
                 footnoteSuppressed
             )
           }
         }
   }     
  
   ## ----- end for loop ----- ##
  
    ## Normalize ordinary missing values and suppression phrases
    ## Do this after creating the suppression flags
    ## important because the df may now contain new columns or modified values
    charCols <- names(dat)[
      vapply(dat, is.character, logical(1))
    ]
    
    ## Loop over the columns and identify ordinary missing value placeholders, 
    ## including whitespace and the list of missing values. also converts any
    ## characters to lower case
    for (t in charCols) {
      x <- dat[[t]]
        
      isMissing <- (
        !is.na(x) &   ## avoids treating already missing values as ordinary text placeholders
          str_to_lower(str_trim(x)) %in%
          str_to_lower(cmsMissings)
      )
      
      ## The earlier work detected flags but did not actually replace those values 
      isSuppressed <- (
        !is.na(x) &
          str_detect(x, cmsSuppressionPattern)
      )
      ## But also ensure that placeholders and suppression phrases do not remain
      ## in the character columns as if they were valid observations...
      x[isMissing | isSuppressed] <- NA_character_
        dat[[t]] <- x
      }
  
  return(dat)
}

## Convert numeric-like character columns without coercing identifiers or
## categorical fields
## This helper is intentionally opt-in and column-specific
parseCMSnumeric <- function(data, 
                            columns) {
  
  # --- Arguments ------------------------------ #
  ## data:     dataset to process
  ## columns:  vector of columns that meet the criteria for processing
  # -------------------------------------------- #
  
  ## Error handling if it's not a data frame
  stopifnot(is.data.frame(data))
  ## Error handling in the event no numeric columns are passed
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols)) {
    warning("Numeric columns not found and skipped: ",
            paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  ## Otherwise, grab the columns specified
  cols <- intersect(columns, names(data))
  ## Leverage readr's parse_number to fix 'em
  data %>%
    mutate(across(all_of(cols), ~ readr::parse_number(as.character(.x))))
}

## 3. Readmissions outcome table ---------------------------------------------

## The CMS readmissions dataset uses measure codes such as PN and AMI for outcomes
## This function allows the user to select which outcome(s) they want to compare
cleanReadmissions <- function(data,
                              condition = "PN",
                              measure_codeColumn = NULL, 
                              numericsCols = c("number_of_discharges", 
                                               "number_of_readmissions",
                                               "predicted_readmission_rate",
                                               "expected_readmission_rate",
                                               "excess_readmission_ratio"
                                               ),
                              keepAllConditions = TRUE) {
  
  # --- Arguments ------------------------------ #
  ## data:               dataset to process
  ## condition:          readmissions condition(s); options include:
  ##                     PN, COPD, KNEE, HF, CABG, AMI; combinations can be given as vector
  ## numericsCols:       columns that may need to be converted from character after
  ##                     removal of suppression values
  ## keepAllConditions:  retains all conditions
  # -------------------------------------------- #
  
  ## Warn the user that they have chosen more than one condition
  if(length(condition) > 1) {
    warning("You have selected more than one condition. Do you wish to proceed?\n")
  } else {
    cat("\nProcessing readmissions data for", condition, "...\n")
  }
  
  ## Call cleanCMS on the readmissions dataset
  dat <- cleanCMS(data)

  ## Set the required columns
  required <- c("facility_id", "measure_name")
  ## Drop everything else
  missing_required <- setdiff(required, names(dat))
  ## Error handling; if we somehow are missing what's required, warn the user
  if (length(missing_required)) {
    stop("Readmissions data is missing required column(s): ",
         paste(missing_required, collapse = ", "),
         ". Available columns: ", paste(names(dat), collapse = ", "))
  }

  ## Some CMS vintages encode the condition as a short code in measure_name
  ## others may expose a separate measure code; allow explicit override:
  codeColumn <- measure_codeColumn
  if (is.null(codeColumn) || !codeColumn %in% names(dat)) codeColumn <- "measure_name"

  dat <- dat %>%
    mutate(
      across(all_of("facility_id"), as.character),
      .cmsMeasureValue = as.character(.data[[codeColumn]]),
      ## Older/current CMS exports may encode the condition as PN/AMI directly
      ## or inside a label such as READM-30-PN-HRRP
      .cmsConditionCode = str_to_upper(
        str_match(.cmsMeasureValue, "(?:READM-30-)?(PN|AMI|HF|COPD|CABG|HIP-KNEE)(?:-HRRP)?")[, 2]
      )
    ) %>%
    mutate(
      .cmsConditionCode = coalesce(
        .cmsConditionCode,
        str_to_upper(str_trim(.cmsMeasureValue))
      )
    )
  
  ## If the user doesn't want to keep all the conditions, then filter
  if (!keepAllConditions) {
    dat <- dat %>% filter(.cmsConditionCode == str_to_upper(condition))
  }

  ## Also run the numeric parser function on the numeric columns
  dat <- parseCMSnumeric(dat, numericsCols)

  ## Retain the condition code as a clear, stable label for downstream use
  dat %>%
    select(-.cmsMeasureValue) %>%
    rename(condition_code = .cmsConditionCode)
  
  return(dat)
}

# cleanReadmissions(hospital_readmissions_reduction_program)

## 4. Pivot a configured long-format measure table ----------------------------

# `measures` is a character vector of exact measure labels to keep. Pass NULL
# to keep all measures. `id` and `measureColumn` are expected in snake_case.
#
# `cols2drop` is for identifiers/metadata that should not become model features.
# It is applied only when those columns exist.
cols2drop <- c("start_date", 
              "end_date", 
              "facility_name", 
              "address", 
              "city_town",
              "citytown",
              "state",
              "zip_code",
              "county_parish",
              "countyparish",
              "telephone_number",
              "footnote", 
              "measure_id", 
              "condition",
              "lower_estimate", 
              "higher_estimate"
)

pivotMeasures <- function(data = cmsData,
                          measures = NULL,
                          name_prefix = NULL,
                          values_f = list) {

  # --- Arguments ------------------------------ #
  ## data:        dataset to process
  ## measures:          
  ## name_prefix:       
  ## values_f:  
  # -------------------------------------------- #
  
  ## Runs the cleanCMS function on the dataset and then drops the columns that
  ## have been specified to be dropped
  dat <-  cleanCMS(data) %>% 
    select(-any_of(cols2drop)) %>%
    ## Ensure that facility id is always character
    mutate(
      across(all_of("facility_id"), as.character),
      ## And same for the measure columns
      across(all_of(measureColumn), as.character)
    )

  ## Set the facility id as the keys
  keys <- c("facility_id", measureColumn)
  ## Error handling; stop if for any reason the table is missing required columns
  missing_keys <- setdiff(keys, names(dat))
  if (length(missing_required)) {
    stop("Long-format table is missing required column(s): ",
         paste(missing_keys, collapse = ", "),
         ". Available columns: ", paste(names(dat), collapse = ", "))
  }


  if (!is.null(measures)) {
    dat <- dat %>% filter(.data[[measureColumn]] %in% measures)
  }

  if (nrow(dat) == 0L) {
    warning("No rows remain after filtering measures. Returning an ID-only tibble.")
    return(tibble(!!id := character()))
  }

  # Prevent accidental many-to-many duplication from silently changing the
  # row count. Duplicate measure rows are retained as list-columns only when
  # the caller explicitly supplies a values_f.
  valueColumns <- setdiff(names(dat), keys)

  if (!length(valueColumns)) {
    stop("No value columns remain to pivot after removing IDs and measure label.")
  }

  # Prefix measure names when helpful, but default to the original measure text
  # so this stays close to the Demo 1 feature names.
  if (!is.null(name_prefix)) {
    dat <- dat %>%
      mutate("{measureColumn}" := paste0(name_prefix, .data[[measureColumn]]))
  }

  dups <- dat %>%
    count(across(all_of(keyColumns)), name = ".n") %>%
    filter(.n > 1L)

  if (nrow(dups) > 0L && identical(values_f, list)) {
    warning(
      nrow(dups),
      " facility/measure key combination(s) have duplicate rows. ",
      "Review these before modeling; pivot_wider will list-pack values."
    )
  }

  dat %>%
    pivot_wider(
      names_from = "measure_name",
      values_from = all_of(valueColumns),
      names_sep = "_",
      values_f = values_f
    ) %>% View()
}

## 5. Join hospital-level tables ---------------------------------------------

# `measureRules` is a named list. Each name identifies a dataset in `datasets`;
# each value is a character vector of exact measure labels to retain. Use NULL
# for a dataset that is already wide or should be joined without pivoting.
#
# Example:
# measureRules <- list(
#   healthcare_associated_infections = "MRSA Bacteremia",
#   outpatient_imaging_efficiency = "Abdomen CT Use of Contrast Material",
#   complications_and_deaths = c("Death rate for heart attack patients", ...)
# )
#
# The base table should have one row per facility. The function reports
# duplicate facility IDs and checks join coverage after each table is joined.
joinDatasets <- function(datasets,
                         hospitalInfo,
                         measureRules = list(),
                         readmissions = NULL,
                         condition = NULL,
                         readmissions_measureColumn = "measure_name",
                         quiet = FALSE,
                         requireUniqBase = TRUE) {
  
  # --- Arguments ------------------------------ #
  ## datasets:        dataset to process
  ## hospitalInfo:          
  ## measureRules:       
  ## readmissions:  
  # -------------------------------------------- #

  ## Error handling
  stopifnot(is.list(datasets), is.data.frame(hospitalInfo))

  ## Set the facility id:
  id <- "facility_id"
  
  ## Create a base table from the hospital information table
  base <- cleanCMS(hospitalInfo) %>%
    mutate(across(all_of(id), as.character))

  if (!id %in% names(base)) {
    stop("hospitalInfo does not contain join key '", id, "'.")
  }

  base_dups <- base %>% count(.data[[id]]) %>% filter(n > 1L)
  if (nrow(base_dups) > 0L) {
    msg <- paste0("hospitalInfo has ", nrow(base_dups),
                  " duplicated facility ID(s).")
    if (requireUniqBase) stop(msg, " Make the base one row per facility first.")
    warning(msg, call. = FALSE)
  }

  # Standardize and validate dataset names against the configuration.
  if (length(measureRules) && is.null(names(measureRules))) {
    stop("measureRules must be a *named* list keyed by dataset name.")
  }

  for (datName in names(datasets)) {
    dat <- datasets[[datName]]
    if (!is.data.frame(dat)) next
    dat <- cleanCMS(dat)

    if (!id %in% names(dat)) {
      if (!quiet) message("Skipping ", datName, ": no ", id, " column.")
      next
    }

    # The base table is already in the join. Don't rejoin it as a predictor table.
    if (identical(datName, "hospital_general_information")) next

    measureRule <- measureRules[[datName]]
    is_long_measure_table <- "measure_name" %in% names(dat) &&
      !is.null(measureRule)

    if (is_long_measure_table) {
      wide <- pivotMeasures(
        dat,
        measures = measureRule,
        id = id,
        measureColumn = "measure_name"
      )
    } else if ("measure_name" %in% names(dat) &&
               is.null(measureRule)) {
      if (!quiet) {
        message("Skipping ", datName,
                ": has measure_name but no measureRules entry.")
      }
      next
    } else {
      # Already-wide table: keep it wide, but remove repeated hospital metadata.
      wide <- dat %>%
        select(-any_of(c(
          "facility_name", "address", "city_town", "state", "zip_code",
          "county_parish", "telephone_number"
        ))) %>%
        mutate(across(all_of(id), as.character))
    }

    # Join key diagnostics.
    wide_dups <- wide %>% count(.data[[id]]) %>% filter(n > 1L)
    if (nrow(wide_dups) > 0L) {
      stop("Dataset '", datName, "' has ", nrow(wide_dups),
           " rows per duplicated facility ID after pivoting. Resolve dups ",
           "before joining; otherwise the join may multiply hospital rows.")
    }

    newCols <- setdiff(names(wide), id)
    overlap <- intersect(newCols, names(base))
    if (length(overlap)) {
      # Make overlaps explicit rather than silently generating .x/.y columns.
      names(wide)[match(overlap, names(wide))] <-
        paste0(overlap, "__", janitor::make_clean_names(datName))
    }

    n_before <- nrow(base)
    baseID <- unique(base[[id]])
    incomingID <- unique(wide[[id]])

    base <- full_join(base, wide, by = id)

    if (!quiet) {
      message(
        "Joined ", datName, ": ",
        nrow(wide), " rows; ",
        sum(incomingID %in% baseID), " facility IDs overlapped; ",
        sum(!incomingID %in% baseID), " new facility IDs; ",
        n_before, " -> ", nrow(base), " rows."
      )
    }
  }

  if (!is.null(readmissions)) {
    if (is.null(condition)) {
      stop("Supply `condition` (e.g., 'PN' or 'AMI') when joining readmissions.")
    }

    readm <- cleanReadmissions(
      readmissions,
      condition = condition,
      id = id,
      measureColumn = readmissions_measureColumn
    )

    # Remove source metadata that dups hospitalInfo fields.
    readm <- readm %>%
      select(-any_of(c(
        "facility_name", "address", "city_town", "state", "zip_code",
        "county_parish", "telephone_number", "footnote", "start_date",
        "end_date"
      )))

    if (nrow(readm) == 0L) {
      warning("No readmissions rows found for condition '", condition, "'.")
    }

    readm_dups <- readm %>% count(.data[[id]]) %>% filter(n > 1L)
    if (nrow(readm_dups) > 0L) {
      stop("Readmissions data has multiple rows per facility for condition '",
           condition, "'. Inspect measure coding and aggregation before joining.")
    }

    overlap <- intersect(setdiff(names(readm), id), names(base))
    if (length(overlap)) {
      names(readm)[match(overlap, names(readm))] <-
        paste0(overlap, "__readmissions")
    }

    base <- full_join(base, readm, by = id)
    base <- base %>% mutate(condition = condition)

    if (!quiet) {
      message("Joined readmissions outcome for condition ", condition,
              "; final rows: ", nrow(base), ".")
    }
  }

  base %>% distinct()
}


## 6. First-pass configuration -----------------------------------------------

# These labels are carried forward from Demo 1 / reRunDemoData.R. CMS may revise
# wording in current releases; check exact current labels with:
#   sort(unique(cmsData$complications_and_deaths$measure_name))
# Update labels here rather than changing the joining functions.

demo1_measureRules <- list(
  healthcare_associated_infections = c("MRSA Bacteremia"),
  outpatient_imaging_efficiency = c("Abdomen CT Use of Contrast Material"),
  complications_and_deaths = c(
    "Death rate for heart attack patients",
    "Perioperative pulmonary embolism or deep vein thrombosis rate",
    "CMS Medicare PSI 90: Patient safety and adverse events composite",
    "Postoperative respiratory failure rate"
  ),
  timely_and_effective_care = c(
    "Healthcare workers given influenza vaccination",
    "Percentage of healthcare personnel who completed COVID-19 primary vaccination series",
    "Average (median) time patients spent in the emergency department before leaving from the visit A lower number of minutes is better",
    "Left before being seen",
    "Venous Thromboembolism Prophylaxis",
    "Intensive Care Unit Venous Thromboembolism Prophylaxis",
    "Emergency department volume"
  ),
  unplanned_hospital_visits = c("Hospital return days for heart attack patients"),
  patient_survey_hcahps = c(
    "Nurse communication", "Doctor communication", "Staff responsiveness",
    "Communication about medicines", "Discharge information", "Care transition",
    "Cleanliness", "Quietness", "Overall hospital rating", "Recommend hospital"
  )
)

# WRAPPER ---------------------------------------------------------------------
tidyNjoin <- function(importFiles = T
    ) {
  
  ## If the switch is TRUE, then we need to import each of the files as a dataframe
  ## first. Otherwise, it will extract each of the list elements from cmsData
  if(importFiles) {
      ## Initialize a list:
      cmsData <- list()
    
      ## Grab just the hospital-level files:
      files <- list.files(path = filepath, pattern = ".csv")
      ## Turn them into a dataframe for easier filtering
      files <- data.frame(file = files) %>% 
      ## Then filter out non-dataset files that could be in the directory
      filter(file != "cms-retrieval-manifest.csv", file != "full-CMS-catalog.csv")
  
    ## Iterate over the hospital-level files and read each one, naming 
    ## dynamically & storing the object in the global envirocent
    for(f in 1:nrow(files)) {
    cat("Reading in file", files$file[f], "...")
      
      ## First check the headers of the files
      headers <- names(read_csv(paste0(filepath, "/", files$file[f]), 
                                n_max = 0, show_col_types = FALSE))
      
      ## Then look to see if the headers contains footnote or not
      if("footnote" %in% headers) {
        ## Read in the data including the footnote column
        dat <- read_csv(paste0(filepath, "/", files$file[f]),
                        show_col_types = FALSE,
                        ## HOT FIX of the "N/A" characters that were problematic
                        na = c("", "NA", "N/A", "Not Applicable", "Not Available","NULL", "."),
                        ## HOT FIX if there's a footnote column, it's character type
                        col_types = cols(footnote = col_character()))

      } else { ## If no footnote column
        dat <- read_csv(paste0(filepath, "/", files$file[f]),
                        show_col_types = FALSE,
                        ## HOT FIX of the "N/A" characters that were problematic
                        na = c("", "NA", "N/A", "Not Applicable", "Not Available","NULL", "."))
      }
      
      dat <- dat %>% 
        ## HOT FIX to convert date columns based on month/day/year
        mutate(across(contains("Date"), ~ mdy(.)))
      
      ## Strip off the -hospital.csv from the end of each file name
      filename <- gsub("-hospital\\.csv$|.csv$", "", files$file[f])
      ## Replace the hyphens with _
      filename <- gsub("-", "_", filename)
      
      ## If any table other than the first table, remove the geo fields:
      if(f > 1) {
        dat <- dat %>% 
          select(-any_of(geoFields))
      }
      
      ## Assign the filename to dat and throw into the global env
      assign(filename, dat)
      
      ## Build a list of the tables:
      cmsData[[filename]] <- dat
    } 
  }
  
  ## Clean up each of the tables:
  for(t in 1:length(cmsData)) {
    
    
  }
  
  
  
  
}


source("cleanJoinCMS.R")




