## -------------------------------------------------------------------------- ##
## Author:     K.S. Geist
## Date:       9 October 2026
## Purpose:    Source module to retrieve CMS Provider Data Catalog hospital 
##             datasets via the public API; saves raw CSVs locally; records a
##             manifest of what was retrieved
## TO RUN:
## source(file = "retrieveCMShospitalAPI.R")

## DEVELOPER NOTES:
## API documentation:  https://data.cms.gov/provider-data/about

#   No API key is required for the Provider Data Catalog API

#   The script retrieves CURRENT datasets, not a historical snapshot 
#   (e.g., FY2024 is whatever is currently stored for FY2024, not initial release)

#   CMS limits each Provider Data Catalog query to 1,500 rows, so the script
#   paginates with offset

#   Raw values are preserved. In particular, suppressed values such as 
# "Too Few to Report" are NOT replaced 

#   Script retrieves source tables only 
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

# GLOBAL ENVIRONMENT ---------------------------------------------------------

## Public Provider Data Catalog API does not require an API key; URL only 
baseURL <- "https://data.cms.gov/provider-data/api/1"

## Max page size documented by CMS for this API; this is to create pagination
pgSize <- 1500L

## Choose source datasets ----------------------------------------------------
## Titles correspond to the source tables used in the hospital workflow but CMS 
## occasionally changes titles or reorganizes datasets. The script searches
## the live catalog and reports any requested titles it cannot match.

## Function will have an override to allow you to change the names of titles, if
## needed for a given year.
cmsDatasetNames <- c(
  "Hospital General Information",
  "Healthcare Associated Infections - Hospital",
  # "Payment and Value of Care - Hospital",  ## older name from previous work
  "Outpatient Imaging Efficiency - Hospital",
  "Complications and Deaths - Hospital",
  # "Medicare Hospital Spending Per Patient - Hospital", ## older name
  "Timely and Effective Care - Hospital",
  "Unplanned Hospital Visits - Hospital",
  "Patient-Reported Outcomes - Hospital",
  "Patient survey (HCAHPS) - Hospital",
  "Hospital Readmissions Reduction Program",
  "Maternal Health - Hospital",
  "Medicare Spending Per Beneficiary - Hospital",
  "Hospital Value-Based Purchasing (HVBP) - Safety",	
  "Hospital Value-Based Purchasing (HVBP) - Total Performance Score"
)

## Define helper functions ----------------------------------------------------

### Helpers for dataset titles ------------------------------------------------
## The API needs a dataset identifier, not just a title to retrieve the dataset
## Match titles without worrying about capitalization, punctuation, or the
## optional suffix "Hospital"

#### Grab CMS dataset catalog --------------------------------------------------
## CMS data repo is challenging to navigate, including documentation of names
## changed over time. To facilitate troubleshooting and/or dataset choices,
## grab the dataset catalog for HCAHPS data and dump a catalog for the user

grabCatalog <- function(odir = odir, 
                        baseURL = NA) {
  # --- Arguments ------------------------------ #
  ## odir:     outdir, defaults to global env odir
  ## baseURL: URL for the JSON query of the catalog; defaults to global env baseURL
  # -------------------------------------------- #
    catalogURL <- paste0(
      baseURL,
      "/metastore/schemas/dataset/items"
    )
    
    catalog <- fromJSON(catalogURL, simplifyVector = FALSE)
    
    ## Turn the catalog's list of records into a simple data frame:
    catalogDF <- map_dfr(catalog, function(x) {
      tibble(
        ## If any part of the metadata is NULL make it NA
        title = if (!is.null(x$title)) x$title else NA_character_,
        identifier = if (!is.null(x$identifier)) x$identifier else NA_character_,
        modified = if (!is.null(x$modified)) x$modified else NA_character_
      )
    })
    
    ## If no outdirectory is provided don't write a CSV, otherwise:
    if(!is.na(odir)) {
      ## Write the catalog out for the user as a df
      write_csv(catalogDF, file.path(odir, "full-CMS-catalog.csv"))
    }

    return(catalogDF)
}

#### Standardize dataset title names -------------------------------------------
#### Just does some quick text handling to standardize the dataset title names
standardizeTitle <- function(x) {
  # --- Arguments ------------------------------ #
  ## x: element of a vector, here the title of a dataset
  # -------------------------------------------- #
    x %>%
      ## make lower case
      str_to_lower() %>% 
      ## Drop any non alpha-numeric chars
      str_replace_all("[^a-z0-9]+", " ") %>% 
      # ## Strip out the ending 'hospital' -- nm. we won't need this
      # str_remove("\\s+hospital$") %>% 
      ## removes whitespace at the start/end + replaces all internal \s+ with \s 
      str_squish()
}

#### Find the dataset title in the catalog --------------------------------------
#### This is the function that performs matching and error handling for whether
#### a supplied dataset name is in the current catalog
findTitle <- function(requestedTitle, 
                      catalogDF = catalogDF) {
  # --- Arguments ------------------------------ #
  ## requestedTitle:  requested title of a dataset
  ## catalogDF: a catalog of titles
  # -------------------------------------------- #
  
    ## Filter the dataset to just the current title under investigation
    matches <- catalogDF %>%
      filter(standardizeTitle(title) == standardizeTitle(requestedTitle))
    
    ## If no match is found
    if (nrow(matches) == 1L) {
      return(matches)
    }
    ## Returns a message that the given title wasn't found
    message("\nCould not find an exact match for: ", requestedTitle)
    
    ## If not found, then show likely candidates so the user can update datasetNames
    words <- str_split(standardizeTitle(requestedTitle), "\\s+")[[1]]
    words <- words[nchar(words) > 3]
    
    ## Create a set of candidate titles
    candidates <- catalogDF %>%
      ## Leverage str_detect to look for anything close
      filter(if_any(title, ~ str_detect(string = 
        standardizeTitle(.x),
        fixed(paste(words[seq_len(min(2, length(words)))], collapse = " "))
      ))) %>%
      select(title, identifier) %>%
      slice_head(n = 10)
    
    ## Show them what the candidates are
    if (nrow(candidates) > 0L) 
      cat("Here are some likely candidate(s):\n\n")
      print(candidates)
    ## But return nothing else
    message("Matches were not generated. Fix the dataset titles before proceeding!")
}

### Helpers for actual retrieval -----------------------------------------------

#### Grab a single CMS page to ensure connection validity ----------------------
## This function builds a URL with the dataset ID and row offset, then
## fromJSON() downloads the JSON and converts it to a dataframe

## If you want to see the exact API response, run:
##    test_url <- paste0(baseURL, "/datastore/query/id/0")
##    test_response <- fromJSON(test_url, simplifyVector = FALSE)
## Replace id with an identifier from full-CMS-catalog.csv

getCMSpage <- function(id, 
                       offset, 
                       pgSize = 1500L) {
  
  # --- Arguments ------------------------------ #
  ## id:        identification of requested dataset title
  ## offset:    tells the JSON query query start location; bake into URL
  ## pgSize:    tells the max number of rows for the query; CMS currently limits to 1500
  # -------------------------------------------- #
  
  ## Have to rebuild the page URL at each run for pagination to work correctly
  pageURL <- paste0(
    baseURL,
    "/datastore/query/",
    URLencode(id, reserved = TRUE),
    "/0?offset=",                      ## offset is calculate where to start the next time
    offset,
    "&limit=",
    pgSize                          ## max is currently 1500
  )
  
  ## fromJSON() reads a URL directly; simplifyVector = FALSE keeps the JSON
  ## as nested lists, which makes the metadata structure easier to inspect & 
  ## convert to df
  response <- fromJSON(pageURL, 
                       simplifyVector = FALSE)
  
  ## The CMS API response includes the requested rows under "results", so we can
  ## extract that way
  
  ## Error handling; if the response is null:
  if (is.null(response$results)) {
    stop(
      "The API response did not contain a 'results' element.
      Check the endpoint and inspect the response for this URL:\n",
      pageURL, "!"
    )
  }
  
  ## Return the results:
  return(response$results)
}

#### Retrieve all pages -------------------------------------------------------
## Function will always start at offset 0. Each time, it adds the number of rows
## retrieved to the offset. Functions stops when a page contains fewer rows than
## pgSize.

getCMSdataset <- function(id, 
                          title, 
                          pgSize = 1500L) {
  
  # --- Arguments ------------------------------ #
  ## id:        identification of requested dataset title
  ## offset:    tells the JSON query query start location; bake into URL
  ## pgSize:    tells the max number of rows for the query; CMS currently limits to 1500
  # -------------------------------------------- #
  
    offset <- 0L       ## Start the offset @ zero so you get first row
    allPages <- list() ## Initialize a storage list for all the pages 
    pageNum <- 1L      ## Start page number @ 1
    
    ## Using repeat function in R over for() or while() to loop over each
    ## dataset, adjusting the offset as it goes
    repeat {
      message(title, ": retrieving page ", pageNum,
              " (starting at row ", offset + 1L, ")")
      
      ## Now grab the page, given whatever current id and offset is
      page <- getCMSpage(
        id = id,
        offset = offset,
        pgSize = pgSize
      )
      
      ## Make sure to set the number of rows to the size of the current page
      numRows <- length(page)
      
      ## If there are no rows, exit loop
      if (numRows == 0L) break
      
      ## Convert this page's list of records into a data frame
      pageDF <- map_dfr(page, ~ as_tibble(.x, .name_repair = "unique"))
      ## And start the collation of all the pages using page number as the element
      ## of the list
      allPages[[pageNum]] <- pageDF
      
      ## Move the offset forward by the number of records just retrieved
      offset <- offset + numRows
      ## Importantly, make sure to increment the number of pages
      pageNum <- pageNum + 1L
      
      ## And if we reach a short page, then we know we've reached the end of the dataset
      if (numRows < pgSize) break
      
      ## Found it necessary to add a small pause to avoid sending requests 
      ## back-to-back too quickly, which can (1) cause issues on my end and (2) 
      ## potentially cause security alert at the endpoint
      Sys.sleep(0.2)
    }
    
    ## If there are no pages, just return an empty tibble 
    if (length(allPages) == 0L) {
        return(tibble()) 
      } else {
      ## Otherwise, return a collated dataframe from all the pages
        return(bind_rows(allPages))
      }
}


# WRAPPER ---------------------------------------------------------------------
rawCMS <- function(odir = file.path(getwd(), "rawHospitalCMS_files"),
                   datasetNames = cmsDatasetNames) { 

  ## If the file directory doesn't already exist, create it:
  if(!dir.exists(odir)) {
    dir.create(odir, showWarnings = FALSE, recursive = TRUE)
  } else {
    message(odir, " already exists. Skipping dir creation...\n")
  }

## 1. Get the CMS data catalog of dataset names -------------------------------

  catalogDF <- grabCatalog(odir = odir, 
                           baseURL = baseURL)
  
## 2. Find the requested dataset names in the catalog ------------------------
## This is to ensure we can actually grab all the data requested, or to alert
## user to need for troubleshooting; uses map() function to enable looping across vector
  datasetMatches <- map(datasetNames, 
                        findTitle,
                        catalogDF = catalogDF)
  names(datasetMatches) <- datasetNames
  datasetMatches <- compact(datasetMatches)  
 
  ## Final error handling; if no matches, let the user know. otherwise... let the
  ## user know ;)
  if (length(datasetMatches) == 0L) {
    stop("No dataset titles matched. Check full-CMS-catalog.csv.")
  } else {
    cat("Found all expected datasets:\n\n")
    print(datasetNames)
  }

## 3. Download files from JSON endpoint --------------------------------------

## Recall that what it's doing is retrieve a single page to ensure connection 
## then iterating over all rows, 1500 at a time, to create a paginated report. 
## Function then collates

  ## Initialize a list to store the manifest of what we managed to download
  manifest <- list()

  ## Initialize another list of all the data tables we download so we don't have
  ## to read them back in again as CSVs:
  cmsData <- list()
  
  ## Loop over all the titles in the datasets we found
  for (requestedTitle in names(datasetMatches)) {
      ## Grab the metadata, id number, and title
      metadata <- datasetMatches[[requestedTitle]]
      id <- metadata$identifier[[1]]
      cmsTitle <- metadata$title[[1]]
    
      ## Update user of status
      message("\nDownloading: ", cmsTitle)
    
      ## Connect to the page and ensure validity
      dat <- getCMSdataset(
        id = id,
        title = cmsTitle,
        pgSize = pgSize
      )
      
      ## Standardize column names only
      dat <- clean_names(dat)
    
      ## Create a file stub for the name of the raw CSV that we will export
      fileStub <- cmsTitle %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "-") %>%
        str_replace_all("^_|_$", "")
      
      ## Now write the CSV to the outdir:
      ofile <- file.path(odir, paste0(fileStub, ".csv"))
      write_csv(dat, ofile, na = "")
  
      ## Add the dataset to the cmsData list:
      cmsData[[cmsTitle]] <- dat
      
      ## And then we also want to write a manifest of everything that was done:
      manifest[[length(manifest) + 1L]] <- tibble(
        requestedTitle = requestedTitle,
        cmsTitle = cmsTitle,
        id = id,
        retrievalDate = as.character(Sys.Date()),  ## good for scientific doc
        rowsDownloaded = nrow(dat),
        columnsDownloaded = ncol(dat),
        outfile = ofile
      )
    
      message("Saved ", nrow(dat), " rows to: ", ofile)
  }

  ## Write to the manifest so we have a log of what was accomplished
  manifest <- bind_rows(manifest)
  ## And write the CSV file
  write_csv(manifest, 
            file.path(odir, "cms-retrieval-manifest.csv"))
  
  ## Final status messages
  message("\nDone. Files saved in: ", normalizePath(odir))
  ## Print the manifest; final product(s) are in the output files
  print(manifest)

  ## Return the list with all the dataframes
  return(cmsData)
}

## Example to run:
cmsData <- rawCMS()
str(cmsData)

## ------------------------------------------------------------------------- ##
## Next step:
##   Use the saved CSV files in the output directory as inputs to the 
##   data cleaning and joining module, cleanJoinCMS.R.
## ------------------------------------------------------------------------- ##


  

  
  