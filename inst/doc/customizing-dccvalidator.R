## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----demo-check-function, message = FALSE--------------------------------
library("dccvalidator")
library("readr")

## Load a sample manifest
manifest <- read_tsv(
  system.file("extdata", "test_manifest.txt", package = "dccvalidator")
)

manifest

## Check that required columns are complete in the manifest
result <- check_cols_complete(
  manifest,
  required_cols = c("path", "parent", "grant")
)

result

## ----see-result-details--------------------------------------------------
result$message
result$behavior
result$data

## ----custom-function-----------------------------------------------------
dat <- data.frame(pH = c(2, 6, 3, -2, 0, 15))

check_values_ph <- function(data, ph_col = "pH",
                            success_msg = "All pH values are valid",
                            fail_msg = "Some pH values are outside the range 0-14",
                            behavior_msg = "pH values should be between 0-14") {
  values <- data[, ph_col, drop = TRUE]
  if (all(values >= 0 & values <= 14)) {
    check_pass(
      msg = success_msg,
      behavior = behavior_msg
    )
  } else {
    check_fail(
      msg = fail_msg,
      behavior = behavior_msg,
      data = values[values > 14 | values < 0]
    )
  }
}

check_values_ph(dat)

## ----check-condition-example---------------------------------------------
check_values_ph <- function(data, ph_col = "pH",
                            success_msg = "All pH values are valid",
                            fail_msg = "Some pH values are outside the range 0-14",
                            behavior_msg = "pH values should be between 0-14") {
  values <- na.omit(data[, ph_col, drop = TRUE])
  all_valid <- all(values >= 0 & values <= 14)
  
  check_condition(
    msg = ifelse(all_valid, success_msg, fail_msg),
    behavior = behavior_msg,
    data = if (!all_valid) values[values > 14 | values < 0],
    type = ifelse(all_valid, "check_pass", "check_fail")
  )
}

## ----show-module-demo, eval = FALSE--------------------------------------
#  library("shiny")
#  library("shinydashboard")
#  
#  server <- function(input, output) {
#    # Load sample data
#    manifest <- read_tsv(
#      system.file("extdata", "test_manifest.txt", package = "dccvalidator")
#    )
#    biosp <- read_csv(
#      system.file("extdata", "test_biospecimen.csv", package = "dccvalidator")
#    )
#  
#    # Add logic to run the checks you are interested in and store the results in a
#    # list. Here is an example:
#    res <- list(
#      check_cols_complete(
#        manifest,
#        c("path", "parent", "grant"),
#        success_msg = "All required columns present are complete in the manifest",
#        fail_msg = "Some required columns are incomplete in the manifest"
#      ),
#      check_cols_complete(
#        biosp,
#        "specimenID",
#        success_msg = "All required columns present are complete in the biospecimen metadata",
#        fail_msg = "Some required columns are incomplete in the biospecimen metadata"
#      ),
#      check_specimen_ids_dup(
#        biosp,
#        success_msg = "Specimen IDs in the biospecimen metadata file are unique",
#        fail_msg = "Duplicate specimen IDs found in the biospecimen metadata file"
#      )
#    )
#  
#    # Show results in boxes
#    callModule(results_boxes_server, "Validation Results", res)
#  }
#  
#  ui <- function(request) {
#    dashboardPage(
#      header = dashboardHeader(),
#      sidebar = dashboardSidebar(),
#      body = dashboardBody(
#        includeCSS(
#          system.file("app/www/custom.css", package = "dccvalidator")
#        ),
#        results_boxes_ui("Validation Results")
#      )
#    )
#  }
#  
#  shinyApp(ui, server)

