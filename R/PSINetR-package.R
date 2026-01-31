#' @keywords internal
"_PACKAGE"

#' @importFrom utils data
NULL

# Suppress R CMD check NOTEs about dplyr NSE
utils::globalVariables(c(
  ".data",
  "plot_treatment_id",
  "individual_treatment_id",
  "plot_id",
  "individual_id",
  "sensor_id",
  "start_date",
  "end_date"
))