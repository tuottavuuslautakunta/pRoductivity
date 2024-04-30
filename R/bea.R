#' Fetch Data from BEA API
#'
#' This function retrieves data from the BEA API based on the provided parameters.
#' It handles the request, checks the response status, and parses the JSON content
#' into a data frame.
#'
#' @param params List of parameters for the BEA API request.
#'               This should include at least `UserID`, `method`, `datasetname`,
#'               `KeyCode`, `GeoFips`, `Year`, and `ResultFormat`.
#'
#' @return A data frame containing the requested data if the API call is successful.
#'         If the call fails, it will print an error message and return NULL.
#'
#' @examples
#' params <- list(
#'   UserID = "YourAPIKeyHere",
#'   method = "GETDATA",
#'   datasetname = "RegionalData",
#'   KeyCode = "PCPI_CI",
#'   GeoFips = "STATE",
#'   Year = "2020",
#'   ResultFormat = "JSON"
#' )
#' data <- get_bea(params)
#'
#' @export
get_bea <- function(params){

  url <- "https://apps.bea.gov/api/data"  # BEA API endpoint

  response <- httr::GET(url, query = params)

  if (httr::status_code(response) == 200) {  # HTTP status 200 means OK
    data <- httr::content(response, "text")
    json_data <- jsonlite::fromJSON(data)  # Convert JSON response to an R object
    dat <- as.data.frame(json_data$BEAAPI$Results$Data[[1]])
    return(dat)
  } else {
    message(paste("Failed to fetch data. Status code:", httr::status_code(response)))
    return(NULL)
  }
}
