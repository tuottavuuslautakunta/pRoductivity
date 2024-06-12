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
#'   datasetname = "GDPbyIndustry",
#'   Frequency = "A",
#'   Industry = "31G",
#'   TableID = "1",
#'   Year = "ALL"
#' )
#' bea_data <- bea_get_data(params)
#'
#' @export
bea_get_data <- function(params, api_key = NULL){

  params <-
    c(
      Method = "GetData",
      params)

  dat <- bea_response(api_key = api_key, params)

  dat
}

#' Get Available Datasets from BEA API
#'
#' Retrieves a list of datasets available from the BEA API. It constructs a query with minimal parameters to fetch dataset names.
#'
#' @param api_key Your API key for the BEA API. If NULL, the function will attempt to retrieve the API key from the environment variable BEA_API_KEY.
#' @return A data frame listing all datasets available from the BEA API.
#' @examples
#' datasets <- bea_datasets()
#' @export
bea_datasets <- function(api_key = NULL){

  params <-
    list(
      Method = 'GetDataSetList',
      ResultFormat = 'json')

  dat <- bea_response(api_key = api_key, params)

  names(dat)[1] <- "Data_sets"
  dat
}

#' Get Parameters for a Specific BEA Dataset
#'
#' Fetches and lists parameters available for a specific dataset from the BEA API, which are necessary for constructing detailed queries.
#'
#' @param dataset The name of the dataset for which parameters are requested.
#' @param api_key Your API key for the BEA API. If NULL, the function will attempt to retrieve the API key from  the environment variable BEA_API_KEY.
#' @return A data frame containing the parameters for the specified dataset.
#' @examples
#' parameters <- bea_parameters("GDPbyIndustry")
#' @export
bea_parameters <- function(dataset, api_key = NULL){

  params <-
    list(
      Method = 'GetParameterList',
      DatasetName = dataset,
      ResultFormat = 'json')

  dat <- bea_response(api_key = api_key, params, result = "parameter")

  # names(dat)[1] <- "Data_sets"
  dat
}


#' Get Parameter values for a Specific BEA Dataset
#'
#' Fetches and lists parameters available for a specific dataset from the BEA API, which are necessary for constructing detailed queries.
#'
#' @param dataset The name of the dataset for which parameter values are requested.
#' @param parameter The name of the parameter for which values are requested.
#' @param api_key Your API key for the BEA API. If NULL, the function will attempt to retrieve the API key from  the environment variable BEA_API_KEY.
#' @return A data frame containing the parameters for the specified dataset.
#' @examples
#' parameter_values <- bea_parameter_values(dataset = "GDPbyIndustry", parameter = "TableID")
#' @export
bea_parameter_values <- function(dataset, parameter, api_key = NULL){

  params <-
    list(
      Method = 'GetParameterValues',
      DatasetName = dataset,
      ParameterName = parameter,
      ResultFormat = 'json')

  dat <- bea_response(api_key = api_key, params, result = "parameter_values")

  # names(dat)[1] <- "Data_sets"
  dat
}

#' Internal function to formulate queries and handle responses from BEA API
#'
#' Constructs the full API request URL, sends the GET request, and processes the response based on the specified result type.
#'
#' @param api_key The API key used for the BEA API, defaults to an environment variable if not specified.
#' @param params A list of parameters for the API call.
#' @param result The type of result expected ("data" for data frames, "parameter" for parameter lists).
#' @return Data frame of the requested data or parameters, depending on the result type specified.
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @noRd
bea_response <- function(api_key = NULL, params, result = "data"){
  url <- "https://apps.bea.gov/api/data"  # BEA API endpoint

  if (!is.list(params)) stop("The params -argument should be a list")

  if(is.null(api_key)){
    api_key <- Sys.getenv("BEA_API_KEY")
  }

  params <-
    c(
      UserID = api_key,
      params
    )

  response <- httr::GET(url, query = params)

  if (httr::status_code(response) == 200) {  # HTTP status 200 means OK
    data <- httr::content(response, "text", encoding = "UTF-8")
    json_data <- jsonlite::fromJSON(data)  # Convert JSON response to an R object
    if (result == "data"){
      dat <- as.data.frame(json_data$BEAAPI$Results$Data[[1]])
    } else if (result == "parameter") {
      dat <- as.data.frame(json_data$BEAAPI$Results$Parameter)
    } else if (result == "parameter_values") {
      dat <- as.data.frame(json_data$BEAAPI$Results$ParamValue)
    } else {
      stop("Unknown result type.")
    }

  } else {
    stop(paste("Failed to fetch data. Status code:", httr::status_code(response)))
  }

  dat
}
