#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data
#' @noRd
app_server <- function(input, output, session) {

  # Load reference libraries
  resources <- resources()

  # Make nmtran ACE mode available
  # acePath <- installed.packages() %>%
  #   as.data.frame() %>%
  #   dplyr::filter(Package == "shinyAce") %>%
  #   dplyr::pull(LibPath)
  # acePath <- file.path(acePath, "shinyAce/www/ace/")
  # if ( !file.exists(file.path(acePath, "mode-nmtran.js")) ){
  #   message(file.access(acePath, 2))
  #   file.copy(
  #     from = "./inst/resources/mode-nmtran.js",
  #     to = file.path(acePath, "mode-nmtran.js")
  #   )
  # }

  # New model module
  new_model_server(
    session = session,
    input = input,
    output = output,
    resources = resources
  )

  # Covariate module
  covariates_server(
    session = session,
    input = input,
    output = output,
    resources = resources
  )
}
