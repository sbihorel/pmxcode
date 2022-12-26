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

  # New model module
  new_model_server(
    session = session,
    input = input,
    output = output,
    resources = resources
  )

}
