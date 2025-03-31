#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    rclipboard::rclipboardSetup(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    suppressWarnings(
      bslib::page_navbar(
        title = "PMXcode",
        theme = bslib::bs_theme_update(
          theme = bslib::bs_theme(bootswatch = "sandstone", version = 5),
          base_font = bslib::font_collection("Helvetica Neue", "Arial", "sans-serif"),
          font_scale = 0.85
        ),
        bg = "#3e3f3a",
        htmltools::tags$head(
          htmltools::tags$style(
            htmltools::HTML(
              paste(
                "@import url(https://use.fontawesome.com/releases/v6.4.2/css/all.css);",
                ".btn.active{",
                "  background-color: #c96674;",
                "}",
                ".btn.active:hover{",
                "  background-color: #c96674;",
                "}",
                ".small-box h3 {",
                "  font-size: 35px;",
                "  margin: 0 0 0 0;",
                "}",
                ".small-box .icon-large {",
                "  font-size: 60px;",
                "}",
                ".handsontable th, .handsontable td {",
                "  white-space: nowrap !important;",
                "}",
                ".control-label {",
                "  font-weight: 700 !important;",
                "}",
                ".inline label{",
                "  display: table-cell;",
                "  text-align: center;",
                "  vertical-align: middle;",
                "  padding-right: 20px;",
                "}",
                ".inline .form-group {",
                "  display: table-row;",
                "}",
                ".inline2 label{",
                "  display: table-cell;",
                "  text-align: center;",
                "  vertical-align: middle;",
                "  padding-right: 20px;",
                "  min-width: 120px;",
                "}",
                ".inline2 .form-group {",
                "  display: table-row;",
                "}",
                ".tooltip {",
                "  text-transform: none !important;",
                "}",
                sep = "\n"
              )
            )
          )
        ),
        new_model_ui(),
        covariates_ui(),
        help_ui(),
        header = NULL,
        footer = NULL
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @import markdown
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pmxcode"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
