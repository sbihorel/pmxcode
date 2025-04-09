
covariates_ui <- function()
{
  bslib:: nav_panel(
    title = "Covariates",
    icon = icon("people-group", verify_fa = FALSE),
    value = "cov",
    # wellPanel(
    #   h4( icon('person-digging', verify_fa = FALSE), 'Work in progress')
    # ),
    bslib::navset_pill_list(
      id = "cov-menu",
      widths = c(2, 10),
      #---- Reference model ----
      bslib::nav_panel(
        title = "Reference model",
        wellPanel(
          fluidRow(
            col_6( uiOutput("referenceFileUI") ),
            col_6(
              fluidRow(
                col_12(
                  uiOutput("convertBtnUI", style = 'display: inline-block;'),
                  uiOutput("downloadConvertedBtnUI", style = 'display: inline-block;')
                )
              )
            )
          ),
          fluidRow(
            col_6(
              fluidRow(
                col_3( uiOutput("nthetaBox") ),
                col_9( uiOutput("referenceBoxes") )
              )
            ),
            col_6( uiOutput("convertedBoxes") )
          ),
          fluidRow(
            col_6( uiOutput("aceReferenceUI") ),
            col_6( uiOutput("aceConvertUI") )
          )
        )
      ),

      #---- Covariates ----
      bslib::nav_panel(
        title = "Covariate effects",
        fluidRow(
          col_12(
            uiOutput("covariateBoxUI")
          )
        )
      ),
      #---- Univariate models ----
      bslib::nav_panel(
        title = "Univariate models",
        fluidRow(
          col_12(
            uiOutput("univariateCreateUI")
          )
        )
      )
    )
  )
}
