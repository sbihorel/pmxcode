
covariates_ui <- function()
{
  bslib:: nav_panel(
    title = "Covariates",
    icon = icon("people-group", verify_fa = FALSE),
    value = "cov",
    wellPanel(
      h4( icon('person-digging', verify_fa = FALSE), 'Work in progress')
    )
  )
}
