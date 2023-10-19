help_ui <- function()
{
  tabPanel(
    title = "Info",
    icon = icon("question", verify_fa = FALSE),
    value = "info",
    includeMarkdown(system.file("resources/info.md", package = "pmxcode"))
  )
}

