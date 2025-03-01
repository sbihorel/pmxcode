#' Hide or display a tag
#'
#' @param tag the tag
#'
#' @return a tag
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) &&
      !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;",
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) &&
      grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#'
#' @param id the id of the element to hide
#'
#' @noRd
#'
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}


#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...) {
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}


#' @importFrom shiny column
col_7 <- function(...) {
  column(7, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}

#' @importFrom shiny column
col_5 <- function(...) {
  column(5, ...)
}

#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}

#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}

#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}

#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}


HTML_info <- function(info){
  HTML(
    paste(
      icon("circle-info", verify_fa = FALSE),
      paste(info, collapse = " ")
    )
  )
}

message_box <- function( text, icon, theme ){

  tagText <- tagList()
  for ( itext in seq_along(text) ){
    tagText <- c(
      tagText,
      tagList(
        strong(text[itext]),
        br()
      )
    )
  }

  bslib::card(
    class = paste("bg", theme, sep = "-"),
    p(
      bsicons::bs_icon( icon ),
      HTML( "&nbsp;" ),
      tagText
    )
  )

}
