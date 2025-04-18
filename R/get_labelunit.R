#' Get parameter label or unit from parameter library
#'
#' @param input Internal parameter for \code{shiny}
#' @param parms A vector of parameters
#' @param labelunit_lib Library of parameter label and unit
#' @param what Either "label" or "unit"
#'
#' @noRd

get_labelunit <- function(
    input,
    parms,
    labelunit_lib,
    what = "label"
) {

  if ( !'effectStimInput' %in% names(input) ){
    tmp <- NA
  } else {
    tmp <- input$effectStimInput
  }

  labelunits <- dplyr::left_join(
    data.frame(
      PARM = parms
    ),
    labelunit_lib %>%
      dplyr::filter(
        .data$PARM %in% parms &
          (is.na(.data$INCREASE) | .data$INCREASE == as.integer(as.logical(tmp)) ) &
          (.data$FORM %in% c("", input$effectFormInput))
      ),
    by = "PARM"
  ) %>%
    dplyr::mutate(
      LABEL = ifelse(is.na(.data$LABEL), "", .data$LABEL),
      UNIT = ifelse(is.na(.data$UNIT), "", .data$UNIT)
    )

  labels <- labelunits$LABEL
  units <- labelunits$UNIT

  # Replace <ELIM>
  if ( isTruthy(input$kmScaleInput) ){
    labels <- gsub(
      "<ELIM>",
      ifelse(input$kmScaleInput, "Concentration", "Amount"),
      labels
    )
  }

  # Replace <DRIVER>
  if (input$pdInput %in% c("direct", "biophase", "idr")){
    labels <- gsub(
      "<DRIVER>",
      ifelse(input$effectCpDriverInput, "Concentration", "Amount"),
      labels
    )
  }
  if (input$pdInput == "er"){
    labels <- gsub(
      "<DRIVER>",
      ifelse(
        length(input$expvarTextInput) > 0 && input$expvarTextInput != "",
        input$expvarTextInput,
        "<exposure>"
      ),
      labels
    )
  }

  if ( what == "label" ){
    labels
  } else {
    units
  }

}
