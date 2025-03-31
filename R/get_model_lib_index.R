#' Get model index in code library
#'
#' @param input Internal parameter for \code{shiny}
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param parm_lib Library of parameters
#'
#' @noRd

get_model_lib_index <- function(
    input,
    advan,
    trans,
    parm_lib
) {

  req( input$ivInput, input$poInput, input$pkCMTInput, input$eliminationInput )

  req( length(advan()) > 0 )

  if ( grepl('tmdd', input$eliminationInput) ){
    req(input$tmddInput)
  }

  absorptionInput <- sub(
    "none_|zero_|bolus_",
    "",
    sub(
      "_none",
      "",
      paste(input$ivInput, input$poInput, sep = "_")
    )
  )

  parm_lib %>%
    dplyr::filter(
      # CMT criteria
      .data$CMT == as.numeric( sub('cmt', '', input$pkCMTInput) ) &
        .data$ABSORPTION == switch(
          absorptionInput,
          'bolus' = 'bolus_zero',
          'zero' = 'bolus_zero',
          'first' = 'first_sig',
          'sig' = 'first_sig',
          'transit'
        ) &
        # Elimination criteria
        .data$ELIMINATION == input$eliminationInput &
        # ADVAN criteria
        .data$ADVAN == ifelse(
          is.na(advan()),
          '.',
          ifelse(
            advan() %in% c(6, 8, 9, 13, 14, 15),
            '6/8/9/13/14/15',
            as.character(advan())
          )
        ) &
        # TRANS criteria
        .data$TRANS == ifelse(
          grepl('tmdd', input$eliminationInput),
          input$tmddInput,
          trans()
        )
    ) %>%
    dplyr::pull(.data$N)

}
