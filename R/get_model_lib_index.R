get_model_lib_index <- function(
    input,
    advan,
    trans,
    parm_lib
) {

  parm_lib %>%
    dplyr::filter(
      # CMT criteria
      CMT == as.numeric( sub('cmt', '', input$pkCMTInput) ) &
        ABSORPTION == switch(
          input$absorptionInput,
          'bolus' = 'bolus_zero',
          'zero' = 'bolus_zero',
          'first' = 'first_sig',
          'sig' = 'first_sig',
          'transit'
        ) &
        # Elimination criteria
        ELIMINATION == input$eliminationInput &
        # ADVAN criteria
        ADVAN == ifelse(
          is.na(advan()),
          '.',
          ifelse(
            advan() %in% c(6, 8, 9, 13, 14, 15),
            '6/8/9/13/14/15',
            as.character(advan())
          )
        ) &
        # TRANS criteria
        TRANS == ifelse(
          grepl('tmdd', input$eliminationInput),
          input$tmddParmInput,
          trans()
        )
    ) %>%
    dplyr::pull(N)

}
