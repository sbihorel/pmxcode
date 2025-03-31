#' Creation of model code
#'
#' @param input Internal parameter for \code{shiny}
#' @param template Text template
#' @param vars Reactive object - List of variables in data file
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isODE Reactive object - is model coded with ODEs?
#' @param isLINMAT Reactive object - is model coded as linear matrix?
#' @param isPREDPP Reactive object - is mode coded with $PK?
#' @param varianceTable Variance- table
#' @param covarianceBlock Variance-covariance matrix
#' @param rvTable  Reactive object - residual variability matrix
#' @param parm_lib Library of parameters
#' @param model_lib Library for $MODEL replacement
#' @param rv_lib  Library for residual variability replacement
#' @param scaling  Library for scaling
#' @param replacement Logical value indicating with replacement is required
#'
#' @noRd

get_code <- function(
    input = NULL,
    template = NULL,
    vars,
    advan,
    trans,
    isPRED,
    isODE,
    isLINMAT,
    isPREDPP,
    varianceTable,
    covarianceBlock,
    rvTable,
    parm_lib,
    model_lib,
    rv_lib,
    scaling,
    replacement = TRUE
) {

  if (input$platformInput == "NONMEM") {
    return(
      get_nonmem_code(
        input = input,
        template = template(),
        vars = vars,
        advan = advan,
        trans = trans,
        isPRED = isPRED,
        isODE = isODE,
        isLINMAT = isLINMAT,
        isPREDPP = isPREDPP,
        varianceTable = varianceTable,
        covarianceBlock = covarianceBlock,
        rvTable = rvTable,
        parm_lib = parm_lib,
        model_lib = model_lib,
        rv_lib = rv_lib,
        scaling = scaling,
        replacement = replacement
      )
    )
  } else if (input$platformInput == "mrgsolve") {
    return(
      get_mrgsolve_code(
        input = input,
        template = template(),
        advan = advan,
        trans = trans,
        isPRED = isPRED,
        isODE = isODE,
        isLINMAT = isLINMAT,
        isPREDPP = isPREDPP,
        varianceTable = varianceTable,
        covarianceBlock = covarianceBlock,
        rvTable = rvTable,
        parm_lib = parm_lib,
        model_lib = model_lib,
        rv_lib = rv_lib,
        scaling = scaling,
        replacement = replacement
      )
    )
  } else {
    # return(
    #   make_bm_model(input = input,
    #                 template = bm_template,
    #                 user = username,
    #                 date = date,
    #                 vars = final.vars,
    #                 advan = advan,
    #                 trans = trans,
    #                 isPRED = isPRED,
    #                 isODE = is_mrg_ODE,
    #                 isLINMAT = isLINMAT,
    #                 covTable = covCheck3()$data,
    #                 rvTable = rvTable,
    #                 parm_lib = parm_lib,
    #                 model_replacement = model_replacement,
    #                 rv_ref = rv_ref,
    #                 replacement = as.logical(input$subInput)
    #   )
    # )
  }

}
