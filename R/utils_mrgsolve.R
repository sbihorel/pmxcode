
#' Creation of mrgsolve code
#'
#' @param input Internal parameter for {shiny}
#' @param template Text template
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

get_mrgsolve_code <- function(
    input = NULL,
    template = NULL,
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
    replacement = TRUE)
{

  new <- template

  if (!replacement){
    return(new)
  }

  user <- Sys.info()["user"]
  date <- format(Sys.time(), "%b %d, %Y %H:%M:%S %Z")

  # Replace @TIMESTAMP
  new <- sub("@TIMESTAMP", date, new)

  # Replace @USER
  new <- sub("@USER", user, new)

  # Replace @VERSION
  new <- sub("@VERSION", input$platformVersionInput, new)

  # Replace @PURPOSE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_purpose(input = input, new = new, varianceTable = varianceTable)
  }

  # Replace @PATH
  new <- replace_path(input = input, new = new)

  # Replace @NMEXT
  if ( isTruthy(input$nmextInput) && input$nmextInput == "Yes" ){
    if ( areTruthy(input$nmextDirChoose, "path" %in% names(input$nmextDirChoose)) ){
      nmextImport <- TRUE
    } else {
      nmextImport <- FALSE
    }
  } else {
    nmextImport <- FALSE
  }

  new <- replace_nmext(input = input, new = new, nmextImport = nmextImport)

  # Replace @PLUGIN
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_plugin(new = new, input = input)
  }

  # Determine the number of compartments for PK and PD components
  if ( areTruthy(input$pkInput, input$pdInput) ){
    ncmts <- get_ncmts(
      input = input,
      model_lib = model_lib,
      isPRED = isPRED,
      isPREDPP = isPREDPP
    )
    nPKcmts <- ncmts[1]
    nPDcmts <- ncmts[2]
  }

  # Replace @GLOBAL
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_global(
      input = input,
      new = new,
      advan = advan,
      trans = trans,
      nPKcmts = nPKcmts,
      parm_lib = parm_lib
    )
  }

  # Replace @CMT
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_cmt(
      input = input,
      new = new,
      model_lib = model_lib,
      isPRED = isPRED
    )
  }

  # Extract tables content
  if ( areTruthy(input$parameterTable, input$varianceTable) ){
    parms <- hot_to_r(input$parameterTable)
    variance <- hot_to_r(input$varianceTable)

    req( identical(parms$Parameter, variance$Parameter) )

    parms <- dplyr::bind_cols(
      parms %>% dplyr::select(-.data$Variability),
      variance %>% dplyr::select(.data$Variability)
    )
  } else {
    parms <- NULL
  }
  estimations <- hot_to_r(input$estimationTable)

  # Replace @PARAM
  if ( isTruthy(parms) ){
    new <- replace_mrg_param(
      input = input,
      new = new,
      parms = parms,
      nmextImport = nmextImport
    )
  }

  # Replace @OMEGA
  if ( areTruthy(parms, covarianceBlock) ){
    new <- replace_mrg_omega(
      new = new,
      parms = parms,
      blocks = covarianceBlock,
      nmextImport = nmextImport
    )
  }

  # Replace @SIGMA
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_sigma(
      new = new,
      input = input,
      rvTable = rvTable,
      nmextImport = nmextImport
    )
  }

  # Create lines of preamble code
  if ( areTruthy(input$pkInput, input$pdInput) ){
    preamble_code <- get_preamble_code(
      input = input,
      parms = parms,
      vars = NULL
    )
  }

  # Create code lines for PK, PD, and other parameters
  if ( areTruthy(input$pkInput, input$pdInput, varianceTable) ){
    parms_code <- get_mrg_parms_code(
      input = input,
      parms = parms,
      mu = as.logical(input$muInput)
    )
  }

  # Create code lines for derived parameters
  if ( areTruthy(input$pkInput, input$pdInput) ){
    derived_parms_code <- get_derived_parms_code(
      input = input,
      advan = advan,
      trans = trans,
      isPRED = isPRED,
      isODE = isODE,
      isLINMAT = isLINMAT,
      parms = parms,
      parm_lib = parm_lib
    )
  }

  # Create code lines for dose scaling and bioavailability
  if ( areTruthy(input$pkInput, input$pdInput) ){
    scaling_code <- get_scaling_code(
      input = input,
      advan = advan,
      trans = trans,
      parm_lib = parm_lib,
      scaling = scaling
    )
  }

  # Create code lines for compartment initialization
  if ( areTruthy(input$pkInput, input$pdInput) ){
    init_code <- get_init_code(
      input = input,
      advan = advan,
      trans = trans,
      nPKcmts = nPKcmts,
      nPDcmts = nPDcmts,
      parm_lib = parm_lib
    )
  }

  # Replace @PRED or @MAIN
  if ( areTruthy(input$pkInput, input$pdInput, varianceTable) &
       (isTruthy(input$pkInput) | isTruthy(input$pdInput)) ){
    new <- replace_mrg_main_pred(
      input = input,
      new = new,
      preamble_code = preamble_code,
      parms_code = parms_code,
      derived_parms_code = derived_parms_code,
      scaling_code = scaling_code,
      init_code = init_code,
      isPRED = isPRED,
      parms = parms,
      parm_lib = parm_lib,
      rv_lib = rv_lib
    )
  }

  # Replace @DES
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_ode(
      input = input,
      new = new,
      advan = advan,
      trans = trans,
      isODE = isODE,
      nPKcmts = nPKcmts,
      nPDcmts = nPDcmts,
      parm_lib = parm_lib
    )
  }

  # Replace @TABLE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_table(
      input = input,
      new = new,
      advan = advan,
      trans = trans,
      isPRED = isPRED,
      nPKcmts = nPKcmts,
      nPDcmts = nPDcmts,
      parm_lib = parm_lib,
      rv_lib = rv_lib
    )
  }

  # Replace @CAPTURE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_mrg_capture(
      input = input,
      new = new,
      parms = parms
    )
  }

  dummy <- input$refreshButton

  # Remove all tags
  new[ !grepl("@[A-Z]+", new)]

}

#' Replacement of @NMEXT tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param nmextImport A logical indicating whether NONMEM ext file content
#' should be imported

replace_nmext <- function(
    input,
    new,
    nmextImport = FALSE
){

  if ( nmextImport ){

    if ( areTruthy(input$nmextDirChoose, "path" %in% names(input$nmextDirChoose)) ){
      path <- normalizePath(
        shinyFiles::parseDirPath(c(root = "/"), input$nmextDirChoose)
      )
    } else {
      return( new[ !grepl("@NMEXT", new) ] )
    }

    path_elements <- unlist(strsplit(path, .Platform$file.sep))

    run <- path_elements[length(path_elements)]
    project <- paste(
      path_elements[-length(path_elements)],
      collapse = .Platform$file.sep
    )

    # run must use runXYZ notation
    if ( !grepl("^run[0-9]+$", run) ){
      return( new[ !grepl("@NMEXT", new) ] )
    }

    # If model directory was provided, set project as a relative path
    if ( areTruthy(input$modelDirChoose, "path" %in% names(input$modelDirChoose)) ){
      model_path <- normalizePath(
        shinyFiles::parseDirPath(c(root = "/"), input$modelDirChoose)
      )
      project <- xfun::relative_path(project, model_path)
    }

    sub(
      "@NMEXT",
      paste(
        "[ NMEXT ]",
        glue::glue("  run = \"{run}\""),
        glue::glue("  project = \"{project}\""),
        "  root = \"cppfile\"",
        "",
        "  labels = c(@LABEL)",
        "  slabels = c(@SLABEL)",
        "",
        sep = "\n"
      ),
      new
    )

  } else {

    return( new[ !grepl("@NMEXT", new) ] )

  }
}

#' Replacement of @PLUGIN tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template

replace_mrg_plugin <- function(
    input,
    new
){

  if ( !input$pdInput %in% c("logistic", "ordcat") ) {
    return( new[ !grepl("@PLUGIN", new) ] )
  }

  sub(
    "@PLUGIN",
    paste(
      "[ PLUGIN ]",
      "  Rcpp",
      "  ",
      sep = "\n"),
    new
  )

}


#' Replacement of @GLOBAL tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param nPKcmts Number of compartments for PK
#' @param parm_lib Library of parameters
#'

replace_mrg_global <- function(
    input,
    new,
    advan,
    trans,
    nPKcmts,
    parm_lib)
{

  tmp <- c()

  # Add define lines
  if ( !input$pkInput %in% c("none", "pred") ) {

    req( input$eliminationInput, input$ivInput )

    tmp <- c(
      tmp,
      glue::glue(
        "  # define {conc} (A{cmt}/SCALE)",
        conc = dplyr::case_when(
          input$eliminationInput == "tmdd" ~ "CFREE",
          grepl("tmddq", input$eliminationInput) ~ "CTOT",
          TRUE ~ "CP"
        ),
        cmt = ifelse(
          input$pkInput == "pk",
          ifelse(input$ivInput %in% c("bolus", "zero"), 1, 2),
          req(input$pkDefaultObsInput)
        )
      )
    )
  }


  # Add global variables for transit absorption model
  if ( input$pkInput == "pk" && input$poInput == "transit" ) {
    tmp <- c(
      tmp,
      "  double DOSETIME [99];",
      "  double DOSEAMT [99];"
    )
  }

  if ( input$pdInput %in% c("direct", "biophase", "idr") ) {
    tmp <- c(
      tmp,
      glue::glue(
        "  # define DRIVER (A{cmt}{driver})",
        cmt = input$effectCmtDriverInput,
        driver = ifelse(
          input$effectCpDriverInput,
          "/SCALE",
          ""
        )
      )
    )
    if ( input$pdInput == "idr" ){
      tmp <- c(
        tmp,
        glue::glue("  # define RESPONSE (A{nPKcmts + 1})")
      )
    }
  } else if ( input$pdInput == "ode" ) {
    tmp <- c(
      tmp,
      glue::glue("  # define RESPONSE (A{nPKcmts + 1})")
    )
  } else if ( input$pdInput %in% c("er", "logistic", "ordcat") ) {
    tmp <- c(
      tmp,
      "  # define EXPOSURE (TIME)"
    )
  }

  if ( length(tmp) == 0 ){
    tmp <- new[ !grepl("@GLOBAL", new) ]
  } else {
    tmp <- sub(
      "@GLOBAL",
      paste(
        "[ GLOBAL ]",
        paste( c(tmp, "  "), collapse = "\n" ),
        sep = "\n"
      ),
      new
    )
  }

  tmp

}

#' Replacement of @CMT tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param model_lib Library for $MODEL replacement
#' @param isPRED Reactive object - is model coded with $PRED?

replace_mrg_cmt <- function(
    input,
    new,
    model_lib,
    isPRED
){

  if ( isPRED() ) {
    return( new[ !grepl("@CMT", new) ] )
  }

  # Get PK compartments
  if ( input$pkInput == "pk" ) {
    tmp <- model_lib %>%
      dplyr::filter(
        .data$CMT == input$pkCMTInput &
          .data$ABSORPTION == sub(
            "none_|zero_|bolus_",
            "",
            sub(
              "_none",
              "",
              paste(input$ivInput, input$poInput, sep = "_")
            )
          )
      )

    if (grepl("tmdd", input$eliminationInput)) {
      tmp <- tmp %>%
        dplyr::filter(.data$ELIMINATION == input$eliminationInput)
    } else {
      tmp <- tmp %>%
        dplyr::filter(.data$ELIMINATION == "mmlin")
    }
    tmp <- unlist(strsplit(tmp$MRGSOLVE, split = "[|]"))
    nPKcmts <- length(tmp)
  } else {
    if ( input$pkInput != "none" ) {
      nPKcmts <- input$pknCMTInput
    } else {
      nPKcmts <- 0
    }
    tmp <- NULL
  }

  # Get PD compartments
  if ( input$pdInput %in% c("biophase", "idr") ){
    nPDcmts <- 1
  } else if ( input$pdInput == "ode" ){
    nPDcmts <- input$pdnCMTInput
  } else {
    nPDcmts <- 0
  }

  # Initiate of [ CMT ] statements
  tmp <- c( "[ CMT ] @annotated", tmp )

  # Add PK compartments if pkInput is not "pk"
  if ( input$pkInput != "pk" & nPKcmts > 0 ){
    for ( iCMT in 1:nPKcmts ){
      tmp <- c(
        tmp,
        glue::glue("  A{iCMT} : PK compartment {iCMT}")
      )
    }
  }

  # Add PD compartments
  if ( nPDcmts > 0 ){
    for ( iCMT in (nPKcmts + 1:nPDcmts) ){
      if ( input$pdInput == "biophase" ){
        tmp <- c( tmp, glue::glue("  A{iCMT} : Biophase") )
      } else if ( input$pdInput == "idr" ){
        tmp <- c( tmp, glue::glue("  A{iCMT} : Response") )
      } else {
        tmp <- c( tmp, glue::glue("  A{iCMT} : PD compartment {iCMT - nPKcmts}") )
      }
    }
  }

  sub(
    "@CMT",
    paste0(
      paste(tmp, collapse = "\n"),
      "\n"
    ),
    new
  )

}

#' Replacement of @PARAM tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param parms Parameter selection
#' @param nmextImport A logical indicating whether NONMEM ext file content
#' should be imported

replace_mrg_param <- function(
    input,
    new,
    parms,
    nmextImport
){

  if ( nmextImport ){
    return( new[ !grepl("@PARAM", new) ] )
  }

  tmp <- new

  nparms <- ifelse(
    is.data.frame(parms) && nrow(parms) > 0,
    nrow(parms),
    0
  )

  if ( nparms > 0 ){

    # Set default prefix based upon whether model includes IIV or not
    prefix <- ifelse(
      parms$Variability != "None",
      "TV",
      ""
    )

    # Set prefix for ordered categorical models
    if ( input$pdInput == "ordcat" ){
      prefix <- c(
        rep("T", input$maxCategoryInput - input$minCategoryInput),
        rep("TV", nparms - (input$maxCategoryInput - input$minCategoryInput))
      )
    }

    # Get value and label for every model parameters
    tmp <- "[ PARAM ] @annotated"
    for ( i in 1:nparms ){
      tmp <- c(
        tmp,
        glue::glue(
          "  {prefix}{parm} : {init} : {label}{unit}",
          prefix = prefix[i],
          parm = parms$Parameter[i],
          init = parms$Initial[i],
          label = ifelse(
            parms$Label[i] == "",
            glue::glue("Label for THETA{i}"),
            parms$Label[i]
          ),
          unit = ifelse(
            parms$Unit[i] == "",
            " (unitless)",
            glue::glue(" ({parms$Unit[i]})")
          )
        )
      )
    }
    tmp <- c(tmp, "")

    # Align tags
    tmp <- align_annotations(code = tmp)

    # Replace
    tmp <- sub(
      "@PARAM",
      paste(tmp, collapse = "\n"),
      new
    )
  }

  tmp

}

#' Replacement of @OMEGA tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param parms Parameter selection
#' @param blocks Variance - covariance matrix
#' @param nmextImport A logical indicating whether NONMEM ext file content
#' should be imported

replace_mrg_omega <- function(
    input,
    new,
    parms,
    blocks,
    nmextImport
){

  if ( nmextImport ){

    if ( is.data.frame(parms) && nrow(parms)>0 && !all(parms$Variability == "None") ){
      parms <- parms %>%
        dplyr::filter(.data$Variability != "None") %>%
        dplyr::pull(.data$Parameter) %>%
        as.character()
      new <- sub(
        "@LABEL",
        paste(
          sprintf("\"%s\"", parms),
          collapse = ", "
        ),
        new
      )
    }

    return( new[ !grepl("@OMEGA", new) ] )

  }

  if ( !(is.data.frame(parms) && nrow(parms) > 0 && !all(parms$Variability == "None")) ){
    return( new[ !grepl("@OMEGA", new) ] )
  }

  ieta <- 0
  tmp <- c()
  for ( iomega in 1:length(blocks) ){
    omega <- blocks[[iomega]]$omega

    req( all(rownames(omega) %in% parms$Parameter) )

    # Get type of current and previous omega block
    type <- blocks[[iomega]]$type
    previousType <- "notdiagonal"
    if (iomega >= 2){
      previousType <- blocks[[iomega - 1]]$type
    }

    # Add new empty line if necessary
    if ( iomega >= 2 & (previousType != "diagonal" | type != "diagonal") ){
      tmp <- c(tmp, "  ")
    }
    # Add $OMEGA line if necessary
    if ( type != "diagonal" ) {
      tmp <- c(tmp, "[ OMEGA ] @annotated @block")
    } else if (type == "diagonal" & previousType !="diagonal"){
      tmp <- c(tmp, "[ OMEGA ] @annotated")
    }

    # Add omega value and tag
    for ( i in 1:nrow(omega) ){
      ieta <- ieta + 1
      index <- which(parms$Parameter == rownames(omega)[i])
      tmp <- c(
        tmp,
        glue::glue(
          "  E{eta} : {init}: IIV in {parm} [{model}]",
          eta = parms$Parameter[index],
          init = ifelse(
            type == "diagonal",
            # Variability without correlation
            glue::glue("{omega[i, i]} "),
            # Variability with correlation
            paste(
              c(
                paste0(
                  as.character( omega[i, 1:i] ),
                  " "
                ),
                " "
              ),
              collapse = ""
            )
          ),
          parm = parms$Parameter[index],
          model = switch(
            levels(parms$Variability)[parms$Variability[index]],
            "Additive" = "add",
            "Exponential" = "exp",
            "Logit" = "logit"
          )
        )
      )
    }

  }

  tmp <- c(tmp, "  ")

  # Align tags
  tmp <- align_annotations(code = tmp)

  # Replace
  sub("@OMEGA", paste(tmp, collapse = "\n"), new)

}

#' Replacement of @SIGMA tag
#'
#' @param new Text template
#' @param input Internal parameter for {shiny}
#' @param rvTable Residual variability selection
#' @param nmextImport A logical indicating whether NONMEM ext file content
#' should be imported

replace_mrg_sigma <- function(
    input,
    new,
    rvTable,
    nmextImport
){

  if ( nmextImport ){

    slabel <- c()

    if ( isTruthy(input$pkRVInput) ) {
      slabel <- c(
        slabel,
        ifelse(input$pkRVInput %in% c("add", "log", "accv"), "PKADD", ""),
        ifelse(input$pkRVInput %in% c("ccv", "accv"), "PKCCV", "")
      )
    }
    if ( isTruthy(input$pdRVInput) ) {
      slabel <- c(
        slabel,
        ifelse(input$pdRVInput %in% c("add", "log", "accv"), "PDADD", ""),
        ifelse(input$pdRVInput %in% c("ccv", "accv"), "PDCCV", "")
      )
    }
    if ( length(slabel) > 0 ){
      slabel <- slabel [ slabel != "" ]
    }

    new <- sub(
      "@SLABEL",
      paste(
        sprintf("\"%s\"", slabel),
        collapse = ", "
      ),
      new
    )

    return( new[ !grepl("@SIGMA", new) ] )

  }

  tmp <- c()
  ieps <- 1

  # Pre-process rvTable
  if ( isTruthy(rvTable) ){
    rvTable$Variance <- ifelse(
      is.na(rvTable$Variance),
      0,
      rvTable$Variance
    )
  }

  # Get text replacement
  if ( input$pkInput != "none" ){

    req( input$pkRVInput )

    if ( input$pkRVInput %in% c("add", "log", "ccv") ){
      tmp <- c(
        tmp,
        glue::glue(
          "  PK{toupper(model)} : {init} : Residual variability {sup}[{model}]",
          model = ifelse(
            input$pkRVInput != "ccv",
            "add",
            "ccv"
          ),
          init = rvTable$Variance[ieps],
          sup = ifelse(input$pdInput != "none", "for PK ","")
        )
      )
      ieps <- ieps + 1
    } else if ( input$pkRVInput == "accv" ) {
      tmp <- c(
        tmp,
        glue::glue(
          "  PKCCV : {init} : Constant CV residual variability component {sup}[accv]",
          init = rvTable$Variance[ieps],
          sup = ifelse(input$pdInput != "none", "for PK ","")
        ),
        glue::glue(
          "  PKADD : {init} : Additive residual variability component {sup}[accv]",
          init = rvTable$Variance[ieps + 1],
          sup = ifelse(input$pdInput != "none", "for PK ","")
        )
      )
      ieps <- ieps + 2
    }
  }

  if ( input$pdInput != "none" ){

    req( input$pdRVInput )

    if ( input$pdRVInput %in% c("add", "log", "ccv") ){
      tmp <- c(
        tmp,
        glue::glue(
          "  PD{toupper(model)} : {init} : Residual variability {sup}[{model}]",
          model = ifelse(
            input$pdRVInput != "ccv",
            "add",
            "ccv"
          ),
          init = rvTable$Variance[ieps],
          sup = ifelse(input$pkInput != "none", "for PD ", "")
        )
      )
      ieps <- ieps + 1
    } else if ( input$pdRVInput == "accv" ) {
      tmp <- c(
        tmp,
        glue::glue(
          "  PDCCV : {init} : Constant CV residual variability component {sup}[accv]",
          init = rvTable$Variance[ieps],
          sup = ifelse(input$pkInput != "none", "for PD ", "")
        ),
        glue::glue(
          "  PDADD : {init} : Additive residual variability component {sup}[accv]",
          init = rvTable$Variance[ieps + 1],
          sup = ifelse(input$pkInput != "none", "for PD ", "")
        )
      )
      ieps <- ieps + 2
    }
  }

  if ( length(tmp) == 0 ){
    new <- new[ !grepl("@SIGMA", new) ]
  } else {
    # Align tags
    tmp <- align_annotations(code = tmp)

    # Replace
    new <- sub(
      "@SIGMA",
      paste0(
        paste(
          c("[ SIGMA ] @annotated", tmp),
          collapse = "\n"
        ),
        "\n"
      ),
      new
    )
  }

  new

}

#' Replace @MAIN or @PRED tags
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param preamble_code Preamble code
#' @param parms_code Typical and individual parameter code
#' @param derived_parms_code Derived parameter code
#' @param scaling_code Dose scaling and bioavailability code
#' @param init_code Compartment initialization code
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param parms Parameter selection
#' @param parm_lib Library of parameters
#' @param rv_lib  Library for residual variability replacement

replace_mrg_main_pred <- function(
    input,
    new,
    preamble_code,
    parms_code,
    derived_parms_code,
    scaling_code,
    init_code,
    isPRED,
    parms,
    parm_lib,
    rv_lib
){

  if ( isPRED() ){

    # Get rid of @MAIN tag if the model is defined by explicit solution
    new <- new[ !grepl("@MAIN", new) ]

    # Get preamble code
    tmp <- preamble_code

    # Get parameter definition
    tmp <- c(tmp, unlist(parms_code), "")

    isPK <- input$pkInput != "none"
    isPD <- input$pdInput != "none"

    # Get PK-specific code
    if ( isPK ){
      append <- ifelse( isPD, " for PK model", "")

      tmp <- c(
        tmp,
        ifelse(
          input$pkRVInput == "none",
          glue::glue("  // Model output{append}"),
          glue::glue("  // Model output and residual variability{append)")
        )
      )

      # Define model output
      tmpModel <- "  double CP = <Define the model output function>"

      # Extract RV model
      tmpRV <- rv_lib %>%
        dplyr::filter(.data$TYPE == input$pkRVInput) %>%
        dplyr::pull(.data$RV_MRGSOLVE) %>%
        strsplit(split = "[|]") %>%
        unlist()

      # Substitute DRIVER and ENDPOINT parameter indices
      tmpRV <- gsub("<DRIVER>", "CP", tmpRV)
      tmpRV <- gsub("<ENDPOINT>", "PK", tmpRV)

      tmp <- c(tmp, tmpModel, tmpRV, "  ")

    }

    # Get PD-specific code
    if ( isPD ){

      req( input$pdRVInput )

      append <- ifelse( isPK, " for PD model", "")

      if ( isPK | !input$pdInput %in% c("logistic", "ordcat") ){
        tmp <- c(
          tmp,
          ifelse(
            input$pdRVInput == "none",
            glue::glue("  // Model output{append}"),
            glue::glue("  // Model output and residual variability{append}")
          )
        )
      }

      # Extract RV model
      tmpRV <- rv_lib %>%
        dplyr::filter(.data$TYPE == input$pdRVInput) %>%
        dplyr::pull(.data$RV_MRGSOLVE) %>%
        strsplit(split = "[|]") %>%
        unlist()

      if ( input$pdInput == "er" ){

        req(
          areTruthy(
            input$effectFormInput, input$effectParmInput, input$effectStimInput
          )
        )

        # Include code for exposure-response
        tmpModel <- glue::glue(
          "  double RESPONSE = {pred};",
          pred = parm_lib %>%
            dplyr::filter(
              .data$TYPE == "function" &
                .data$FORM == input$effectFormInput &
                .data$TRANS == input$effectParmInput &
                .data$INCREASE == as.integer(as.logical(input$effectStimInput))
            ) %>%
            dplyr::pull(.data$MRG_PRED) %>%
            strsplit(split = "[|]") %>%
            unlist()
        )

        # Adjust the model driver in tmpModel
        tmpModel <- gsub("<x>", "EXPOSURE", tmpModel)

        # Adjust DRIVER and ENDPOINT in residual variability
        tmpRV <- gsub("<DRIVER>", "RESPONSE", tmpRV)
        tmpRV <- gsub("<ENDPOINT>", "PD", tmpRV)

        tmp <- c(tmp, tmpModel, tmpRV)

      } else if ( input$pdInput == "logistic" ) {

        # Get code for logistic regression
        tmpModel <- parm_lib %>%
          dplyr::filter(.data$TYPE == "logistic") %>%
          dplyr::pull(.data$MRG_PRED) %>%
          strsplit(split = "[|]") %>%
          unlist()

        # Find the drug effect function
        parm_lib_index <- parm_lib %>%
          dplyr::filter(
            .data$TYPE == "function" &
              .data$FORM == input$effectFormInput &
              .data$TRANS == input$effectParmInput &
              .data$INCREASE == ifelse(
                input$effectFormInput == "base",
                1,
                as.integer(as.logical(input$effectStimInput))
              )
          )
        logitFun <-  parm_lib_index %>% dplyr::pull(.data$MRG_PRED)

        # Find the baseline parameter to replace in logitFun
        tmpModelParm <- parm_lib %>%
          dplyr::filter(.data$TYPE == "logistic") %>%
          dplyr::pull(.data$PARMS)
        minimumParm <- parm_lib_index %>%
          dplyr::pull(.data$PARMS) %>%
          strsplit(split = "[|]") %>%
          unlist()
        minimumParm <- minimumParm[1]


        # Insert logistic regression code
        tmpModel <- sub("<LOGIT>", sub(minimumParm, tmpModelParm, logitFun), tmpModel)
        if ( isTruthy(input$logisticVarInput) ){
          tmpModel <- gsub("<x>", input$logisticVarInput, tmpModel)
        } else if ( isTruthy(input$logisticVarTextInput) ){
          tmpModel <- gsub("<x>", input$logisticVarTextInput, tmpModel)
        }
        tmpRV <- tmpModel

      } else if ( input$pdInput == "ordcat" ) {

        req(
          areTruthy(
            input$minCategoryInput, input$maxCategoryInput,
            input$effectFormInput, input$effectParmInput, input$effectStimInput
          )
        )

        minCategory <- min(
          floor( c(input$minCategoryInput, input$maxCategoryInput) )
        )
        maxCategory <- min(
          ceiling( c(input$minCategoryInput, input$maxCategoryInput) )
        )

        # Add code for ordered categorical model
        tmpModel <- parm_lib %>%
          dplyr::filter(.data$TYPE == "ordcat") %>%
          dplyr::pull(.data$MRG_PRED) %>%
          strsplit(split = "[|]") %>%
          unlist()

        # Find the drug effect function
        parm_lib_index <- parm_lib %>%
          dplyr::filter(
            .data$TYPE == "function" &
              .data$FORM == input$effectFormInput &
              .data$TRANS == input$effectParmInput &
              .data$INCREASE == ifelse(
                input$effectFormInput == "base",
                1,
                as.integer(as.logical(input$effectStimInput))
              )
          )
        logitFun <- parm_lib_index %>% dplyr::pull(.data$MRG_PRED)

        addFun <- gregexpr("[+]", logitFun)[[1]][1] < gregexpr("[*]", logitFun)[[1]][1]

        # Find the baseline parameter for replacement in logitFun
        minimumParm <- parm_lib_index %>%
          dplyr::pull(.data$PARMS) %>%
          strsplit(split = "[|]") %>%
          unlist()
        minimumParm <- minimumParm[1]

        # Define the effect function
        effect <- sub(minimumParm, "", logitFun)
        if ( addFun ){
          effect <- sub("^ [+] ", "", effect)
        } else {
          effect <- sub("^\\s*[*]\\s*[(]", "", effect)
          effect <- sub("[)]\\s*$", "", effect)
        }
        effect <- glue::glue("  double EFFECT = {effect};")

        # Create replacement code lines
        logit <- logit_indiv <- proba <- c()
        sim <- c(
          glue::glue(
            "  double {event} = {minCategory};",
            event = ifelse(
              input$endpointInput == "",
              "EVENT",
              input$endpointInput
            )
          ),
          "  double rnd = R::runif(0,1);"
        )

        for ( i in minCategory:maxCategory ) {
          if ( i < maxCategory ){
            logit <- c(
              logit,
              glue::glue(
                "  double TVLG{i} = TV{parm}{sign}EFFECT;",
                parm = parms$Parameter[i - minCategory + 1],
                sign = ifelse(addFun, " + ", "*")
              )
            )
            if ( i == minCategory ){
              logit_indiv <- c(
                logit_indiv,
                get_mrg_individual_parm_code(
                  parms = utils::modifyList(parms, list(Parameter = parms$SourceParam[1])),
                  iparm = 1,
                  eparm = parms$Parameter[1]
                )
              )
            }
            logit_indiv <- c(
              logit_indiv,
              get_mrg_individual_parm_code(
                parms =  utils::modifyList(parms, list(Parameter = paste0("LG", i))),
                iparm = 1,
                eparm = parms$Parameter[1]
              )
            )
            proba <- c(
              proba,
              glue::glue("  double P{i} = exp(LG{i})/(1 + exp(LG{i}))$  ; P(Y<={i}|X)")
            )
            sim <- c(
              sim,
              glue::glue(
                "  if (rnd > P{i}) {event} = {i+1};",
                event = ifelse(
                  input$endpointInput == "",
                  "EVENT",
                  input$endpointInput
                )
              )
            )
          }
        }

        # Align comments and replace ; by // in proba
        proba <- gsub(";", "//", align_annotations(code = proba))
        proba <- gsub("$", ";", proba)

        # Insert logistic regression code
        tmpModel <- sub("<EFFECT>", effect, tmpModel)
        tmpModel <- sub(
          "<LOGIT>",
          paste(c(logit, "  ", logit_indiv), collapse = "\n"),
          tmpModel
        )
        tmpModel <- sub("<PROBA>", paste(proba, collapse = "\n"), tmpModel)
        tmpModel <- sub("<SIM>", paste(sim, collapse = "\n"), tmpModel)
        tmpModel <- gsub("<x>", "EXPOSURE", tmpModel)
        tmpModel <- gsub(";;", ";", tmpModel)

        tmp <- c(tmp, tmpModel)

      } else {
        # Define model output
        tmpModel <- "  double RESPONSE = <Define the model output function>;"

        # Adjust DRIVER and ENDPOINT in residual variability
        tmpRV <- gsub("<DRIVER>", "RESPONSE", tmpRV)
        tmpRV <- gsub("<ENDPOINT>", "PD", tmpRV)

        tmp <- c(tmp, tmpModel, tmpRV)

      }

      tmp <- c(tmp, "  ")

    }

    tmp <- sub(
      "@PRED",
      paste(
        "[ PRED ]",
        paste(tmp, collapse = "\n"),
        sep = "\n"
      ),
      new
    )

  } else {
    new <- new[ !grepl("@PRED", new) ]
    tmp <- sub(
      "@MAIN",
      paste(
        "[ MAIN ]",
        paste(
          c(
            # Add preamble
            preamble_code,
            # Add parameter definition
            unlist(parms_code),
            # v derived parameter definition
            derived_parms_code,
            # Add scale and bioavailability definition
            scaling_code,
            # Add compartment initialization block
            init_code,
            ""
          ),
          collapse = "\n"
        ),
        sep = "\n"
      ),
      new
    )
  }

  tmp

}

#' Replacement of @ODE tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isODE Reactive object - is model coded with ODEs?
#' @param nPKcmts Number of PK compartments in the model
#' @param nPDcmts Number of PD compartments in the model
#' @param parm_lib Library of parameters

replace_mrg_ode <- function(
    input,
    new,
    advan,
    trans,
    isODE,
    nPKcmts,
    nPDcmts,
    parm_lib
){

  if ( !isODE() ){
    return( new[ !grepl("@ODE", new) ] )
  }

  tmp <- c()

  if ( !input$pkInput %in% c("none", "pred") ){

    if (input$pkInput == "pk"){

      # Get code from equivalent of NONMEM PREDPP models
      index <- get_model_lib_index(
        input = input, advan = advan, trans = trans, parm_lib = parm_lib
      )
      req( index )

      tmpModel <- unlist(
        strsplit(
          parm_lib[index, "MRG_ODE"],
          split = "[|]"
        )
      )

      if ( parm_lib[index, "ELIMINATION"] != "lin"){
        value <- ifelse(parm_lib[index, "ABSORPTION"] == "bolus_zero", 1, 2)
        if ( input$kmScaleInput ){
          tmpModel <- gsub(
            "<MM>",
            glue::glue("(A({value})/{parm_lib[index, 'VCENTRAL']})"),
            tmpModel
          )
        } else {
          tmpModel <- gsub(
            "<MM>",
            glue::glue("A({value})"),
            tmpModel
          )
        }

      }

      tmp <- c("  // PK model equations", tmp, tmpModel, "  ")

    } else {

      # Get code for models that are not equivalent of NONMEM PREDPP model
      ode <- c()
      for ( iCmt in 1:nPKcmts ){
        ode <- c(
          ode,
          glue::glue(
            "  dxdt_A{iCmt} = <Define ODE for A{iCmt}>"
          )
        )
      }
      tmp <- c("  // PK model equations", tmp, ode, "  ")

    }
  }

  if ( input$pdInput %in% c("idr", "ode", "biophase") ){

    if ( input$pdInput == "idr" ){

      req( input$idrTypeInput, input$idrParmInput, input$idrStimInput )

      # Get code for IDR and stimulation/inhibition models
      tmpModel <- parm_lib %>%
        dplyr::filter(
          .data$TYPE == "idr" &
            .data$FORM == input$idrTypeInput &
            .data$TRANS == input$idrParmInput
        ) %>%
        dplyr::pull(.data$MRG_ODE) %>%
        strsplit(split = "[|]") %>%
        unlist()
      tmpStim <- parm_lib %>%
        dplyr::filter(
          .data$TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
            .data$FORM == input$idrStimInput
        ) %>%
        dplyr::pull(.data$MRG_ODE) %>%
        strsplit(split = "[|]") %>%
        unlist()

      # Input compartment number into IDR model equation
      tmpModel <- gsub("<1>", nPKcmts + 1, tmpModel)

      tmp <- c(tmp, "  // PD model equations", tmpStim, tmpModel, "  ")

    } else if ( input$pdInput == "ode" ){

      # Get $DES code for custom ODE models
      ode <- c()
      for ( iCmt in (nPKcmts + 1:nPDcmts) ){
        ode <- c(
          ode,
          glue::glue("  dxdt_A{iCmt} = <Define ODE for A{iCmt}>")
        )
      }

      tmp <- c(tmp, "  // PD model equations", ode, "  ")

    } else if ( input$pdInput == "biophase" ){

      # Get ODE code for biophase models
      tmpModel <- parm_lib %>%
        dplyr::filter(.data$TYPE == "biophase") %>%
        dplyr::pull(.data$MRG_ODE) %>%
        strsplit(split = "[|]") %>%
        unlist()

      # Input compartment number into biophase model equation
      tmpModel <- gsub("<1>", nPKcmts + 1, tmpModel)
      if ( input$pkInput == "pk" ){
        tmpModel <- gsub(
          "<0>",
          ifelse(parm_lib[index, "ABSORPTION"] == "bolus_zero", 1, 2),
          tmpModel
        )
      } else {
        tmpModel <- gsub("<0>", input$pkDefaultObsInput, tmpModel)
      }

      tmp <- c(tmp, "  // Biophase equation", tmpModel, "  ")

    }

  }

  sub(
    "@ODE",
    paste(
      "[ ODE ]",
      paste(tmp, collapse = "\n"),
      sep = "\n"
    ),
    new
  )

}

#' Replacement of @TABLE tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param nPKcmts Number of PK compartments in the model
#' @param nPDcmts Number of PD compartments in the model
#' @param parm_lib Library of parameters
#' @param rv_lib  Library for residual variability replacement

replace_mrg_table <- function(
    input,
    new,
    advan,
    trans,
    isPRED,
    nPKcmts,
    nPDcmts,
    parm_lib,
    rv_lib
) {

  req( advan, trans, parm_lib )

  if ( isPRED() ){
    return( new[ !grepl("@TABLE", new) ] )
  }

  isPK <- input$pkInput != "none"
  isPD <- input$pdInput != "none"

  tmp <- c()

  if ( isPK & input$pkRVInput != "none" ){
    # If the model does not included any RV, all variables are already defined in other blocks

    # Get TMMD specific code
    if ( grepl("tmdd", input$eliminationInput) ){
      tmp <- c(
        tmp,
        parm_lib %>%
          dplyr::slice(
            n = get_model_lib_index(
              input = input, advan = advan, trans = trans, parm_lib = parm_lib
            )
          ) %>%
          dplyr::pull(.data$MRG_TABLE) %>%
          strsplit(split = "[|]") %>%
          unlist(),
        ""
      )
    }

    if ( isPD ){
      tmp <- c(
        tmp,
        "  // Residual variability for PK model"
      )
    }

    # Get RV model code
    tmpRV <- rv_lib %>%
      dplyr::filter(.data$TYPE == input$pkRVInput) %>%
      dplyr::pull(.data$RV_MRGSOLVE) %>%
      strsplit(split = "[|]") %>%
      unlist()

    # Input effect DRIVER and ENDPOINT in PD  models
    tmpRV <- gsub(
      "<DRIVER>",
      ifelse(
        grepl("tmdd", input$eliminationInput), "CFREE", "CP"),
      tmpRV
    )
    tmpRV <- gsub("<ENDPOINT>", "PK", tmpRV)

    tmp <- c(tmp, tmpRV, "  ")

  }

  if ( isPD ){

    req( input$pdRVInput )

    # Even if PD model has no RV, some PD variables must be defined

    if ( isPK ){
      if ( input$pdRVInput == "none" ) {
        if ( input$pdInput %in% c("direct", "biophase") ){
          tmp <- c(
            tmp,
            "  // Model output"
          )
        }
      } else {
        tmp <- c(
          tmp,
          ifelse(
            input$pdInput %in% c("direct", "biophase"),
            "  // Model output and residual variability for PD model",
            "  // Residual variability for PD model"
          )
        )
      }
    }

    # Get RV code  (if no RV, tmpRV will be NULL)
    tmpRV <- rv_lib %>%
      dplyr::filter(.data$TYPE == input$pdRVInput) %>%
      dplyr::pull(.data$RV_MRGSOLVE) %>%
      strsplit(split = "[|]") %>%
      unlist()

    if ( input$pdInput %in% c("direct", "biophase") ){

      req(
        areTruthy(
          input$effectFormInput, input$effectParmInput, input$effectStimInput
        )
      )

      # Find the type of model
      tmpModel <- glue::glue(
        "  double RESPONSE = {resp}",
        resp = parm_lib %>%
          dplyr::filter(
            .data$TYPE == "function" &
              .data$FORM == input$effectFormInput &
              .data$TRANS == input$effectParmInput &
              .data$INCREASE == as.integer(as.logical(input$effectStimInput))
          ) %>%
          dplyr::pull(.data$MRG_PRED) %>%
          strsplit(split = "[|]") %>%
          unlist()
      )

      # Modify the effect driver and endpoint in tmpModel and tmpRV
      tmpModel <- gsub("<x>", "DRIVER", tmpModel)
      tmpRV <- gsub("<DRIVER>", "RESPONSE", tmpRV)
      tmpRV <- gsub("<ENDPOINT>", "PD", tmpRV)

      tmpRV <- c(tmpModel, tmpRV)

    } else if ( input$pdInput %in% c("idr", "ode") ) {

      # Modify the effect driver and endpoint in tmpRV
      tmpRV <- gsub("<DRIVER>", "RESPONSE", tmpRV)
      tmpRV <- gsub("<ENDPOINT>", "PD", tmpRV)

    }

    tmp <- c(tmp, tmpRV, "  ")

  }

  if ( length(tmp) == 0 | all(grepl('^\\s+$', tmp)) ){
    tmp <- new[ !grepl("@TABLE", new) ]
  } else {
    tmp <- sub(
      "@TABLE",
      paste(
        "[ TABLE ]",
        paste(tmp, collapse = "\n"),
        sep = "\n"
      ),
      new
    )
  }

  tmp

}

#' Replacement of @CAPTURE tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param parms Parameter selection

replace_mrg_capture <- function(
    input,
    new,
    parms
){

  tmp <- c()

  # Get dependent variables and related variables
  if ( input$pkInput != "none" ) {
    if ( input$pkInput == "pk" && grepl("tmdd", input$eliminationInput) ) {
      # TMDD models do not use CP
      tmp <- c(tmp, "CFREE", "CTOT")
      if ( length(input$pkRVInput) > 0 && input$pkRVInput != "none" ) {
        tmp <- c(tmp, "CFREEi")
      }
    } else {
      # All other models should use CP
      tmp <- c(tmp, "CP")
      if ( length(input$pkRVInput) > 0 && input$pkRVInput != "none" ) {
        tmp <- c(tmp, "CPi")
      }
    }
  }
  if ( input$pdInput != "none" ) {
    if ( input$pdInput %in% c("er", "logistic", "ordcat") ) {
      tmp <- c(tmp, "EXPOSURE")
    }
    if ( input$pdInput == "logistic" ) {
      tmp <- c(tmp, "PROB", "EVENT")
    } else if ( input$pdInput == "ordcat" ) {
      range <- sort( req(input$minCategoryInput), req(input$maxCategoryInput))
      range <- c( floor(range[1]), ceiling(range[2]) -1 )
      tmp <- c(
        tmp,
        sprintf("P%s", range[1]:range[2]),
        ifelse(
          input$endpointInput == "",
          "EVENT",
          input$endpointInput
        )
      )
    } else {
      tmp <- c(tmp, "RESPONSE")
      if ( length(input$pdRVInput) > 0 && input$pdRVInput != "none" ) {
        tmp <- c(tmp, "RESPONSEi")
      }
    }
  }

  # Get parameters
  if ( length(parms) > 0 ){
    captureParams <- parms %>%
      dplyr::filter(.data$Variability != "None") %>%
      dplyr::pull(.data$Parameter) %>%
      as.character()
  } else {
    captureParams <- NULL
  }

  # Get ETA's
  if ( length(captureParams) > 0 ) {
    captureEtas <- paste0("E", captureParams)
  } else {
    captureEtas <- NULL
  }

  # Concatenate all
  if ( length(captureParams) > 0 ){
    tmp <- c(tmp, paste0("TV", captureParams), captureParams, captureEtas)
  }

  # Remove potential duplicate variables
  tmp <- unique(tmp)

  # Replace
  sub(
    "@CAPTURE",
    glue::glue(
      "[ CAPTURE ]\n  {vars}",
      vars = paste(tenvars(tmp), collapse = "\n  "),
      .trim = FALSE
    ),
    new
  )

}

#' Get mrgsolve model parameter code lines as list
#'
#' @param input Internal parameter for {shiny}
#' @param parms Parameter selection
#' @param mu A logical indicator for mu transformation

get_mrg_parms_code <- function(input, parms, mu){

  PK <- PD <- OT <- NULL
  multipleType <- FALSE

  if ( any(parms$Variability != "None") ) {
    for ( type in unique(parms$Type) ){
      individual <- glue::glue(
        "  {pre}// {type} parameters",
        pre = ifelse(multipleType, "\n  ", "")
      )

      if ( input$pdInput %in% c("ordcat") ){

        req( input$minCategoryInput, input$maxCategoryInput )

        range <- sort(input$minCategoryInput, input$maxCategoryInput)
        range <- c( floor(range[1]), ceiling(range[2]) -1 )
        nCategories <- diff(range) + 1

      }

      # Get individual parameter code for current type
      for ( iparm in which(parms$Type == type) ){

        if ( input$pdInput == "ordcat" && iparm <= nCategories ){
          individual <- c(
            individual,
            glue::glue(
              "  double TV{parm} = {parm2}T{parm};",
              parm = parms$Parameter[iparm],
              parm2 = ifelse(
                iparm == 1,
                "",
                glue::glue("T{parms$Parameter[iparm - 1]} + ")
              )
            )
          )
        } else {
          individual <- c( individual, get_mrg_individual_parm_code(parms, iparm) )
        }
      }

      if ( type == "PK" ){
        PK <- individual
      } else if ( type == "PD" ){
        PD <- individual
      } else {
        OT <- individual
      }

      multipleType <- TRUE

    }
  }

  list(PK = PK, PD = PD, OT = OT)

}

#' Get line of code for individual parameter value
#'
#' @param parms Parameter selection
#' @param iparm Index of parameter in parms data frame
#' @param eparm Parameter associated with IIV in ordered categorical models


get_mrg_individual_parm_code <- function(parms, iparm, eparm){

  parm <- parms$Parameter[iparm]

  # eparm is only provided in ordered categorical model
  if (missing(eparm)){
    eparm <- parm
  }

  switch(
    levels(parms$Variability)[parms$Variability[iparm]],
    "None" = glue::glue( "  double {parm} = TV{parm};" ),
    "Additive" = glue::glue( "  double {parm} = TV{parm} + E{eparm};" ),
    "Exponential" = glue::glue( "  double {parm} = TV{parm}*exp(E{eparm});" ),
    "Logit" =
      # Use the numerically stable of logit transform
      if (parms$Min[iparm] == 0 & parms$Max[iparm] == 1){
        glue::glue(
          "  double {parm} = 1/((1/TV{parm} - 1)*exp(-E{eparm}) + 1);"
        )
      } else if (parms$Min[iparm] == 0 & parms$Max[iparm] != 1){
        glue::glue(
          "  double {parm} = {parms$High[iparm]}/((1/TV{parm} - 1)*exp(-E{eparm}) + 1);"
        )
      } else {
        glue::glue(
          "  {parm} = {parms$Low[iparm]} + ({parms$High[iparm]} - {parms$Low[iparm]})/((1/TV{parm} - 1)*exp(-E{eparm}) + 1)"
        )
      }
  )
}

