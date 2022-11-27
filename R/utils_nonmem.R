
#' Creation of NONMEM code
#'
#' @param input Internal parameter for {shiny}
#' @param template Text template
#' @param vars Reactive object - List of variables in data file
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isODE Reactive object - is model coded with ODEs?
#' @param isLINMAT Reactive object - is model coded as linear matrix?
#' @param isPREDD Reactive object - is mode coded with $PK?
#' @param varianceTable Variance- table
#' @param covarianceBlock Variance-covariance matrix
#' @param rvTable  Reactive object - residual variability matrix
#' @param parm_lib Library of parameters
#' @param model_lib Library for $MODEL replacement
#' @param rv_lib  Library for residual variability replacement
#' @param scaling  Library for scaling
#' @param replacement Logical value indicating with replacement is required
#'

get_nonmem_code <- function(
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

  # Replace @PROB1 and @PROB2
  if ( isTruthy(input$modelInput) ){
    new <- replace_problem(input = input, new = new)
  }

  # Replace @PURPOSE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_purpose(input = input, new = new, variance = varianceTable)
  }

  # Replace @PATH
  new <- replace_path(input = input, new = new)

  # Replace @INPUT
  new <- replace_input(input = input, new = new, vars = vars)

  # Replace @DATA
  new <- replace_data(input = input, new = new)

  # Replace @SUBROUTINE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_subroutine(
      input = input,
      new = new,
      advan = advan,
      trans = trans,
      isPRED = isPRED,
      isODE = isODE,
      isLINMAT = isLINMAT
    )
  }

  # Replace @MODEL
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('model')
    new <- replace_model(
      input = input,
      new = new,
      model_lib = model_lib,
      isPRED = isPRED,
      isPREDPP = isPREDPP
    )
  }

  # Replace @ABBREVIATED
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('abbreviated')
    new <- replace_abbreviated(input = input, new = new, vars = vars)
  }

  # Extract tables content
  parms <- hot_to_r(input$parameterTable)
  estimations <- hot_to_r(input$estimationTable)

  # Replace @THETA

  if (isTruthy(parms) ){
    message('theta')
    new <- replace_theta(new = new, parms = parms)
  }

  # Replace @OMEGA
  if ( areTruthy(parms, covarianceBlock) ){
    message('omega')
    new <- replace_omega(
      new = new,
      parms = parms,
      varianceTable = varianceTable,
      blocks = covarianceBlock)
  }

  # Replace @SIGMA
  if (isTruthy(rvTable) ){
    message('sigma')
    new <- replace_sigma(input = input, new = new, rvTable = rvTable)
  }

  # Create @PRIOR
  message('prior')
  new <- replace_prior(
    input = input,
    new = new,
    parms = parms,
    varianceTable = varianceTable,
    estimations = estimations
  )

  # Create lines of preamble code
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('preamble')
    preamble_code <- get_preamble_code(
      input = input,
      parms = parms,
      vars = vars
    )
  }

  # Create code lines for PK, PD, and other parameters
  if ( areTruthy(input$pkInput, input$pdInput, varianceTable) ){
    message('params')
    parms_code <- get_parms_code(
      input = input,
      parms = parms,
      varianceTable = varianceTable,
      mu = as.logical(input$muInput)
    )
  }

  # Create code lines for derived parameters
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('derived params')
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
    message('scaling')
    scaling_code <- get_scaling_code(
      input = input,
      advan = advan,
      trans = trans,
      parm_lib = parm_lib,
      scaling = scaling
    )
  }

  # Determine the number of compartments for PK and PD components
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('ncmts')
    ncmts <- get_ncmts(
      input = input,
      model_lib = model_lib,
      isPRED = isPRED,
      isPREDPP = isPREDPP
    )
    nPKcmts <- ncmts[1]
    nPDcmts <- ncmts[2]
  }

  # Create code lines for compartment initialization
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('init')
    init_code <- get_init_code(
      input = input,
      advan = advan,
      trans = trans,
      nPKcmts = nPKcmts,
      nPDcmts = nPDcmts,
      parm_lib = parm_lib
    )
  }

  # Replace @PRED or @PK
  if ( areTruthy(input$pkInput, input$pdInput, varianceTable) &
       (isTruthy(input$pkInput) | isTruthy(input$pdInput)) ){
    message('pkpred')
    new <- replace_pk_pred(
      input = input,
      new = new,
      preamble_code = preamble_code,
      parms_code = parms_code,
      derived_parms_code = derived_parms_code,
      scaling_code = scaling_code,
      init_code = init_code,
      isPRED = isPRED,
      parms = parms,
      varianceTable = varianceTable,
      parm_lib = parm_lib,
      rv_lib = rv_lib
    )
  }

  # Replace @DES
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('des')
    new <- replace_des(
      input = input,
      new = new,
      advan = advan,
      trans = trans,
      isODE = isODE,
      vars = vars,
      nPKcmts = nPKcmts,
      nPDcmts = nPDcmts,
      parm_lib = parm_lib
    )
  }

  # Replace @ERROR
  if ( areTruthy(input$pkInput, input$pdInput) ){
    message('error')
    new <- replace_error(
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

  # Replace @TASK
  if ( areTruthy(input$pkInput, input$pdInput) ){
    new <- replace_task(
      input = input,
      new = new,
      estimations = estimations,
      isODE = isODE
    )
  }

  if (TRUE){
    return(new)
  }

  # Replace @TABLE
  new <- replace_table(new = new,
                       input = input,
                       parms = parms,
                       vars = vars
  )

  # Replace @TAGS
  new <- replace_tags(new = new,
                      input = input,
                      parms = parms
  )

  return(new)

}


#' Replacement of @PROB1 and @PROB2 tags
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template

replace_problem <- function(
    input,
    new
){

  if ( input$modelInput != "" ){
    model <- input$modelInput
  } else {
    model <- "$PROBLEM <Enter the control stream name>"
  }

  if ( input$nmFlavorInput == "Standard style" ){
    new <- sub("@PROB1", "", new)
    new <- sub(
      "@PROB2",
      paste0("$PROBLEM ", model, "\n"),
      new
    )
  } else {
    new <- sub(
      "@PROB1",
      paste0("$PROBLEM ", model, "\n"),
      new
    )
    new <- sub("@PROB2", "", new)
  }

  return(new)

}


#' Replacement of @INPUT tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param vars Character vector of variable names

replace_input <- function(input, new, vars){

  tmp <- if ( is.reactive(vars) ) { vars() } else { vars }

  if ( length(tmp) > 0 ){
    # Get 10 variables per lines
    varLines <- NULL
    while( length(tmp) > 0 ){
      varLines <- c(
        varLines,
        paste(
          tmp[1:min(10, length(tmp) )],
          collapse = " "
        )
      )
      tmp <- tmp[-(1:min(10,length(tmp)))]
    }
    new <- sub(
      "@INPUT",
      sprintf("$INPUT %s\n", paste(varLines, collapse = "\n  ")),
      new
    )
  } else {
    new <- sub(
      "@INPUT",
      "$INPUT <Enter list of variables>\n",
      new
    )
  }

  return(new)

}

#' Replacement of @DATA tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template

replace_data <- function(input, new){

  # Get absolute data file path
  if ( areTruthy(input$dataFileChoose, "files" %in% names(input$dataFileChoose)) ){
    dataPath <- normalizePath(
      shinyFiles::parseFilePaths(c(root = "/"), input$dataFileChoose)$datapath
    )
  } else {
    dataPath <- NULL
  }

  # Get absolute model file path
  if ( areTruthy(input$modelDirChoose, "path" %in% names(input$modelDirChoose)) ){
    modelPath <- normalizePath(
      shinyFiles::parseDirPath(c(root = "/"), input$modelDirChoose)
    )
  } else {
    modelPath <- NULL
  }

  # Get path of data file relative to model file
  if ( length(modelPath) > 0 ){
    relativePath <- try(
      xfun::relative_path(dataPath, dir = modelPath),
      silent = TRUE
    )

    if ( !inherits(relativePath, "try-error") ){
      dataPath <- relativePath
    }
  }

  sub(
    "@DATA",
    sprintf(
      "$DATA %s\n  IGNORE=@\n",
      ifelse(
        length(dataPath) > 0,
        dataPath,
        "<path to dataset>"
      )
    ),
    new
  )

}


#' Replacement of @SUBROUTINE tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isODE Reactive object - is model coded with ODEs?
#' @param isLINMAT Reactive object - is model coded as linear matrix?
#'

replace_subroutine <- function(
    input,
    new,
    advan,
    trans,
    isPRED,
    isODE,
    isLINMAT
) {

  if ( isPRED() ){
    # Remove tag for $PRED model
    new <- new[!grepl('@SUBROUTINE', new)]
  } else {
    tmp <- ''

    if (isODE()){
      tmp <- sprintf(
        '$SUBROUTINES ADVAN%s TRANS1 TOL=%d\n',
        input$advanInput,
        ifelse(
          isTruthy(input$nsigInput),
          3*as.numeric(input$nsigInput),
          3
        )
      )
    } else if ( isLINMAT() ){
      tmp <- sprintf(
        '$SUBROUTINES ADVAN%s TRANS1 TOL=%d\n',
        input$advanInput,
        ifelse(
          isTruthy(input$nsigInput),
          3*as.numeric(input$nsigInput),
          3
        )
      )
    } else {
      tmp <- sprintf('$SUBROUTINES ADVAN%d TRANS%d\n', advan(), trans())
    }

    new <- sub('@SUBROUTINE', tmp, new)

  }

  return(new)

}

#' Replace @MODEL tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param model_lib Library for $MODEL replacement
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isPREDD Reactive object - is mode coded with $PK?
#'

replace_model <- function(
    input,
    new,
    model_lib,
    isPRED,
    isPREDPP
){

  if (isPRED() | isPREDPP()){
    new <- new[!grepl("@MODEL", new)]
  } else {

    # Determine compartments for PK component
    if ( input$pkInput == "pk" ) {
      tmp <- model_lib %>%
        dplyr::filter(CMT == input$pkCMTInput & ABSORPTION == input$absorptionInput)

      if ( grepl("tmdd", input$eliminationInput) ) {
        tmp <- tmp %>%
          dplyr::filter(ELIMINATION == input$eliminationInput)
      } else {
        tmp <- tmp %>%
          dplyr::filter(ELIMINATION == "mmlin")
      }
      tmp <- unlist(strsplit(tmp[, "NONMEM"], split = "[|]"))
      nPKcmts <- length(tmp)
    } else {
      if ( isTruthy(input$pknCMTInput) ){
        nPKcmts <- ifelse(
          input$pkInput != "none",
          input$pknCMTInput,
          0
        )
      } else {
        nPKcmts <- 0
      }
      tmp <- c()
    }

    # Determine compartments for PD component
    nPDcmts <- switch(
      input$pdInput,
      "link" = 1,
      "idr" = 1,
      "ode" = input$pdnCMTInput,
      0
    )

    # Start of $MODEL statement replacement
    tmp <- c(
      sprintf("$MODEL NCOMPARTMENTS=%s", nPKcmts + nPDcmts),
      tmp
    )

    # Add PK compartments which have not yet been defined (i.e, if pkInput is not "pk")
    if ( input$pkInput != "pk" & nPKcmts > 0 ){
      for ( iCMT in 1:nPKcmts ){
        tmp <- c(
          tmp,
          paste0(
            sprintf("  COMP=(COMP%d", iCMT),
            sprintf("%s", ifelse(as.numeric(input$pkDefaultDoseInput) == iCMT, " DEFDOSE", "")),
            sprintf("%s", ifelse(as.numeric(input$pkDefaultObsInput) == iCMT, " DEFOBS", "")),
            ")"
          )
        )
      }
    }

    # Add PD compartments
    if ( nPDcmts > 0) {
      for ( iCMT in (nPKcmts + 1:nPDcmts) ){
        if (input$pdInput == "link"){
          tmp <- c(tmp, "  COMP=(BIOPHASE)")
        } else if (input$pdInput == "idr"){
          tmp <- c(tmp, "  COMP=(IDR)")
        } else {
          tmp <- c(tmp, sprintf("  COMP=(COMP%d)", iCMT))
        }
      }
    }

    new <- sub(
      "@MODEL",
      sprintf(
        "%s\n",
        paste(tmp, collapse = "\n")
      ),
      new
    )

  }

  new

}

#' Replacement of @ABBREVIATED tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param vars Character vector of variable names

replace_abbreviated <- function(
    input,
    new,
    vars
){

  # $ABBREVIATED only required for transit compartment absorption model
  if ( !(input$pkInput == 'pk' && input$absorptionInput == 'transit') ){
    new <- new[!grepl('@ABBREVIATED', new)]
  } else {
    doseMatrix <- 'DSEVENT'
    ndoseMatrix <- 'NDSEVENT'
    doseTimeMatrix <- 'DSTIME'

    # Ensure uniqueness of matrix variables
    if (length(vars()) > 0){
      while( doseMatrix %in% vars() ){
        doseMatrix <- paste0('Z', doseMatrix)
      }
      while (ndoseMatrix %in% vars()){
        ndoseMatrix <- paste0('Z', ndoseMatrix)
      }
      while (doseTimeMatrix %in% vars()){
        doseTimeMatrix <- paste0('Z', doseTimeMatrix)
      }
    }
    rep <- c(
      sprintf(
        '$ABBREVIATED DECLARE %s(100),%s(100)    ; 100 is maximum number of doses per ID',
        doseTimeMatrix,
        doseMatrix
      ),
      '$ABBREVIATED DECLARE DOWHILE I',
      sprintf('$ABBREVIATED DECLARE DOWHILE %s', ndoseMatrix)
    )
    new <- sub('@ABBREVIATED', paste0(paste(rep, collapse = '\n'), '\n'), new)
  }

  new

}

#' Replacement of @THETA tag
#'
#' @param new Text template
#' @param vars Character vector of variable names

replace_theta <- function(
    new,
    parms
){

  tmp <- c(
    "$THETA",
    sapply(
      1:nrow(parms),
      function(x, parms){
        sprintf(
          "  (%s, %s, %s)  %s;--th%s- %s: %s%s",
          parms$Low[x],
          parms$Initial[x],
          parms$High[x],
          ifelse(parms$Fixed[x] == "Yes", "FIXED", ""),
          x,
          parms$Parameter[x],
          ifelse(
            parms$Label[x] == "",
            sprintf("Label for THETA%s", x),
            parms$Label[x]
          ),
          ifelse(
            parms$Unit[x] == "",
            " (-)",
            sprintf(" (%s)", parms$Unit[x])
          )
        )
      },
      parms),
    ""
  )

  # Align tags
  tmp <- align_tags(code = tmp)

  # Replace
  new <- sub("@THETA", paste(tmp, collapse = "\n"), new)

  new

}


#' Replacement of @OMEGA tag
#'
#' @param new Text template
#' @param vars Character vector of variable names
#' @param varianceTable Variability selection
#' @param covarianceBlock Variance - covariance matrix
#'

replace_omega <- function(
    new,
    parms,
    varianceTable,
    blocks
){

  if ( !all(varianceTable$Variability == 'None') ){
    # Add the theta numbers in parameter table
    parms$TH <- 1:nrow(parms)



    ieta <- 0
    tmp <- c()
    for ( iomega in 1:length(blocks) ){
      omega <- blocks[[iomega]]$omega

      req( all(rownames(omega) %in% parms$Parameter) )

      # Get type of current and previous omega block
      type <- blocks[[iomega]]$type
      previousType <- 'notdiagonal'
      if (iomega >= 2){
        previousType <- blocks[[iomega - 1]]$type
      }

      # Add new empty line if necessary
      if ( iomega >= 2 & (previousType != 'diagonal' | type != 'diagonal') ){
        tmp <- c(tmp, '  ')
      }
      # Add $OMEGA line if necessary
      if ( type != 'diagonal' ) {
        tmp <- c(tmp, sprintf('$OMEGA BLOCK(%s)', nrow(omega)) )
      } else if ( type == 'diagonal' & previousType != 'diagonal' ){
        tmp <- c(tmp, '$OMEGA')
      }

      # Add omega value and tag
      for ( i in 1:nrow(omega) ){
        ieta <- ieta + 1
        index <- which(parms$Parameter == rownames(omega)[i])
        tmp <- c(
          tmp,
          sprintf(
            '  %s;--eta%s- IIV in %s [%s]',
            ifelse(
              type == 'diagonal',
              # Variability without correlation
              sprintf('%s  ', omega[i, i]),
              # Variability with correlation
              paste(
                c(
                  paste0(
                    as.character(
                      omega[i, 1:i]
                    ),
                    ' '
                  ),
                  ' '
                ),
                collapse = ''
              )
            ),
            ieta,
            parms$Parameter[index],
            switch(
              levels(varianceTable$Variability)[varianceTable$Variability[index]],
              'Additive' = 'add',
              'Exponential' = 'exp',
              'Logit' = if (parms$Low[index] == 0 & parms$High[index] == 1){
                sprintf('cv=100*(1-th%s)*eta%s', parms$TH[index], index)
              } else if (parms$Low[index] == 0 & parms$High[index] != 1){
                sprintf('cv=100*%s*(1-th%s)*eta%s', parms$High[index], parms$TH[index], index)
              } else {
                sprintf(
                  'cv=100*((th%s-%s)*(%s-th%s)/(th%s*(%s-%s)))*eta%s',
                  parms$TH[index],
                  parms$Low[index],
                  parms$High[index],
                  parms$TH[index],
                  parms$TH[index],
                  parms$High[index],
                  parms$Low[index],
                  ieta
                )
              }
            )
          )
        )
      }

    }

    tmp <- c(tmp, '  ')

    # Align tags
    tmp <- align_tags(tmp)

    # Replace
    new <- sub('@OMEGA', paste(tmp, collapse = '\n'), new)

  } else {
    new <- new[!grepl('@OMEGA', new)]
  }

  new

}

#' Replacement of @SIGMA tag
#'
#' @param new Text template
#' @param input Internal parameter for {shiny}
#' @param rvTable Residual variability selection
#'

replace_sigma <- function(new, input, rvTable){

  sigma <- c()
  ieps <- 1

  # Process rvTable
  rvTable[, 3] <- ifelse(is.na(rvTable[, 3]), 0, rvTable[, 3])

  # Add content
  if ( input$pkInput != "none" ){
    req(input$pkRVInput)
    if ( input$pkRVInput %in% c("add", "log", "ccv") ){
      sigma <- c(
        sigma,
        sprintf(
          "  %s  ;--eps%s- Residual variability %s%s[%s]",
          rvTable[ieps, 3],
          ieps,
          ifelse(input$pdInput != "none", "for PK ", ""),
          ifelse(input$pkRVInput == "log", "(log units) ", ""),
          ifelse(input$pkRVInput == "ccv", "ccv", "add")
        )
      )
      ieps <- ieps + 1
    } else if ( input$pkRVInput == "accv" ){
      sigma <- c(
        sigma,
        sprintf(
          "  %s  ;--eps%s- Constant CV RV component %s[accv1=eps%s-ccv;eps%s-add]",
          rvTable[ieps, 3],
          ieps,
          ifelse(input$pdInput != "none", "for PK ",""),
          ieps,
          ieps + 1
        ),
        sprintf(
          "  %s  ;--eps%s- Additive RV component %s[accv1]",
          rvTable[ieps + 1, 3],
          ieps + 1,
          ifelse(input$pdInput != "none", "for PK ","")
        )
      )
      ieps <- ieps + 2
    }
  }
  if ( input$pdInput != "none" ){
    req(input$pdRVInput)
    if ( input$pdRVInput %in% c("add", "log", "ccv") ){
      sigma <- c(
        sigma,
        sprintf(
          "  %s  ;--eps%s- Residual variability %s%s[%s]",
          rvTable[ieps, 3],
          ieps,
          ifelse(input$pkInput != "none", "for PD ", ""),
          ifelse(input$pdRVInput == "log", "(log units) ", ""),
          ifelse(input$pdRVInput == "ccv", "ccv", "add")
        )
      )
    } else if ( input$pdRVInput == "accv" ){
      sigma <- c(
        sigma,
        sprintf(
          "  %s  ;--eps%s- Constant CV RV component %s[accv%s=eps%s-ccv;eps%s-add]",
          rvTable[ieps, 3],
          ieps,
          ifelse(input$pkInput != "none", "for PD ",""),
          ifelse(input$pkRVInput == "accv", 2, 1),
          ieps,
          ieps + 1
        ),
        sprintf(
          "  %s  ;--eps%s- Additive RV component %s[accv%s]",
          rvTable[ieps + 1, 3],
          ieps + 1,
          ifelse(input$pkInput != "none", "for PD ",""),
          ifelse(input$pkRVInput == "accv", 2, 1)
        )
      )
    }
  }

  if ( length(sigma) == 0 ){
    new <- new[!grepl("@SIGMA", new)]
  } else {
    # Align tags
    sigma <- align_tags(code = sigma)

    # Add prediction tag for ACCV residual variability models
    if ( input$pkInput != "none" && input$pkRVInput == "accv" ){
      sigma <- c(
        sigma,
        "  ",
        ";--pred[accv1]=0.1,1,10"
      )
    }
    if ( input$pdInput != "none"  && input$pdRVInput == "accv" ){
      if ( input$pdInput != "none" && input$pkRVInput == "accv" ) {
        sigma <- c(
          sigma,
          ";--pred[accv2]=0.1,1,10",
        )
      } else {
        sigma <- c(
          sigma,
          "  ",
          ";--pred[accv1]=0.1,1,10"
        )
      }
    }

    # Replace
    new <- sub(
      "@SIGMA",
      paste0(paste(c("$SIGMA", sigma), collapse = "\n"), "\n"),
      new
    )
  }

  return(new)

}

#' Replacement of @SIGMA tag
#'
#' @param new Text template
#' @param input Internal parameter for {shiny}
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param rvTable Residual variability selection

replace_prior <- function(
    input,
    new,
    parms,
    varianceTable,
    estimations
){

  tmp <- c()
  if (
    notTruthy(
      parms,
      varianceTable,
      (isTruthy(input$pkRVInput) | isTruthy(input$pdRVInput)),
      (as.logical(input$estimationInput) && any(estimations$Method == "BAYES"))
    )
  ){
    new <- new[!grepl("@PRIOR", new)]
  } else {

    # THETAP prior information about THETAs
    thetap <- c(
      "$THETAP",
      sapply(
        1:nrow(parms),
        function(x, parms){
          sprintf(
            "  %s FIXED ; Prior information for %s",
            parms$Initial[x],
            parms$Parameter[x]
          )
        },
        parms),
      ""
    )

    thetap <- align_tags(code = thetap)

    # THETAPV confidence around the priors on THETAs
    thetapv <- sprintf(
      "$THETAPV BLOCK(%s) FIXED VALUES(10000, 0.0)",
      nrow(parms)
    )

    # OMEGAP prior information about OMEGAs
    omegap <- c()

    if ( any(varianceTable$Variability != "None") ){
      omegaParameters <- which(varianceTable$Variability != "None")
      omegap <- sprintf(
        "$OMEGAP BLOCK(%s)",
        length(omegaParameters)
      )
      for ( i in seq_along(omegaParameters) ){
        omegaParameter <- omegaParameters[i]
        omegap <- c(
          omegap,
          paste(
            "  ",
            paste(
              paste0(
                as.character(c(rep(0, i - 1), 0.2)),
                " "
              ),
              collapse = ""
            ),
            sprintf(
              "; Prior information for IIV in %s",
              parms$Parameter[omegaParameter]
            )
          )
        )
      }
    }

    omegap <- align_tags(code = omegap)

    # OMEGAPD confidence around the prior information on OMEGA
    omegapd <- c()

    if (any(varianceTable$Variability != "None")){

      nOmegaBlocks <- length(
        unlist(
          sapply(
            which( grepl("[$]OMEGA", new) ),
            function(x, new){
              which(
                grepl("[$]OMEGA", unlist(strsplit(new[x], split = "\n")))
              )
            },
            new,
            simplify = FALSE
          )
        )
      )

      omegapd <- paste(
        "$OMEGAPD",
        paste(
          rep("4 FIXED", times = nOmegaBlocks),
          collapse = " "
        )
      )
    }

    # SIGMAP prior information about SIGMA
    sigmap <- c()

    if ( input$pkInput != "none" | input$pdInput != "none" ){

      if ( (input$pkInput != "none" && input$pkRVInput != "none") |
           (input$pdInput != "none" && input$pdRVInput != "none" )
      ){
        sigmap <- "$SIGMAP"
      }

      if ( input$pkInput != "none" && input$pkRVInput != "none" ){
        if ( input$pkRVInput %in% c("add", "log", "ccv") ){
          sigmap <- c(
            sigmap,
            sprintf(
              "  1.0 FIXED ; Prior information for residual variability %s%s",
              ifelse(input$pdInput != "none", "for PK ", ""),
              ifelse(input$pkRVInput == "log", "(log units", "")
            )
          )
        } else {
          sigmap <- c(
            sigmap,
            sprintf(
              "  0.2 FIXED ; Prior information for constant CV RV component %s",
              ifelse(input$pdInput != "none", "for PK ", "")
            ),
            sprintf(
              "  1.0 FIXED ; Prior information for additive RV component %s",
              ifelse(input$pdInput != "none", "for PK ", "")
            )
          )
        }
      }

      if ( input$pdInput != "none" && input$pdRVInput != "none" ){
        if ( input$pdRVInput %in% c("add", "log", "ccv") ){
          sigmap <- c(
            sigmap,
            sprintf(
              "  1.0 FIXED ; Prior information for residual variability %s%s",
              ifelse(input$pkInput != "none", "for PD ", ""),
              ifelse(input$pdRVInput == "log", "(log units", "")
            )
          )
        } else {
          sigmap <- c(
            sigmap,
            sprintf(
              "  0.2 FIXED ; Prior information for constant CV RV component %s",
              ifelse(input$pkInput != "none", "for PD ", "")
            ),
            sprintf(
              "  1.0 FIXED ; Prior information for additive RV component %s",
              ifelse(input$pkInput != "none", "for PD ", "")
            )
          )
        }
      }
    }

    # SIGMAPD confidence around the prior information on SIGMA
    sigmapd <- c()

    if ( input$pkInput != "none" | input$pdInput != "none" ){
      sigmapd <- "$SIGMAPD 4 FIXED"
    }

    # Replacement
    tmp <- c(
      tmp,
      "$PRIOR NWPRI",
      "",
      "; Prior information about THETAs",
      thetap,
      "",
      "; Variance to prior information about THETAs",
      "; A large variance indicates that prior information about THETAs is highly uninformative",
      thetapv,
      "",
      if ( length(omegap) > 0 ) {
        c(
          "; Prior information about OMEGAs",
          omegap,
          ""
        )
      },
      if ( length(omegap) > 0 ) {
        c(
          "; Degrees of freedom for OMEGA prior information",
          "; Low values indicate that prior information about OMEGAs is highly uninformative",
          omegapd,
          ""
        )
      },
      if ( length(sigmap) > 0 ) {
        c(
          "; Prior information about SIGMAs",
          sigmap,
          ""
        )
      },
      if ( length(sigmap) > 0 ) {
        c(
          "; Degrees of freedom for SIGMA prior information",
          "; Low values indicate that prior information about SIGMAs is highly uninformative",
          sigmapd,
          ""
        )
      }
    )

    new <- sub(
      "@PRIOR",
      paste0(paste(tmp, collapse = "\n"), "\n"),
      new
    )
  }

}

#' Replace @PK and @PRED tags
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param preamble_code Preamble code
#' @param parms_code Typical and individual parameter code
#' @param scaling_code Dose scaling and bioavailability code
#' @param init_code Compartment initialization code
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param parm_lib Library of parameters
#' @param rv_lib  Library for residual variability replacement

replace_pk_pred <- function(
    input,
    new,
    preamble_code,
    parms_code,
    derived_parms_code,
    scaling_code,
    init_code,
    isPRED,
    parms,
    varianceTable,
    parm_lib,
    rv_lib
){

  if ( isTruthy(input$mapTable) ){
    dvidVar <- hot_to_r(input$mapTable) %>%
      dplyr::filter(Description == "Endpoint identifier variable") %>%
      dplyr::pull(Variable)
  } else {
    dvidVar <- NULL
  }

  if ( isPRED() ){

    new <- new[!grepl('@PK', new)]

    # Add preamble
    tmp <- preamble_code

    # Add parameter definition
    tmp <- c(tmp, unlist(parms_code), '')

    isPK <- input$pkInput != 'none'
    isPD <- input$pdInput != 'none'

    # Add PK-specific code
    if ( isPK ){

      if ( isPD ){
        tmp <- c(
          tmp,
          ifelse(
            input$pkRVInput == "none",
            "  ; Model output for PK model",
            "  ; Model output and residual variability for PK model"
          ),
          sprintf(
            "  IF (%s == 1) THEN <Check that endpoint value is appropriate>",
            ifelse(
              length(dvidVar) > 0 && dvidVar != "",
              dvidVar,
              "<endpoint variable>"
            )
          )
        )
      } else {
        tmp <- c(
          tmp,
          ifelse(
            input$pkRVInput == "none",
            "  ; Model output",
            "  ; Model output and residual variability"
          )
        )
      }

      # Add model output
      tmpModel <- "  CP = <Define the model output function>"

      # Add RV model
      tmpRV <- rv_lib %>%
        dplyr::filter(TYPE == input$pkRVInput)
      if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM_M3)

        if ( blqVariable != ""){
          tmpRV <- sub(
            "<BLQ>",
            blqVariable,
            tmpRV
          )
        }

      } else {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM)
      }

      if ( input$pkRVInput == "none" ) {
        if ( isPD & input$pdRVInput != "none" ) {
          tmpRV <- "  IPRED = <F>|  IRES = 0|  IWRES = 0|  |  Y = IPRED"
        } else {
          tmpRV <- "  IPRED = <F>|  |  Y = IPRED"
        }
      }
      tmpRV <- unlist(strsplit(tmpRV, split = "[|]"))

      # Substitute F, DV, and EPS parameter indices
      tmpRV <- gsub("<F>", "CP", tmpRV)
      tmpRV <- gsub("<DV>", "DV", tmpRV)
      tmpRV <- gsub("<1>", 1, tmpRV)
      tmpRV <- gsub("<2>", 2, tmpRV)

      # Adjust error model
      if ( input$pkRVInput %in% c("ccv", "log") ){
        tmpRV <- c(
          tmpModel,
          "  FLAG = 0",
          if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
            "  IF (CP == 0) FLAG = 1E-16"
          },
          tmpRV
        )
      } else {
        tmpRV <- c(tmpModel, tmpRV)
      }

      if ( isPD ){
        tmpRV <- c(
          paste0("  ", tmpRV),
          "  ENDIF"
        )
      }

      tmp <- c(tmp, tmpRV, "  ")

      nPKeps <- ifelse(input$pkRVInput == "accv", 2, 1)

    } else {
      nPKeps <- 0
    }

    # Add model-specific code
    if ( isPD ){

      if ( isPK ){
        tmp <- c(
          tmp,
          ifelse(
            input$pdRVInput == "none",
            "  ; Model output for PD model",
            "  ; Model output and residual variability for PD model"
          ),
          sprintf(
            "  IF (%s == 2) THEN <Check that endpoint value is appropriate>",
            ifelse(
              length(dvidVar) > 0 && dvidVar != "",
              dvidVar,
              "<endpoint variable>"
            )
          )
        )
      } else if ( !input$pdInput %in% c("logis", "ordcat") ) {

        tmp <- c(
          tmp,
          ifelse(
            input$pdRVInput == "none",
            "  ; Model output",
            "  ; Model output and residual variability"
          )
        )
      }

      # Add RV model
      tmpRV <- rv_lib %>%
        dplyr::filter(TYPE == input$pdRVInput)
      if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM_M3)

        if ( blqVariable != ""){
          tmpRV <- sub(
            "<BLQ>",
            blqVariable,
            tmpRV
          )
        }

      } else {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM)
      }
      if ( input$pdRVInput == "none" ) {
        if ( isPK & input$pkRVInput != "none" ){
          tmpRV <- "  IPRED = <F>|  IRES = 0|  IWRES = 0|  |  Y = IPRED"
        } else {
          tmpRV <- "  IPRED = <F>|  |  Y = IPRED"
        }
      }
      tmpRV <- unlist(strsplit(tmpRV, split = "[|]"))

      if ( input$pdInput == "logis" ){

        req(
          areTruthy(
            input$effectFormInput,input$effectParmInput, input$effectStimInput
          )
        )

        # Add code for logistic regression
        tmpModel <- parm_lib %>%
          dplyr::filter(TYPE == "logis") %>%
          dplyr::pull(PRED)
        tmpModel <- unlist(strsplit(tmpModel, split = "[|]"))

        # Extract the drug effect function
        parm_lib_index <- parm_lib %>%
          dplyr::filter(
            TYPE == "function" &
              FORM == input$effectFormInput &
              TRANS == input$effectParmInput &
              INCREASE == ifelse(
                input$effectFormInput == "base",
                1,
                as.integer(as.logical(input$effectStimInput))
              )
          )
        logitFun <- parm_lib_index %>% dplyr::pull(PRED)

        # Find the baseline parameter to replace in logitFun
        tmpModelParm <- parm_lib %>%
          dplyr::filter(TYPE == "logis") %>%
          dplyr::pull(PARMS)
        logitFunParms <- parm_lib_index %>% dplyr::pull(PARMS)
        minimumParm <- unlist(strsplit(logitFunParms, split = "[|]"))[1]

        # Add logistic regression code
        tmpModel <- sub("<LOGIT>", sub(minimumParm, tmpModelParm, logitFun), tmpModel)
        if ( isTruthy(input$logisVarInput) ){
          tmpModel <- gsub("<x>", input$logisVarInput, tmpModel)
        } else if ( isTruthy(input$logisVarTextInput) ){
          tmpModel <- gsub("<x>", input$logisVarTextInput, tmpModel)
        }
        tmpRV <- tmpModel

      } else if ( input$pdInput == "ordcat" ){

        req(
          areTruthy(
            input$minCategoryInput, input$maxCategoryInput,
            input$effectFormInput,input$effectParmInput, input$effectStimInput
          )
        )

        if (input$minCategoryInput <= input$maxCategoryInput){
          minCategory <- floor(input$minCategoryInput)
          maxCategory <- ceiling(input$maxCategoryInput)
        } else {
          minCategory <- floor(input$maxCategoryInput)
          maxCategory <- ceiling(input$minCategoryInput)
        }

        # Add code for ordered categorical model
        tmpModel <- parm_lib %>%
          dplyr::filter(TYPE == "ordcat") %>%
          dplyr::pull(PRED)
        tmpModel <- unlist(strsplit(tmpModel, split = "[|]"))

        # Extract the drug effect function
        parm_lib_index <- parm_lib %>%
          dplyr::filter(
            TYPE == "function" &
              FORM == input$effectFormInput &
              TRANS == input$effectParmInput &
              INCREASE == ifelse(
                input$effectFormInput == "base",
                1,
                as.integer(as.logical(input$effectStimInput))
              )
          )
        logitFun <- parm_lib_index %>% dplyr::pull(PRED)

        addFun <- gregexpr("[+]", logitFun)[[1]][1] < gregexpr("[*]", logitFun)[[1]][1]

        # Find the baseline parameter to replace in logitFun
        logitFunParms <- parm_lib_index %>% dplyr::pull(PARMS)
        minimumParm <- unlist(strsplit(logitFunParms, split = "[|]"))[1]

        # Define effect
        effect <- sub(minimumParm, "", logitFun)
        if ( addFun ){
          effect <- sub("^ [+] ", "", effect)
        } else {
          effect <- sub("^\\s*[*]\\s*[(]", "", effect)
          effect <- sub("[)]\\s*$", "", effect)
        }
        effect <- sprintf("  EFFECT = %s", effect)

        # Create replacement codes
        logit <- logiti <- prob <- yn <- ind0 <- ind1 <- c()
        y <- sprintf("  Y = Y%s*IND%s", minCategory, minCategory)
        sim <- c(
          sprintf(
            "  %s = DV",
            ifelse(
              input$endpointInput == "",
              "EVENT",
              input$endpointInput
            )
          ),
          "  IF (ICALL == 4) THEN",
          "    CALL RANDOM(2, R)",
          sprintf(
            "    %s = %s",
            ifelse(
              input$endpointInput == "",
              "EVENT",
              input$endpointInput
            ),
            minCategory
          )
        )

        for ( i in minCategory:maxCategory ) {
          if ( i < maxCategory ){
            logit <- c(
              logit,
              sprintf(
                "  TVLG%s = TV%s%sEFFECT",
                i,
                parms$Parameter[i - minCategory + 1],
                ifelse(addFun, " + ", "*")
              )
            )
            if ( i == minCategory ){
              logiti <- c(
                logiti,
                get_individual_parm_line(
                  modifyList(parms, list(Parameter = parms$SourceParam[1])),
                  varianceTable,
                  1,
                  1,
                  FALSE
                )
              )
            }
            logiti <- c(
              logiti,
              get_individual_parm_line(
                modifyList(parms, list(Parameter = paste0("LG", i))),
                varianceTable,
                1,
                1,
                FALSE
              )
            )
            prob <- c(
              prob,
              glue::glue("  P{i} = EXP(LG{i})/(1 + EXP(LG{i}))  ; P(Y<={i}|X)")
            )
          }
          yn <- c(
            yn,
            ifelse(
              i == minCategory,
              glue::glue("  Y{i} = P{i}  ; P(Y={i}|X)"),
              sprintf("  Y%s = %s - P%s  ; P(Y=%s|X)",
                      i,
                      ifelse(i == maxCategory, "1", sprintf("P%s", i)),
                      i - 1,
                      i
              )
            )
          )
          if (i > minCategory) {
            y <- paste0(y, glue::glue(" + Y{i}*IND{i}"))
          }
          ind0 <- c(ind0, glue::glue("  IND{i} = 0"))
          ind1 <- c(ind1, glue::glue("  IF (DV == {i}) IND{i} = 1"))
          if (i < maxCategory) {
            sim <- c(
              sim,
              sprintf(
                "    IF (R > P%s) %s = %s",
                i,
                ifelse(
                  input$endpointInput == "",
                  "EVENT",
                  input$endpointInput
                ),
                i + 1
              )
            )
          } else{
            sim <- c(sim, "  ENDIF")
          }
        }

        # Align comments in prob and yn
        prob <- align_tags(code = prob)
        yn <- align_tags(code = yn)

        # Add logistic regression code
        tmpModel <- sub("<EFFECT>", effect, tmpModel)
        tmpModel <- sub(
          "<LOGIT>",
          paste(c(logit, "  ", logiti), collapse = "\n"),
          tmpModel
        )
        tmpModel <- sub("<PROB>", paste(prob, collapse = "\n"), tmpModel)
        tmpModel <- sub(
          "<EST>",
          paste(
            c(
              paste(ind0, collapse = "\n"),
              "  ",
              paste(ind1, collapse = "\n"),
              "  ",
              paste(yn, collapse = "\n"),
              "  ",
              y
            ),
            collapse = "\n"
          ),
          tmpModel
        )
        tmpModel <- sub("<SIM>", paste(sim, collapse = "\n"), tmpModel)
        if ( isTruthy(input$logisVarInput) ){
          tmpModel <- gsub("<x>", input$logisVarInput, tmpModel)
        } else if ( isTruthy(input$logisVarTextInput) ){
          tmpModel <- gsub("<x>", input$logisVarTextInput, tmpModel)
        }

        tmpRV <- tmpModel

      } else if ( input$pdInput == "er" ){

        req(
          areTruthy(
            input$effectFormInput, input$effectParmInput, input$effectStimInput
          )
        )

        # Add code for exposure-response
        tmpModel <- paste0(
          "  RESP = ",
          parm_lib %>%
            dplyr::filter(
              TYPE == "function" &
                FORM == input$effectFormInput &
                TRANS == input$effectParmInput &
                INCREASE == as.integer(as.logical(input$effectStimInput))
            ) %>%
            dplyr::pull(PRED)
        )


        tmpModel <- unlist(strsplit(tmpModel, split = "[|]"))
        if ( isTruthy(input$exposureVarInput) ){
          tmpModel <- gsub("<x>", input$exposureVarInput, tmpModel)
        } else if ( isTruthy(input$exposureVarTextInput) ){
          tmpModel <- gsub("<x>", input$exposureVarTextInput, tmpModel)
        }

        # Substitute F, DV, and EPS parameter indices
        tmpRV <- gsub("<F>", "RESP", tmpRV)
        tmpRV <- gsub("<DV>", "DV", tmpRV)
        tmpRV <- gsub("<1>", nPKeps + 1, tmpRV)
        tmpRV <- gsub("<2>", nPKeps + 2, tmpRV)

        # Adjust error model
        if ( input$pdRVInput %in% c("ccv", "log") ){
          tmpRV <- c(
            tmpModel,
            "  FLAG = 0",
            if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
              "  IF (RESP == 0) FLAG = 1E-16"
            },
            tmpRV
          )
        } else {
          tmpRV <- c(tmpModel, tmpRV)
        }

      } else {

        # Add model output
        tmpModel <- "  RESP = <Define the model output function>"
        tmpModel <- gsub("IPRED", "RESP", tmpModel)

        # Substitute F, DV, and EPS parameter indices
        tmpRV <- gsub("<F>", "RESP", tmpRV)
        tmpRV <- gsub("<DV>", "DV", tmpRV)
        tmpRV <- gsub("<1>", nPKeps + 1, tmpRV)
        tmpRV <- gsub("<2>", nPKeps + 2, tmpRV)

        # Create flag variable for CCV and log error models
        if (input$pdRVInput %in% c("ccv", "log")){
          tmpRV <- c(
            tmpModel,
            "  FLAG = 0",
            if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
              "  IF (RESP == 0) FLAG = 1E-16"
            },
            tmpRV
          )
        } else {
          tmpRV <- c(tmpModel, tmpRV)
        }

      }

      if ( isPK ){
        tmpRV <- c(
          paste0("  ", tmpRV),
          "  ENDIF"
        )
      }

      tmp <- c(tmp, tmpRV, "  ")

    }
    new <- sub(
      "@PRED",
      paste("$PRED", paste(tmp, collapse = "\n"), sep = "\n"),
      new
    )

  } else {
    new <- new[!grepl("@PRED", new)]
    new <- sub(
      "@PK",
      paste(
        "$PK",
        paste(
          c(
            # Include preamble code
            preamble_code,
            # Include parameter code
            unlist(parms_code),
            # Include derived parameter code
            derived_parms_code,
            # Include scale and bioavailability code
            scaling_code,
            # Include compartment initialization block
            init_code,
            ""
          ),
          collapse = "\n"),
        sep = "\n"),
      new
    )
  }

  new

}

#' Replace @DES tag
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isODE Reactive object - is model coded with ODEs?
#' @param vars Reactive object - List of variables in data file
#' @param nPKcmts Number of PK compartments in the model
#' @param nPDcmts Number of PD compartments in the model
#' @param parm_lib Library of parameters
#'

replace_des <- function(
    input,
    new,
    advan,
    trans,
    isODE,
    vars,
    nPKcmts,
    nPDcmts,
    parm_lib
){

  if ( !isODE() ){
    new <- new[!grepl("@DES", new)]
  } else {

    tmp <- c()

    if ( !input$pkInput %in% c("none", "pred") ){

      if (input$pkInput == "pk"){

        # Get the $DES code for PREDPP models
        index <- get_model_lib_index(
          input = input, advan = advan, trans = trans, parm_lib = parm_lib
        )
        req( index )

        tmpModel <- unlist(
          strsplit(
            parm_lib[index, "DES"],
            split = "[|]"
          )
        )

        if ( parm_lib[index, "ELIMINATION"] != "lin"){
          value <- ifelse(parm_lib[index, "ABSORPTION"] == "bolus_zero", 1, 2)
          if ( input$kmScaleInput ){
            tmpModel <- gsub(
              "<MM>",
              sprintf("(A(%s)/S%s)", value, value),
              tmpModel
            )
          } else {
            tmpModel <- gsub(
              "<MM>",
              sprintf("A(%s)", value),
              tmpModel
            )
          }
        }

        if ( input$absorptionInput == "transit" ){

          doseVariable <- "DOSEAMT"
          ndoseVariable <- "NDOSE"
          dosetimeVariable <- "DOSETIME"

          if ( length(input$dataInput) > 0 && length(vars()) > 0 ){
            while( doseVariable %in% vars() ){
              doseVariable <- paste0("Z", doseVariable)
            }
            while ( ndoseVariable %in% vars() ){
              ndoseVariable <- paste0("Z", ndoseVariable)
            }
            while ( dosetimeVariable %in% vars() ){
              dosetimeVariable <- paste0("Z", dosetimeVariable)
            }
          }

          tmpModel <- gsub("<dose>", doseVariable, tmpModel)
          tmpModel <- gsub("<ndose>", ndoseVariable, tmpModel)
          tmpModel <- gsub("<dosetime>", dosetimeVariable, tmpModel)

        }

        tmp <- c("  ; PK model equations", tmp, tmpModel, "  ")

      } else {

        # Get $DES code for non-PREDPP models
        tmpModel <- c()
        for (icmt in 1:nPKcmts){
          tmpModel <- c(
            tmpModel,
             sprintf(
               "  DADT(%s) = <Define ODE for A(%s)>",
               icmt,
               icmt
             )
          )
        }
        tmp <- c("  ; PK model equations", tmp, tmpModel, "  ")

      }
    }

    if ( input$pdInput %in% c("idr", "ode", "link") ){

      if ( input$pdInput == "idr" ){

        req( input$idrTypeInput, input$idrParmInput, input$idrStimInput )

        # Get $DES code for IDR and stimulation/inhibition models
        tmpModel <- parm_lib %>%
          dplyr::filter(
            TYPE == "idr" & FORM == input$idrTypeInput & TRANS == input$idrParmInput
          ) %>%
          dplyr::pull(DES)
        tmpStim <- parm_lib %>%
          dplyr::filter(
            TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
              FORM == input$idrStimInput
          ) %>%
          dplyr::pull(DES)

        tmpModel <- unlist(strsplit(tmpModel, split = "[|]"))
        tmpStim <- unlist(strsplit(tmpStim, split = "[|]"))

        # Input compartment number into IDR model equation
        tmpModel <- gsub("<1>", nPKcmts + 1, tmpModel)

        # Input effect driver in stimulation/inhibition models
        if ( input$effectCpDriverInput ){
          if ( input$pkInput == "pk" ){
            value <- ifelse(
              input$absorptionInput %in% c("zero", "bolus"),
              1,
              2
            )
          } else if ( input$pkInput %in% c("linmat", "ode") ){
            value <- as.numeric(input$pkDefaultObsInput)
          }
          driver <- sprintf(
            "A(%s)/%s",
            input$effectCmtDriverInput,
            ifelse(
              input$pkInput %in% c("pk", "linmat", "ode") &
                input$effectCmtDriverInput == (value + ifelse(input$pdInput == "link", 1, 0)),
              sprintf("S%s", value),
              sprintf("V%s", input$effectCmtDriverInput)
            )
          )
        } else {
          driver <- sprintf("A(%s)", input$effectCmtDriverInput)
        }
        tmpStim <- gsub(
          "<DRIVER>",
          ifelse(length(driver) == 0, "V", driver),
          tmpStim
        )

        tmp <- c(tmp, "  ; PD model equations", tmpStim, tmpModel, "  ")

      } else if ( input$pdInput == "ode" ){

        # Get $DES code for custom ODE models
        tmpModel <- c()
        for ( icmt in (nPKcmts + 1:nPDcmts) ){
          tmpModel <- c(
            tmpModel,
            sprintf("  DADT(%s) = <Define ODE for A(%s)>", icmt, icmt)
          )
        }

        tmp <- c(tmp, "  ; PD model equations", tmpModel, "  ")

      } else if ( input$pdInput == "link" ){

        # Get $DES code for biophase models
        tmpModel <- parm_lib %>%
          dplyr::filter(TYPE == "link") %>%
          dplyr::pull(DES)
        tmpModel <- unlist(strsplit(tmpModel, split = "[|]"))

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

        tmp <- c(tmp, "  ; Biophase equation", tmpModel, "  ")

      }

    }

    new <- sub(
      "@DES",
      paste("$DES", paste(tmp, collapse = "\n"), sep = "\n"),
      new
    )

  }

  new

}

#' Replace @ERROR tag
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
#'

replace_error <- function(
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

  req(advan, trans, parm_lib)

  if (isPRED()){
    new <- new[!grepl("@ERROR", new)]
  } else {

    isPK <- input$pkInput != "none"
    isPD <- input$pdInput != "none"

    if ( isPK ){
      req( input$pkRVInput )
    }
    if ( isPD ){
      req( input$pdRVInput )
    }

    tmp <- c()

    if ( isTruthy(input$mapTable) ){
      mapTable <- hot_to_r(input$mapTable)
      dvidVariable <- mapTable %>%
        dplyr::filter(Description == "Endpoint identifier variable") %>%
        dplyr::pull(Variable)
      blqVariable <- mapTable %>%
        dplyr::filter(Description == "BLQ variable") %>%
        dplyr::pull(Variable)
    } else {
      dvidVariable <- ""
      blqVariable <- ""
    }

    # RV model for PK
    if ( isPK ){

      # Get TMMD-specific code for $ERROR
      if ( grepl("tmdd", input$eliminationInput) ){
        index <- get_model_lib_index(
          input = input, advan = advan, trans = trans, parm_lib = parm_lib
        )
        tmp <- c(
          tmp,
          unlist(
            strsplit(
              parm_lib$ERROR[index],
              split = "[|]"
            )
          ),
          ""
        )
      }

      # If PKPD model, need to add some wording and DVID-based if statement
      if ( isPD ){
        tmp <- c(
          tmp,
          "  ; Residual variability for PK model",
          sprintf(
            "  IF (%s == 1) THEN <Check that endpoint value is appropriate>",
            ifelse(
              dvidVariable != "",
              dvidVariable,
              "<endpoint variable>"
            )
          )
        )
      }

      # Extract RV model
      tmpRV <- rv_lib %>%
        dplyr::filter(TYPE == input$pkRVInput)
      if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM_M3)

        if ( blqVariable != ""){
          tmpRV <- sub(
            "<BLQ>",
            blqVariable,
            tmpRV
          )
        }

      } else {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM)
      }
      tmpRV <- unlist(
        strsplit(
          tmpRV,
          split = "[|]"
        )
      )
      if ( length(tmpRV) == 0 ){
        tmpRV <- "  IPRED = <F>"
      }

      # Create flag variable for CCV and log error models
      if ( input$pkRVInput %in% c("ccv", "log") ){
        tmpRV <- c(
          "  FLAG = 0",
          "  IF (AMT > 0) FLAG = 1",
          if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
            "  IF (<F> == 0) FLAG = 1E-16"
          },
          tmpRV
        )
      }

      # Substitute F, DV, and EPS parameter indices
      if ( grepl("tmdd", input$eliminationInput) ){
        # if tmdd, we assume that DV and total concentrations but F does not always provide CTOT
        tmpRV <- gsub("<F>", "CTOT", tmpRV)
      } else {
        # F defines the default output if input$pkInput is "pk", "linmat", or "ode",
        tmpRV <- gsub("<F>", "F", tmpRV)
      }
      tmpRV <- gsub("<DV>", "DV", tmpRV)
      tmpRV <- gsub("<1>", 1, tmpRV)
      tmpRV <- gsub("<2>", 2, tmpRV)

      if ( isPD ){
        tmpRV <- c(
          paste0("  ", tmpRV),
          "  ENDIF"
        )
      }

      tmp <- c(tmp, tmpRV, "  ")

      nPKeps <- ifelse(input$pkRVInput == "accv", 2, 1)

    } else {
      nPKeps <- 0
    }

    # RV model for PD
    if ( isPD ){

      # If PKPD model, need to add some wording and DVID-based if statement
      if ( isPK ){
        tmp <- c(
          tmp,
          "  ; Residual variability for PD model",
          sprintf(
            "  IF (%s == 2) THEN <Check that endpoint value is appropriate>",
            ifelse(
              dvidVariable != "",
              dvidVariable,
              "<endpoint variable>"
            )
          )
        )
      }

      # Extract RV model
      tmpRV <- rv_lib %>%
        dplyr::filter(TYPE == input$pdRVInput)
      if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM_M3)

        if ( blqVariable != ""){
          tmpRV <- sub(
            "<BLQ>",
            blqVariable,
            tmpRV
          )
        }

      } else {
        tmpRV <- tmpRV %>%
          dplyr::pull(RV_NONMEM)
      }
      tmpRV <- unlist(
        strsplit(
          tmpRV,
          split = "[|]"
        )
      )
      if ( length(tmpRV) == 0 ){
        tmpRV <- "  IPRED = <F>"
      }

      if (input$pdInput %in% c("direct", "link")){

        req( input$effectFormInput, input$effectParmInput, input$effectStimInput )

        # Extract the type of model
        tmpModel <- paste0(
          "  IPRED = ",
          parm_lib %>%
            dplyr::filter(
              TYPE == "function" &
                FORM == input$effectFormInput &
                TRANS == input$effectParmInput &
                INCREASE == as.integer(as.logical(input$effectStimInput))
            ) %>%
            dplyr::pull(PRED)
        )
        tmpModel <- unlist(
          strsplit(
            tmpModel,
            split = "[|]"
          )
        )

        # Adjust the effect driver
        if ( input$pkInput == "pk" ){
          value <- ifelse(input$absorptionInput %in% c("zero", "bolus"), 1, 2)
        } else if ( input$pkInput %in% c("linmat", "ode" )){
          value <- as.numeric(input$pkDefaultObsInput)
        }
        tmpModel <- gsub(
          "<x>",
          ifelse(
            input$effectCpDriverInput,
            sprintf(
              "(A(%s)/%s)",
              input$effectCmtDriverInput,
              ifelse(
                input$pkInput %in% c("pk", "linmat", "ode") &
                  input$effectCmtDriverInput == (value + ifelse(input$pdInput == "link", 1, 0)),
                sprintf("S%s", value),
                sprintf("V%s", input$effectCmtDriverInput)
              )
            ),
            sprintf("A(%s)", input$effectCmtDriverInput)
          ),
          tmpModel
        )
        if ( as.logical(input$effectCpDriverInput) ){
          tmpModel <- paste(
            tmpModel,
            "<Check that the volume/scaling parameter is appropriate>"
          )
        }

        tmpModel <- gsub("IPRED", "RESP", tmpModel)

        # Substitute F, DV, and EPS parameter indices
        tmpRV <- gsub("<F>", "RESP", tmpRV)
        tmpRV <- gsub("<DV>", "DV", tmpRV)
        tmpRV <- gsub("<1>", nPKeps + 1, tmpRV)
        tmpRV <- gsub("<2>", nPKeps + 2, tmpRV)

        # Create flag variable for CCV and log error models
        if ( input$pdRVInput %in% c("ccv", "log") ){
          tmpRV <- c(
            tmpModel,
            "  FLAG = 0",
            if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
              "  IF (RESP == 0) FLAG = 1E-16"
            },
            tmpRV
          )
        } else {
          tmpRV <- c(tmpModel, tmpRV)
        }

      } else if ( input$pdInput == "idr" ){

        # Substitute F, DV, and EPS parameter indices
        tmpRV <- gsub("<F>", sprintf("A(%s)", nPKcmts + 1), tmpRV)
        tmpRV <- gsub("<DV>", "DV", tmpRV)
        tmpRV <- gsub("<1>", nPKeps + 1, tmpRV)
        tmpRV <- gsub("<2>", nPKeps + 2, tmpRV)

        # Create flag variable for CCV and log error models
        if ( input$pdRVInput %in% c("ccv", "log") ){
          tmpRV <- c(
            "  FLAG = 0",
            if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
              sprintf("  IF (A(%s) == 0) FLAG = 1E-16", nPKcmts + 1)
            },
            tmpRV
          )
        }

      } else if ( input$pdInput == "ode" ){

        # Substitute F
        tmpRV <- gsub(
          "<F>",
          "A(<Insert appropriate compartment number>)",
          tmpRV
        )

        # Substitute DV
        tmpRV <- gsub("<DV>", "DV", tmpRV)

        # Substitute EPS parameter indices
        tmpRV <- gsub("<1>", nPKeps + 1, tmpRV)
        tmpRV <- gsub("<2>", nPKeps + 2, tmpRV)

        # Create flag variable for CCV and log error models
        if (input$pdRVInput %in% c("ccv", "log")){
          tmpRV <- c(
            "  FLAG = 0",
            if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
              "  IF (A(<Insert appropriate compartment number>) == 0) FLAG = 1E-16"
            },
            tmpRV
          )
        }

      }

      if ( isPK ){
        tmpRV <- c(
          paste0("  ", tmpRV),
          "  ENDIF"
        )
      }

      tmp <- c(tmp, tmpRV, "  ")

    }

    new <- sub(
      "@ERROR",
      paste("$ERROR", paste(tmp, collapse = "\n"), sep = "\n"),
      new
      )

  }

  new

}

#' Replace @TASK tag

replace_task <- function(
    input,
    new,
    estimations,
    isODE
) {

  req( input$estimationInput )
browser()
  tmp <- c()

  # Get $ESTIMATION step
  if ( input$estimationInput ){
    tmp <- sapply(
      1:5,
      function(x, estimations, input){
        if ( estimations$Method[x] %in% c("FO", "FOCE") ) {
          # Classic estimation methods
          sprintf(
            "$ESTIMATION %s%s%s%s%sPRINT=1 MAXEVAL=9999 NSIG=%s %sSORT\n  MSFO=%s.msf",
            ifelse(
              estimations$Method[x] == "FO",
              "0 POSTHOC ",
              "CONDITIONAL "
            ),
            ifelse(
              estimations$Method[x] == "FOCE" & estimations$Interaction[x] != "" && estimations$Interaction[x] == "Yes",
              "INTERACTION ",
              ""
            ),
            ifelse(
              estimations$Likelihood[x] != "" && estimations$Likelihood[x] == "Yes",
              "LIKELIHOOD ",
              ""
            ),
            ifelse(
              estimations$NoPrediction[x] != "" && estimations$NoPrediction[x] == "Yes",
              "NOPREDICTION ",
              ""
            ),
            ifelse(
              estimations$Options[x]!="",
              paste0(estimations$Options[x], " "),
              ""
            ),
            input$nsigInput,
            ifelse(
              isODE(),
              sprintf("SIGL=%s ", 3*as.numeric(input$nsigInput)),
              ""
            ),
            ifelse(
              input$probInput != "",
              sub(".ctl", "", input$probInput),
              "<Enter the control stream name>"
            )
          )
        } else if (estimations$Method[x] %in% c("ITS", "SAEM", "IMP", "BAYES")){
          # EM algorithms
          sprintf(
            "$ESTIMATION METHOD=%s %s%s%s%sPRINT=1 NSIG=%s %sSORT\n  MSFO=%s.msf",
            estimations$Method[x],
            ifelse(
              estimations$Interaction[x] != "" && estimations$Interaction[x] == "Yes",
              "INTERACTION ",
              ""
            ),
            ifelse(
              estimations$Likelihood[x] != "" && estimations$Likelihood[x] == "Yes",
              "LIKELIHOOD ",
              ""
            ),
            ifelse(
              estimations$NoPrediction[x] != "" && estimations$NoPrediction[x] == "Yes",
              "NOPREDICTION ",
              ""
            ),
            ifelse(
              estimations$Options[x]!="",
              paste0(estimations$Options[x], " "),
              ""
            ),
            input$nsigInput,
            ifelse(
              isODE(),
              sprintf("SIGL=%s ", 3*as.numeric(input$nsigInput)),
              ""
            ),
            ifelse(
              input$probInput != "",
              sub(".ctl", "", input$probInput),
              "<Enter the control stream name>"
            )
          )
        } else {
          # case when method is set to "none"
          NULL
        }
      },
      estimations,
      input
    )

    tmp <- c(unlist(tmp), "")

  }

  # Get $COVARIANCE
  if ( input$covInput ){
    tmp <- c(
      tmp,
      "$COVARIANCE PRINT=E",
      ""
    )
  }

  # Get $SIMULATION
  if ( input$simInput ){
    if ( any(grepl("CALL RANDOM", new)) ){
      tmp <- c(
        tmp,
        sprintf(
          "$SIMULATION ONLYSIMULATION NSUB=%s\n  (%s) (%s UNIFORM) NOPREDICTION",
          input$nsubInput,
          input$simSeedInput
        ),
        ""
      )
    } else {
      tmp <- c(
        tmp,
        sprintf(
          "$SIMULATION ONLYSIMULATION NSUB=%s (%s)",
          input$nsubInput,
          input$simSeedInput,
          get_seed()
        ),
        ""
      )
    }

  }

  new <- sub("@TASK", paste(tmp, collapse = "\n"), new)

  new

}
