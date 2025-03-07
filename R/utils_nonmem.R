
#' Creation of NONMEM code
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
    replacement = TRUE
) {

  new <- template

  if ( !replacement ){
    return(new)
  }

  debug <- FALSE

  user <- Sys.info()["user"]
  date <- format(Sys.time(), "%b %d, %Y %H:%M:%S %Z")


  # Extract tables content
  parms <- hot_to_r(input$parameterTable)
  estimations <- hot_to_r(input$estimationTable)

  # Replace @TIMESTAMP
  new <- sub("@TIMESTAMP", date, new)

  # Replace @USER
  new <- sub("@USER", user, new)

  # Replace @VERSION
  new <- sub("@VERSION", input$platformVersionInput, new)

  # Replace @PROB1 and @PROB2
  if ( isTruthy(input$modelInput) ){
    if ( debug ) message("PROBLEM")
    new <- replace_problem(input = input, new = new)
  }

  # Replace @PURPOSE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("PURPOSE")
    new <- replace_purpose(input = input, new = new, parms = parms, varianceTable = varianceTable)
  }

  # Replace @PATH
  if ( debug ) message("PATH")
  new <- replace_path(input = input, new = new)

  # Replace @INPUT
  if ( debug ) message("INPUT")
  new <- replace_input(input = input, new = new, vars = vars)

  # Replace @DATA
  if ( debug ) message("DATA")
  new <- replace_data(input = input, new = new)

  # Replace @SUBROUTINE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("SUBROUTINE")
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
    if ( debug ) message("MODEL")
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
    if ( debug ) message("ABBREVIATED")
    new <- replace_abbreviated(input = input, new = new, vars = vars)
  }

  # Replace @THETA
  if (isTruthy(parms) ){
    if ( debug ) message("THETA")
    new <- replace_theta(new = new, parms = parms)
  }

  # Replace @OMEGA
  if ( areTruthy(parms, covarianceBlock) ){
    if ( debug ) message("OMEGA")
    new <- replace_omega(
      new = new,
      parms = parms,
      varianceTable = varianceTable,
      blocks = covarianceBlock)
  }

  # Replace @SIGMA
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("SIGMA")
    new <- replace_sigma(input = input, new = new, rvTable = rvTable)
  }

  # Create @PRIOR
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("PRIOR")
    new <- replace_prior(
      input = input,
      new = new,
      parms = parms,
      varianceTable = varianceTable,
      estimations = estimations
    )
  }

  # Create lines of preamble code
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("preamble")
    preamble_code <- get_preamble_code(
      input = input,
      parms = parms,
      vars = vars
    )
  }

  # Create code lines for PK, PD, and other parameters
  if ( areTruthy(input$pkInput, input$pdInput, varianceTable) ){
    if ( debug ) message("parms")
    parms_code <- get_parms_code(
      input = input,
      parms = parms,
      varianceTable = varianceTable,
      mu = as.logical(input$muInput)
    )
  }

  # Create code lines for derived parameters
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("derived")
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
    if ( debug ) message("scaling")
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
    if ( debug ) message("cmts")
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
    if ( debug ) message("init")
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
    if ( debug ) message("PK/PRED")
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
      rv_lib = rv_lib,
      mu = as.logical(input$muInput)
    )
    if ( as.logical(input$muInput) ){
      new <- sub(
        '^[$]PK',
        paste(
          '$PK',
          '  ; Note that MU_ variable definitions cannot include time-varying covariates\n',
          sep = '\n'
        ),
        new
      )
    }
  }

  # Replace @DES
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("DES")
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
    if ( debug ) message("ERROR")
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
    if ( debug ) message("TASKS")
    new <- replace_task(
      input = input,
      new = new,
      estimations = estimations,
      isODE = isODE
    )
  }


  # Replace @TABLE
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("TABLE")
    new <- replace_table(
      input = input,
      new = new,
      vars = vars
    )
  }

  # Replace @TAGS
  if ( areTruthy(input$pkInput, input$pdInput) ){
    if ( debug ) message("TAGS")
    new <- replace_tags(
      input = input,
      new = new
    )
  }

  if ( debug )
    message(
      paste(new, collapse = "\n")
    )

  # Dummy call to implement manual refresh
  dummy <- input$refreshButton

  # Remove all tags for clean-up
  new[ !grepl("@[A-Z]+", new)]

}


#' Replacement of @PROB1 and @PROB2 tags
#'
#' @param input Internal parameter for \code{shiny}
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
#' @param input Internal parameter for \code{shiny}
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
      glue::glue(
        "$INPUT {input}\n",
        input = paste(varLines, collapse = "\n  "),
        .trim = FALSE
      ),
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
#' @param input Internal parameter for \code{shiny}
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
    glue::glue(
      "$DATA {data}\n  IGNORE=@\n",
      data = ifelse(
        length(dataPath) > 0,
        dataPath,
        "<path to dataset>"
      ),
      .trim = FALSE
    ),
    new
  )

}


#' Replacement of @SUBROUTINE tag
#'
#' @param input Internal parameter for \code{shiny}
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
    new <- new[!grepl("@SUBROUTINE", new)]
  } else {
    tmp <- ""

    if ( isODE() | isLINMAT() ){
      tmp <- glue::glue(
        "$SUBROUTINES ADVAN{input$advanInput} TRANS1 TOL={tol}\n",
        tol = ifelse(
          isTruthy(input$nsigInput),
          3*as.numeric(input$nsigInput),
          3
        ),
        .trim = FALSE
      )
    } else {
      tmp <- glue::glue(
        "$SUBROUTINES ADVAN{advan()} TRANS{trans()}\n",
        .trim = FALSE
      )
    }

    new <- sub("@SUBROUTINE", tmp, new)

  }

  return(new)

}

#' Replace @MODEL tag
#'
#' @param input Internal parameter for \code{shiny}
#' @param new Text template
#' @param model_lib Library for $MODEL replacement
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isPREDPP Reactive object - is mode coded with $PK?
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

      if ( grepl("tmdd", input$eliminationInput) ) {
        tmp <- tmp %>%
          dplyr::filter(.data$ELIMINATION == input$eliminationInput) %>%
          dplyr::pull(.data$NONMEM) %>%
          strsplit(split = "[|]") %>%
          unlist()
      } else {
        tmp <- tmp %>%
          dplyr::filter(.data$ELIMINATION == "mmlin") %>%
          dplyr::pull(.data$NONMEM) %>%
          strsplit(split = "[|]") %>%
          unlist()
      }
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
      "biophase" = 1,
      "idr" = 1,
      "ode" = input$pdnCMTInput,
      0
    )

    # Start of $MODEL statement replacement
    tmp <- c(
      glue::glue("$MODEL NCOMPARTMENTS={nPKcmts + nPDcmts}"),
      tmp
    )

    # Add PK compartments if pkInput is not "pk"
    if ( input$pkInput != "pk" & nPKcmts > 0 ){
      for ( iCMT in 1:nPKcmts ){
        tmp <- c(
          tmp,
          glue::glue(
            "  COMP = (COMP{iCMT}{defdose}{defobs})",
            defdose = ifelse(as.numeric(input$pkDefaultDoseInput) == iCMT, " DEFDOSE", ""),
            defobs = ifelse(as.numeric(input$pkDefaultObsInput) == iCMT, " DEFOBS", "")
          )
        )
      }
    }

    # Add PD compartments
    if ( nPDcmts > 0) {
      for ( iCMT in (nPKcmts + 1:nPDcmts) ){
        if (input$pdInput == "biophase"){
          tmp <- c(tmp, "  COMP=(BIOPHASE)")
        } else if (input$pdInput == "idr"){
          tmp <- c(tmp, "  COMP=(IDR)")
        } else {
          tmp <- c(tmp, glue::glue("  COMP=(COMP{iCMT})"))
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
#' @param input Internal parameter for \code{shiny}
#' @param new Text template
#' @param vars Character vector of variable names

replace_abbreviated <- function(
    input,
    new,
    vars
){

  # $ABBREVIATED is only required for transit compartment absorption model
  if ( !(input$pkInput == "pk" && input$poInput == "transit") ){
    tmp <- new[ !grepl("@ABBREVIATED", new) ]
  } else {

    # Ensure uniqueness of dosing info matrices
    doseAmount <- "DOSEAMT"
    doseCounter <- "DOSECNT"
    doseTime <- "DOSETIME"

    while( any(grepl(doseAmount, vars())) ){
      doseAmount <- glue::glue("Z{doseAmount}")
    }

    while ( any(grepl(doseCounter, vars())) ){
      doseCounter <- glue::glue("Z{doseCounter}")
    }

    while ( any(grepl(doseTime, vars())) ){
      doseTime <- glue::glue("Z{doseTime}")
    }

    # Ensure uniqueness of matrix variables
    tmp <- c(
      glue::glue(
        "$ABBREVIATED DECLARE {doseTime}(100),{doseAmount}(100)    ; 100 is maximum number of doses per ID"
      ),
      "$ABBREVIATED DECLARE DOWHILE I",
      glue::glue("$ABBREVIATED DECLARE DOWHILE {doseCounter}")
    )
    tmp <- sub(
      "@ABBREVIATED",
      paste0( paste(tmp, collapse = "\n"), "\n" ),
      new
    )
  }

  tmp

}

#' Replacement of @THETA tag
#'
#' @param new Text template
#' @param parms Parameter selection

replace_theta <- function(
    new,
    parms
){

  if ( !"Fixed" %in% names(parms) ){
    # Platform switch to NONMEM
    parms$Fixed <- "No"
  }

  tmp <- c(
    "$THETA",
    sapply(
      1:nrow(parms),
      function(x, parms){
        glue::glue(
          "  ({min}, {init}, {max})  {fix};--th{x}- {parm}: {label}{unit}{transform}",
          min = scale_value( parms$Min[x], parms$Min[x], parms$Max[x], parms$Scale[x] ),
          init = scale_value( parms$Min[x], parms$Initial[x], parms$Max[x], parms$Scale[x] ),
          max = scale_value( parms$Min[x], parms$Max[x], parms$Max[x], parms$Scale[x] ),
          fix = ifelse(parms$Fixed[x] == "Yes", "FIXED ", ""),
          parm = parms$Parameter[x],
          label = ifelse(
            parms$Label[x] == "",
            glue::glue("Label for THETA{x}"),
            sub("\\s+[(]logit[)]", "", parms$Label[x])
          ),
          unit = ifelse(
            parms$Unit[x] == "",
            " (unitless)",
            glue::glue(" ({parms$Unit[x]})")
          ),
          transform = dplyr::case_when(
            grepl("[(]logit[)]", parms$Label[x]) ~ " [logit]",
            parms$Scale[x] == "Linear" ~ "",
            TRUE ~ glue::glue(" [{tolower(parms$Scale[x])}]")
          )
        )
      },
      parms
    ),
    ""
  )

  # Align comments
  tmp <- align_tags(code = tmp)

  # Replace
  new <- sub("@THETA", paste(tmp, collapse = "\n"), new)

  new

}


#' Replacement of @OMEGA tag
#'
#' @param new Text template
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param blocks Variance - covariance matrix
#'

replace_omega <- function(
    new,
    parms,
    varianceTable,
    blocks
){

  if ( !all(varianceTable$Variability == "None") ){

    # Add the theta numbers in parameter table
    parms$TH <- 1:nrow(parms)

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
        tmp <- c(tmp, glue::glue("$OMEGA BLOCK({nrow(omega)})") )
      } else if ( type == "diagonal" & previousType != "diagonal" ){
        tmp <- c(tmp, "$OMEGA")
      }

      # Add omega value and tag
      for ( i in 1:nrow(omega) ){
        ieta <- ieta + 1
        index <- which(parms$Parameter == rownames(omega)[i])
        variability <- varianceTable$Variability[index]
        scale <- parms$Scale[index]
        parm_min <- parms$Min[index]
        parm_max <- parms$Max[index]
        theta <- paste0('theta', parms$TH[index])
        tmp <- c(
          tmp,
          glue::glue(
            "  {init};--eta{ieta}- IIV in {label} [{model}]",
            init = ifelse(
              type == "diagonal",
              # Variability without correlation
              glue::glue("{omega[i, i]}  "),
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
            label = parms$Parameter[index],
            model = dplyr::case_when(
              variability == "Additive" ~ "add",
              variability == "Exponential" ~ "exp",
              scale == "Linear" & variability == "Logit" & parm_min == 0 & parm_max == 1 ~
                # Parameter between 0 and 1
                glue::glue("cv=100*(1-{theta})*sqrt(eta{ieta})"),
              scale == "Linear" & variability == "Logit" & parm_min == 0 & parm_max != 1 ~
                # Parameter between 0 and max > min
                glue::glue("cv=100*(1-{theta}/{parm_max})*sqrt(eta{ieta})"),
              scale == "Linear" & variability == "Logit" & parm_min != 0  ~
                # Parameter between min > 0 and max > min
                glue::glue("cv=100*(({theta}-{parm_min})*({parm_max}-{theta}))/({theta}*({parm_max}-{parm_min}))*sqrt(eta{ieta})"),
              scale == "Logit" & variability == "Logit" & parm_min == 0 & parm_max == 1 ~
                # Parameter between 0 and 1
                glue::glue("cv=100*(1-inv_logit({theta}))*sqrt(eta{ieta})"),
              scale == "Logit" & variability == "Logit" & parm_min == 0 & parm_max != 1 ~
                # Parameter between 0 and max > min
                glue::glue("cv=100*(1-inv_logit({theta},0,{parm_max})/{parm_max})*sqrt(eta{ieta})"),
              scale == "Logit" & variability == "Logit" & parm_min != 0  ~
                # Parameter between min > 0 and max > min
                glue::glue("cv=100*((inv_logit({theta},{parm_min},{parm_max})-{parm_min})*({parm_max}-inv_logit({theta},{parm_min},{parm_max})))/(inv_logit({theta},{parm_min},{parm_max})*({parm_max}-{parm_min}))*sqrt(eta{ieta})")
            )
          )
        )
      }

    }

    tmp <- c(tmp, "  ")

    # Align tags
    tmp <- align_tags(tmp)

    # Replace
    new <- sub("@OMEGA", paste(tmp, collapse = "\n"), new)

  } else {
    new <- new[ !grepl("@OMEGA", new) ]
  }

  new

}

#' Replacement of @SIGMA tag
#'
#' @param new Text template
#' @param input Internal parameter for \code{shiny}
#' @param rvTable Residual variability selection
#'

replace_sigma <- function(new, input, rvTable){

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

    req(input$pkRVInput)

    if ( input$pkRVInput %in% c("add", "log", "ccv") ){
      tmp <- c(
        tmp,
        glue::glue(
          "  {rvTable$Variance[ieps]}  ;--eps{ieps}- Residual variability {sup}{log}[{model}]",
          sup = ifelse(input$pdInput != "none", "for PK ", ""),
          log = ifelse(input$pkRVInput == "log", "(log units) ", ""),
          model = ifelse(input$pkRVInput == "ccv", "ccv", "add")
        )
      )
      ieps <- ieps + 1
    } else if ( input$pkRVInput == "accv" ){
      tmp <- c(
        tmp,
        glue::glue(
          "  {rvTable$Variance[ieps]}  ;--eps{ieps}- Constant CV residual variability component {sup}[accv1=eps{ieps}-ccv;eps{ieps+1}-add]",
          sup = ifelse(input$pdInput != "none", "for PK ","")
        ),
        glue::glue(
          "  {rvTable$Variance[ieps + 1]}  ;--eps{ieps+1}- Additive residual variability component {sup}[accv1]",
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
          "  {rvTable$Variance[ieps]}  ;--eps{ieps}- Residual variability {sup}{log}[{model}]",
          sup = ifelse(input$pkInput != "none", "for PD ", ""),
          log = ifelse(input$pdRVInput == "log", "(log units) ", ""),
          model = ifelse(input$pdRVInput == "ccv", "ccv", "add")
        )
      )
    } else if ( input$pdRVInput == "accv" ){
      tmp <- c(
        tmp,
        glue::glue(
          "  {rvTable$Variance[ieps]}  ;--eps{ieps}- Constant CV RV component {sup}[accv{model}=eps{ieps}-ccv;eps{ieps+1}-add]",
          sup = ifelse(input$pkInput != "none", "for PD ",""),
          model = ifelse(input$pkRVInput == "accv", 2, 1)
        ),
        glue::glue(
          "  {rvTable$Variance[ieps + 1]}  ;--eps{ieps+1}- Additive RV component {sup}[accv{model}]",
          sup = ifelse(input$pkInput != "none", "for PD ",""),
          model = ifelse(input$pkRVInput == "accv", 2, 1)
        )
      )
    }
  }

  if ( length(tmp) == 0 ){
    new <- new[!grepl("@SIGMA", new)]
  } else {
    # Align tags
    tmp <- align_tags(code = tmp)

    # Add prediction tag for ACCV residual variability models
    if ( input$pkInput != "none" && input$pkRVInput == "accv" ){
      tmp <- c(
        tmp,
        "  ",
        ";--pred[accv1]=0.1,1,10"
      )
    }
    if ( input$pdInput != "none"  && input$pdRVInput == "accv" ){
      if ( input$pdInput != "none" && input$pkRVInput == "accv" ) {
        tmp <- c(
          tmp,
          ";--pred[accv2]=0.1,1,10"
        )
      } else {
        tmp <- c(
          tmp,
          "  ",
          ";--pred[accv1]=0.1,1,10"
        )
      }
    }

    # Replace
    new <- sub(
      "@SIGMA",
      paste(
        c("$SIGMA", tmp, ""),
        collapse = "\n"
      ),
      new
    )
  }

  return(new)

}

#' Replacement of @PRIOR tag
#'
#' @param new Text template
#' @param input Internal parameter for \code{shiny}
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param estimations Table of estimation tasks

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
          glue::glue(
            "  {parms$Initial[x]} FIXED ; Prior information for {parms$Parameter[x]}"
          )
        },
        parms),
      ""
    )

    thetap <- align_tags(code = thetap)

    # THETAPV confidence around the priors on THETAs
    thetapv <- glue::glue(
      "$THETAPV BLOCK({nrow(parms)}) FIXED VALUES(10000, 0.0)",
    )

    # OMEGAP prior information about OMEGAs
    omegap <- c()

    if ( any(varianceTable$Variability != "None") ){
      omegaParameters <- which(varianceTable$Variability != "None")
      omegap <- glue::glue(
        "$OMEGAP BLOCK({length(omegaParameters)})",
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
            glue::glue(
              "; Prior information for IIV in {parms$Parameter[omegaParameter]}"
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
            glue::glue(
              "  1.0 FIXED ; Prior information for residual variability {sup}{log}",
              sup = ifelse(input$pdInput != "none", "for PK ", ""),
              log = ifelse(input$pkRVInput == "log", "(log units", "")
            )
          )
        } else {
          sigmap <- c(
            sigmap,
            glue::glue(
              "  0.2 FIXED ; Prior information for constant CV RV component{sup}",
              sup = ifelse(input$pdInput != "none", " for PK ", "")
            ),
            glue::glue(
              "  1.0 FIXED ; Prior information for additive RV component{sup}",
              sup = ifelse(input$pdInput != "none", " for PK ", "")
            )
          )
        }
      }

      if ( input$pdInput != "none" && input$pdRVInput != "none" ){
        if ( input$pdRVInput %in% c("add", "log", "ccv") ){
          sigmap <- c(
            sigmap,
            glue::glue(
              "  1.0 FIXED ; Prior information for residual variability {sup}{log}",
              sup = ifelse(input$pkInput != "none", "for PD ", ""),
              log = ifelse(input$pdRVInput == "log", "(log units", "")
            )
          )
        } else {
          sigmap <- c(
            sigmap,
            glue::glue(
              "  0.2 FIXED ; Prior information for constant CV RV component{sup}",
              sup = ifelse(input$pkInput != "none", " for PD ", "")
            ),
            glue::glue(
              "  1.0 FIXED ; Prior information for additive RV component{sup}",
              sup = ifelse(input$pkInput != "none", " for PD ", "")
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
#' @param input Internal parameter for \code{shiny}
#' @param new Text template
#' @param preamble_code Preamble code
#' @param parms_code Typical and individual parameter code
#' @param derived_parms_code Derived parameter code
#' @param scaling_code Dose scaling and bioavailability code
#' @param init_code Compartment initialization code
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param parm_lib Library of parameters
#' @param rv_lib  Library for residual variability replacement
#' @param mu A logical indicator for mu transformation

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
    rv_lib,
    mu
){

  if ( isTruthy(input$mapTable) ){
    dvidVar <- hot_to_r(input$mapTable) %>%
      dplyr::filter(.data$Description == "Endpoint identifier variable") %>%
      dplyr::pull(.data$Variable) %>%
      as.character()
  } else {
    dvidVar <- NULL
  }

  if ( !isPRED() ){

    if ( input$ivInput != "none" & input$poInput != "none" ){

      # Extract the line of code where bioavailability is defined
      bio <- hot_to_r( input$parameterTable ) %>%
        dplyr::filter( .data$Type == "PK" ) %>%
        dplyr::filter( grepl("^F[0-9]", .data$Parameter) ) %>%
        dplyr::pull( .data$Parameter ) %>%
        as.character()

      if ( length(bio) > 0 ){
        f_row <- which(
          grepl( glue::glue("^\\s+{bio}\\s+=.*TV{bio}"), parms_code$PK )  |
            grepl( glue::glue("^\\s+{bio}\\s+=.*L{bio}"), parms_code$PK )
        )
        if ( mu & length(f_row) == 0 ){
          f_row <- which(
            grepl( glue::glue("^\\s+{bio}\\s+=.*MU_"), parms_code$PK )
          )
        }
        f_row <- f_row[1]
        f_line <- parms_code$PK[f_row]
        parms_code$PK <- parms_code$PK[-f_row]

        # Substitute in the scaling code
        f_row <- which(
          grepl( "^\\s+F[0-9]+\\s+=", scaling_code )
        )
        scaling_code[f_row] <- f_line
      }

    }

    new <- new[ !grepl("@PRED", new) ]
    tmp <- sub(
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

  } else {
    new <- new[ !grepl("@PK", new) ]

    # Add preamble
    tmp <- preamble_code

    # Add parameter definition
    tmp <- c(tmp, unlist(parms_code), "")

    isPK <- input$pkInput != "none"
    isPD <- input$pdInput != "none"

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
          glue::glue(
            "  IF ({dvid} == 1) THEN <Check that endpoint value is appropriate>",
            dvid = ifelse(
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
        dplyr::filter(.data$TYPE == input$pkRVInput)

      if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
        tmpRV <- tmpRV %>%
          dplyr::pull(.data$RV_NONMEM_M3)

        blqVariable <- ""

        if ( isTruthy(input$mapTable) ){
          blqVariable <- hot_to_r(input$mapTable) %>%
            dplyr::filter(.data$Description == "BLQ variable") %>%
            dplyr::pull(.data$Variable) %>%
            as.character()
        }

        if ( blqVariable != "" ){
          tmpRV <- sub(
            "<BLQ>",
            blqVariable,
            tmpRV
          )
        }

      } else {
        tmpRV <- tmpRV %>%
          dplyr::pull(.data$RV_NONMEM)
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
          glue::glue(
            "  IF ({dvid} == 2) THEN <Check that endpoint value is appropriate>",
            dvid = ifelse(
              length(dvidVar) > 0 && dvidVar != "",
              dvidVar,
              "<endpoint variable>"
            )
          )
        )
      } else if ( !input$pdInput %in% c("logistic", "ordcat") ) {
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
        dplyr::filter(.data$TYPE == input$pdRVInput)
      if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
        tmpRV <- tmpRV %>%
          dplyr::pull(.data$RV_NONMEM_M3)

        if ( blqVariable != ""){
          tmpRV <- sub(
            "<BLQ>",
            blqVariable,
            tmpRV
          )
        }

      } else {
        tmpRV <- tmpRV %>%
          dplyr::pull(.data$RV_NONMEM)
      }
      if ( input$pdRVInput == "none" ) {
        if ( isPK & input$pkRVInput != "none" ){
          tmpRV <- "  IPRED = <F>|  IRES = 0|  IWRES = 0|  |  Y = IPRED"
        } else {
          tmpRV <- "  IPRED = <F>|  |  Y = IPRED"
        }
      }
      tmpRV <- unlist(strsplit(tmpRV, split = "[|]"))

      if ( input$pdInput == "logistic" ){

        req(
          areTruthy(
            input$effectFormInput,input$effectParmInput, input$effectStimInput
          )
        )

        # Add code for logistic regression
        tmpModel <- parm_lib %>%
          dplyr::filter(.data$TYPE == "logistic") %>%
          dplyr::pull(.data$PRED) %>%
          strsplit(split = "[|]") %>%
          unlist()

        # Extract the drug effect function
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
        logitFun <- parm_lib_index %>% dplyr::pull(.data$PRED)

        # Find the baseline parameter to replace in logitFun
        tmpModelParm <- parm_lib %>%
          dplyr::filter(.data$TYPE == "logistic") %>%
          dplyr::pull(.data$PARMS)
        minimumParm <- parm_lib_index %>%
          dplyr::pull(.data$PARMS) %>%
          strsplit(split = "[|]") %>%
          unlist()
        minimumParm <- minimumParm[1]

        # Get logistic regression code
        tmpModel <- sub("<LOGIT>", sub(minimumParm, tmpModelParm, logitFun), tmpModel)
        if ( isTruthy(input$logisticVarInput) ){
          tmpModel <- gsub("<x>", input$logisticVarInput, tmpModel)
        } else if ( isTruthy(input$logisticVarTextInput) ){
          tmpModel <- gsub("<x>", input$logisticVarTextInput, tmpModel)
        }
        tmpRV <- tmpModel

      } else if ( input$pdInput == "ordcat" ){
        req(
          areTruthy(
            input$minCategoryInput, input$maxCategoryInput,
            input$effectFormInput, input$effectParmInput, input$effectStimInput
          )
        )

        minCategory <- min(
          floor( c(input$minCategoryInput, input$maxCategoryInput) )
        )
        maxCategory <- max(
          ceiling( c(input$minCategoryInput, input$maxCategoryInput) )
        )
        event <- ifelse(
          input$endpointInput == "",
          "EVENT",
          input$endpointInput
        )

        # Add code for ordered categorical model
        tmpModel <- parm_lib %>%
          dplyr::filter(.data$TYPE == "ordcat") %>%
          dplyr::pull(.data$PRED) %>%
          strsplit(split = "[|]") %>%
          unlist()

        # Get the drug effect function
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
        logitFun <- parm_lib_index %>% dplyr::pull(.data$PRED)

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
        effect <- glue::glue("  EFFECT = {effect}")

        # Create replacement code lines
        logit <- logit_indiv <- proba <- yn <- ind0 <- ind1 <- c()
        y <- glue::glue("  Y = Y{minCategory}*IND{minCategory}")
        sim <- c(
          glue::glue("  {event} = DV"),
          "  IF (ICALL == 4) THEN",
          "    CALL RANDOM(2, R)",
          glue::glue("    {event} = {minCategory}")
        )

        for ( i in minCategory:maxCategory ) {
          if ( i < maxCategory ){
            logit <- c(
              logit,
              glue::glue(
                "  TVLG{i} = TV{parm}{fun}EFFECT",
                parm = parms$Parameter[i - minCategory + 1],
                fun = ifelse(addFun, " + ", "*")
              )
            )
            if ( i == minCategory ){
              logit_indiv <- c(
                logit_indiv,
                get_individual_parm_code(
                  utils::modifyList(parms, list(Parameter = parms$SourceParam[1])),
                  varianceTable,
                  1,
                  1,
                  FALSE
                )
              )
            }
            logit_indiv <- c(
              logit_indiv,
              get_individual_parm_code(
                utils::modifyList(parms, list(Parameter = paste0("LG", i))),
                varianceTable,
                1,
                1,
                FALSE
              )
            )
            proba <- c(
              proba,
              glue::glue("  P{i} = 1/(1 + EXP(-LG{i}))  ; P(Y<={i}|X)")
            )
          }
          yn <- c(
            yn,
            ifelse(
              i == minCategory,
              glue::glue("  Y{i} = P{i}  ; P(Y={i}|X)"),
              glue::glue(
                "  Y{i} = {val} - P{i-1}  ; P(Y={i}|X)",
                val = ifelse(i == maxCategory, "1", glue::glue("P{i}"))
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
              glue::glue("    IF (R > P{i}) {event} = {i+1}")
            )
          } else{
            sim <- c(sim, "  ENDIF")
          }
        }

        # Align comments in proba and yn
        proba <- align_tags(code = proba)
        yn <- align_tags(code = yn)

        # Add logistic regression code
        tmpModel <- sub("<EFFECT>", effect, tmpModel)
        tmpModel <- sub(
          "<LOGIT>",
          paste(c(logit, "  ", logit_indiv), collapse = "\n"),
          tmpModel
        )
        tmpModel <- sub("<PROBA>", paste(proba, collapse = "\n"), tmpModel)
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
        if ( isTruthy(input$logisticVarInput) ){
          tmpModel <- gsub("<x>", input$logisticVarInput, tmpModel)
        } else if ( isTruthy(input$logisticVarTextInput) ){
          tmpModel <- gsub("<x>", input$logisticVarTextInput, tmpModel)
        }

        tmpRV <- tmpModel

      } else if ( input$pdInput == "er" ){

        req(
          areTruthy(
            input$effectFormInput, input$effectParmInput, input$effectStimInput
          )
        )

        # Add code for exposure-response
        tmpModel <- glue::glue(
          "  RESP = {pred}",
          pred = parm_lib %>%
            dplyr::filter(
              .data$TYPE == "function" &
                .data$FORM == input$effectFormInput &
                .data$TRANS == input$effectParmInput &
                .data$INCREASE == as.integer(as.logical(input$effectStimInput))
            ) %>%
            dplyr::pull(.data$PRED) %>%
            strsplit(split = "[|]") %>%
            unlist()
        )

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
    tmp <- sub(
      "@PRED",
      paste("$PRED", paste(tmp, collapse = "\n"), sep = "\n"),
      new
    )

  }

  tmp

}

#' Replace @DES tag
#'
#' @param input Internal parameter for \code{shiny}
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

        if ( grepl( 'mm', parm_lib[index, "ELIMINATION"]) ) {
          value <- ifelse(parm_lib[index, "ABSORPTION"] == "bolus_zero", 1, 2)
          if ( input$kmScaleInput ){
            tmpModel <- gsub(
              "<MM>",
              glue::glue("(A({value})/S{value})"),
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

        if ( input$poInput == "transit" ){

          doseAmount <- "DOSEAMT"
          doseCounter <- "DOSECNT"
          doseTime <- "DOSETIME"

          while( any(grepl(doseAmount, vars())) ){
            doseAmount <- glue::glue("Z{doseAmount}")
          }
          tmpModel <- gsub("<DOSEAMT>", doseAmount, tmpModel)

          while ( any(grepl(doseCounter, vars())) ){
            doseCounter <- glue::glue("Z{doseCounter}")
          }
          tmpModel <- gsub("<DOSECNT>", doseCounter, tmpModel)

          while ( any(grepl(doseTime, vars())) ){
            doseTime <- glue::glue("Z{doseTime}")
          }
          tmpModel <- gsub("<DOSETIME>", doseTime, tmpModel)

        }

        tmp <- c("  ; PK model equations", tmp, tmpModel, "  ")

      } else {

        # Get $DES code for non-PREDPP models
        tmpModel <- c()
        for (iCmt in 1:nPKcmts){
          tmpModel <- c(
            tmpModel,
             glue::glue(
               "  DADT({iCmt}) = <Define ODE for A({iCmt})>"
             )
          )
        }
        tmp <- c("  ; PK model equations", tmp, tmpModel, "  ")

      }
    }

    if ( input$pdInput %in% c("idr", "ode", "biophase") ){

      if ( input$pdInput == "idr" ){

        req( input$idrTypeInput, input$idrParmInput, input$idrStimInput )

        # Get $DES code for IDR and stimulation/inhibition models
        tmpModel <- parm_lib %>%
          dplyr::filter(
            .data$TYPE == "idr" &
              .data$FORM == input$idrTypeInput &
              .data$TRANS == input$idrParmInput
          ) %>%
          dplyr::pull(.data$DES) %>%
          strsplit(split = "[|]") %>%
          unlist()
        tmpStim <- parm_lib %>%
          dplyr::filter(
            .data$TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
              .data$FORM == input$idrStimInput
          ) %>%
          dplyr::pull(.data$DES) %>%
          strsplit(split = "[|]") %>%
          unlist()

        # Input compartment number into IDR model equation
        tmpModel <- gsub("<1>", nPKcmts + 1, tmpModel)

        # Input effect driver in stimulation/inhibition models
        if ( input$effectCpDriverInput ){
          if ( input$pkInput == "pk" ){
            value <- ifelse(
              input$poInput == "none" & input$ivInput %in% c("zero", "bolus"),
              1,
              2
            )
          } else if ( input$pkInput %in% c("linmat", "ode") ){
            value <- as.numeric(input$pkDefaultObsInput)
          }
          driver <- glue::glue(
            "A({input$effectCmtDriverInput})/{scale}",
            scale = ifelse(
              input$pkInput %in% c("pk", "linmat", "ode") &
                input$effectCmtDriverInput == (value + ifelse(input$pdInput == "biophase", 1, 0)),
              glue::glue("S{value}"),
              glue::glue("V{input$effectCmtDriverInput}")
            )
          )
        } else {
          driver <- glue::glue("A({input$effectCmtDriverInput})")
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
        for ( iCmt in (nPKcmts + 1:nPDcmts) ){
          tmpModel <- c(
            tmpModel,
            glue::glue("  DADT({iCmt}) = <Define ODE for A({iCmt})>")
          )
        }

        tmp <- c(tmp, "  ; PD model equations", tmpModel, "  ")

      } else if ( input$pdInput == "biophase" ){

        # Get $DES code for biophase models
        tmpModel <- parm_lib %>%
          dplyr::filter(.data$TYPE == "biophase") %>%
          dplyr::pull(.data$DES) %>%
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
#' @param input Internal parameter for \code{shiny}
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

  if ( isPRED() ){
    return( new[!grepl("@ERROR", new)] )
  }

  isPK <- input$pkInput != "none"
  isPD <- input$pdInput != "none"

  if ( isPK ){
    req( input$pkRVInput )
  }
  if ( isPD ){
    req( input$pdRVInput )
  }

  tmp <- c()
  dvidVariable <- ""
  blqVariable <- ""

  if ( isTruthy(input$mapTable) ){
    mapTable <- hot_to_r(input$mapTable)
    dvidVariable <- mapTable %>%
      dplyr::filter(.data$Description == "Endpoint identifier variable") %>%
      dplyr::pull(.data$Variable) %>%
      as.character()
    blqVariable <- mapTable %>%
      dplyr::filter(.data$Description == "BLQ variable") %>%
      dplyr::pull(.data$Variable) %>%
      as.character()
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
        glue::glue(
          "  IF ({dvid} == 1) THEN <Check that endpoint value is appropriate>",
          dvid = ifelse(
            dvidVariable != "",
            dvidVariable,
            "<endpoint variable>"
          )
        )
      )
    }

    # Extract RV model
    tmpRV <- rv_lib %>%
      dplyr::filter(.data$TYPE == input$pkRVInput)
    if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
      tmpRV <- tmpRV %>%
        dplyr::pull(.data$RV_NONMEM_M3)

      if ( blqVariable != ""){
        tmpRV <- sub(
          "<BLQ>",
          blqVariable,
          tmpRV
        )
      }

    } else {
      tmpRV <- tmpRV %>%
        dplyr::pull(.data$RV_NONMEM)
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
        glue::glue(
          "  IF ({dvid} == 2) THEN <Check that endpoint value is appropriate>",
          dvid = ifelse(
            dvidVariable != "",
            dvidVariable,
            "<endpoint variable>"
          )
        )
      )
    }

    # Extract RV model
    tmpRV <- rv_lib %>%
      dplyr::filter(.data$TYPE == input$pdRVInput)
    if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ) {
      tmpRV <- tmpRV %>%
        dplyr::pull(.data$RV_NONMEM_M3)

      if ( blqVariable != ""){
        tmpRV <- sub(
          "<BLQ>",
          blqVariable,
          tmpRV
        )
      }

    } else {
      tmpRV <- tmpRV %>%
        dplyr::pull(.data$RV_NONMEM)
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

    if (input$pdInput %in% c("direct", "biophase")){

      req( input$effectFormInput, input$effectParmInput, input$effectStimInput )

      # Extract the type of model
      tmpModel <- paste0(
        "  IPRED = ",
        parm_lib %>%
          dplyr::filter(
            .data$TYPE == "function" &
              .data$FORM == input$effectFormInput &
              .data$TRANS == input$effectParmInput &
              .data$INCREASE == as.integer(as.logical(input$effectStimInput))
          ) %>%
          dplyr::pull(.data$PRED)
      )
      tmpModel <- unlist(
        strsplit(
          tmpModel,
          split = "[|]"
        )
      )

      # Adjust the effect driver
      if ( input$pkInput == "pk" ){
        value <- ifelse(
          input$poInput == "none" & input$ivInput %in% c("zero", "bolus"),
          1,
          2
        )
      } else if ( input$pkInput %in% c("linmat", "ode" )){
        value <- as.numeric(input$pkDefaultObsInput)
      }
      tmpModel <- gsub(
        "<x>",
        ifelse(
          input$effectCpDriverInput,
          glue::glue(
            "(A({input$effectCmtDriverInput})/{scale})",
            scale = ifelse(
              input$pkInput %in% c("pk", "linmat", "ode") &
                input$effectCmtDriverInput == (value + ifelse(input$pdInput == "biophase", 1, 0)),
              glue::glue("S{value}"),
              glue::glue("V{input$effectCmtDriverInput}")
            )
          ),
          glue::glue("A({input$effectCmtDriverInput})")
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
      tmpRV <- gsub("<F>", glue::glue("A({nPKcmts + 1})"), tmpRV)
      tmpRV <- gsub("<DV>", "DV", tmpRV)
      tmpRV <- gsub("<1>", nPKeps + 1, tmpRV)
      tmpRV <- gsub("<2>", nPKeps + 2, tmpRV)

      # Create flag variable for CCV and log error models
      if ( input$pdRVInput %in% c("ccv", "log") ){
        tmpRV <- c(
          "  FLAG = 0",
          if ( isTruthy(input$flagF0Input) && as.logical(input$flagF0Input) ) {
            glue::glue("  IF (A({nPKcmts + 1}) == 0) FLAG = 1E-16")
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

  sub(
    "@ERROR",
    paste("$ERROR", paste(tmp, collapse = "\n"), sep = "\n"),
    new
  )

}

#' Replace @TASK tag
#'
#' @param input Internal parameter for \code{shiny}
#' @param new Text template
#' @param estimations Table of estimation tasks
#' @param isODE Reactive object - is model coded with ODEs?

replace_task <- function(
    input,
    new,
    estimations,
    isODE
) {

  if ( !isTruthy(input$estimationInput) ){
    return(new)
  }

  tmp <- c()

  # Get $ESTIMATION step
  if ( input$estimationInput ){

    if ( !isTruthy(input$estimationTable) ){
      return(new)
    }

    tmp <- sapply(
      1:5,
      function(x, estimations, input){
        if ( estimations$Method[x] %in% c("FO", "FOCE") ) {
          # Classic estimation methods
          glue::glue(
            "$ESTIMATION METHOD={fo}{foce}{lik}{nopred}{opts}PRINT=1 MAXEVAL=9999 NSIG={nsig} {sigl}SORT\n  MSFO={model}.msf",
            fo = ifelse(
              estimations$Method[x] == "FO",
              "0 POSTHOC ",
              "CONDITIONAL "
            ),
            foce = ifelse(
              estimations$Method[x] == "FOCE" & estimations$Interaction[x] != "" && estimations$Interaction[x] == "Yes",
              "INTERACTION ",
              ""
            ),
            lik = ifelse(
              estimations$Likelihood[x] != "" && estimations$Likelihood[x] == "Yes",
              "LIKELIHOOD ",
              ""
            ),
            nopred = ifelse(
              estimations$NoPrediction[x] != "" && estimations$NoPrediction[x] == "Yes",
              "NOPREDICTION ",
              ""
            ),
            opts = ifelse(
              estimations$Options[x]!="",
              paste0(estimations$Options[x], " "),
              ""
            ),
            nsig = ifelse(
              is.na(estimations$NSIG[x]),
              round(estimations$NSIG[x], 0),
              3
            ),
            sigl = ifelse(
              isODE(),
              ifelse(
                is.na(estimations$NSIG[x]),
                glue::glue("SIGL={3*round(estimations$NSIG[x], 0)} "),
                "SIGL=9 "
              ),
              ""
            ),
            model = ifelse(
              input$modelInput != "",
              sub(".ctl", "", input$modelInput),
              "<Enter the control stream name>"
            ),
            .trim = FALSE
          )
        } else if (estimations$Method[x] %in% c("ITS", "SAEM", "IMP", "BAYES")){
          # EM algorithms
          glue::glue(
            "$ESTIMATION METHOD={method} {lik}{nopred}{opts}PRINT=1 NSIG={nsig} {sigl}SORT\n  MSFO={model}.msf",
            method = estimations$Method[x],
            inter = ifelse(
              estimations$Interaction[x] != "" && estimations$Interaction[x] == "Yes",
              "INTERACTION ",
              ""
            ),
            lik = ifelse(
              estimations$Likelihood[x] != "" && estimations$Likelihood[x] == "Yes",
              "LIKELIHOOD ",
              ""
            ),
            nopred = ifelse(
              estimations$NoPrediction[x] != "" && estimations$NoPrediction[x] == "Yes",
              "NOPREDICTION ",
              ""
            ),
            opts = ifelse(
              estimations$Options[x]!="",
              paste0(estimations$Options[x], " "),
              ""
            ),
            nsig = ifelse(
              is.na(estimations$NSIG[x]),
              as.integer(estimations$NSIG[x]),
              3
            ),
            sigl = ifelse(
              isODE(),
              ifelse(
                is.na(estimations$NSIG[x]),
                glue::glue("SIGL={3*as.integer(estimations$NSIG[x])} "),
                "SIGL=9 "
              ),
              ""
            ),
            model = ifelse(
              input$modelInput != "",
              sub(".ctl", "", input$modelInput),
              "<Enter the control stream name>"
            ),
            .trim = FALSE
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
  if ( input$covarianceInput ){
    tmp <- c(
      tmp,
      "$COVARIANCE PRINT=E",
      ""
    )
  }

  # Get $SIMULATION
  if ( input$simulationInput ){
    if ( any(grepl("CALL RANDOM", new)) ){
      tmp <- c(
        tmp,
        glue::glue(
          "$SIMULATION ONLYSIMULATION NSUB={input$nsubInput}\n  ({seed1}) ({seed2} UNIFORM) NOPREDICTION",
          seed1 = input$simulationSeedInput,
          seed2 = round(100000 * signif(stats::runif(1), 5),0),
          .trim = FALSE
        ),
        ""
      )
    } else {
      tmp <- c(
        tmp,
        glue::glue(
          "$SIMULATION ONLYSIMULATION NSUB={input$nsubInput} ({input$simulationSeedInput})",
        ),
        ""
      )
    }

  }

  new <- sub("@TASK", paste(tmp, collapse = "\n"), new)

  new

}

#' Replace @TABLE tag
#'
#' @param input Internal parameter for \code{shiny}
#' @param new Text template
#' @param vars Reactive object - List of variables in data file

replace_table <- function(
    input,
    new,
    vars
) {

  tmp <- c()
  init <- c()

  if ( length(vars()) > 0 ) {
    local.vars <- vars()

    if ( "NUM" %in% local.vars )
      init <- c( init, "NUM" )
    if ( "ONUM" %in% local.vars )
      init <- c( init, "ONUM" )
    if ( "ROWID" %in% local.vars )
      init <- c( init, "ROWID" )

  } else {
    local.vars <- NULL
  }

  # Subject ID (REQUIRED)
  init <- c( init, "ID" )

  # Get NONMEM variables
  if ( isTruthy(input$mapTable) ){

    mappingTable <- hot_to_r( input$mapTable )

    # Add TIME
    if ( as.character(mappingTable %>% dplyr::slice( 2 ) %>% dplyr::pull(.data$Variable)) != "" ){
      tmp <- c(
        tmp,
        as.character(mappingTable %>% dplyr::slice( 2 ) %>% dplyr::pull(.data$NONMEM))
      )
    }
    # Add TAD
    tmp <- c(
      tmp,
      as.character(mappingTable %>% dplyr::slice( 6 ) %>% dplyr::pull(.data$Variable))
    )
    # Add DVID
    tmp <- c(
      tmp,
      as.character(mappingTable %>% dplyr::slice( 5 ) %>% dplyr::pull(.data$Variable))
    )
  } else {
    if ( input$pkInput %in% c("pk", "linmat", "ode") |
         input$pdInput %in% c("direct", "idr", "biophase", "ode") ){
      tmp <- c( tmp, "TIME" )
    }
  }

  if ( "EVID" %in% local.vars | length(local.vars) == 0 )
    tmp <- c( tmp, "EVID" )
  if ( "MDV" %in% local.vars  | length(local.vars) == 0 )
    tmp <- c( tmp, "MDV" )

  # Add variables for logistic regression and ordered categorical models
  if (input$pdInput == "logistic") {
    tmp <- c( tmp, "PROB", "EVENT" )
  }
  if (input$pdInput == "ordcat") {
    req(input$minCategoryInput, input$maxCategoryInput)
    tmp <- c(
      tmp,
      sprintf("LG%s", sort(input$minCategoryInput:input$maxCategoryInput)),
      ifelse(
        input$endpointInput == "",
        "EVENT",
        input$endpointInput
      )
    )
  }

  # Add variables for ER models
  if (input$pdInput == "er") {
    if (length(input$exposureVarInput) > 0 && input$exposureVarInput != "")
      tmp <- c( tmp, input$exposureVarInput )
    if (length(input$exposureVarTextInput) > 0 && input$exposureVarTextInput != "")
      tmp <- c( tmp, input$exposureVarTextInput )
  }

  # Get model parameters with IIV
  if ( isTruthy(input$varianceTable) ){

    # Get parameters
    varianceTable <- hot_to_r(input$varianceTable)
    tableParams <- varianceTable %>%
      dplyr::filter( .data$Variability != "None" ) %>%
      dplyr::pull( .data$Parameter )

    # Get ETA's
    if ( any(varianceTable$Variability != "None")) {
      tableETAs <- paste0("ETA", 1:sum(varianceTable$Variability != "None"))
    } else {
      tableETAs <- NULL
    }
  } else {
    tableParams <- NULL
    tableETAs <- NULL
  }

  # Get covariables
  tableCovariates <- c()
  if ( isTruthy(input$mapContVarInput) ){
    tableCovariates <- c( tableCovariates, input$mapContVarInput )
  }
  if ( isTruthy(input$mapCatVarInput) ){
    tableCovariates <- c( tableCovariates, input$mapCatVarInput )
  }

  # Get residuals
  hasRV <- ( input$pkInput != "none" & ( length(input$pkRVInput) > 0 && input$pkRVInput != "none ") ) |
    ( input$pdInput != "none" & ( length(input$pdRVInput) > 0 && input$pdRVInput != "none" ) )
  if ( !input$pdInput %in% c("logistic", "ordcat") ) {
    res <- "IPRED"
    if ( hasRV ) {
      res <- c( res, "IRES", "IWRES" )
    }
  } else {
    res <- c()
  }
  if ( hasRV ) {
    res <- c(res, "CWRES", "NPDE" )
  }

  # Concatenate all
  if ( length(tableParams) > 0 ){
    tmp <- c(init, tmp, paste0("TV", tableParams), tableParams, tableETAs, tableCovariates, res)
  } else {
    tmp <- c(init, tmp, tableETAs, tableCovariates, res)
  }

  # Remove duplicate variables, empty strings, and DV PRED RES WRES
  tmp <- unique(tmp)
  tmp <- tmp[ which( tmp != "" ) ]
  tmp <- tmp[ which(!tmp %in% c("DV", "PRED", "RES", "WRES")) ]


  if ( input$nmFlavorInput == "Standard style" ){

    # Replace @TABLE tag using 10 variables per line
    new <- sub(
      "@TABLE",
      glue::glue(
        "$TABLE {tenvar}\n  ONEHEADER NOPRINT FILE={model}.tbl\n",
        tenvar = paste(tenvars(tmp), collapse = "\n  "),
        model = ifelse(
          input$modelInput != "",
          sub(".ctl", "", input$modelInput),
          "<Enter the control stream name>"
        ),
        .trim = FALSE
      ),
      new
    )

  } else {

    runid <- sub("^.*?([0-9]+)$", "\\1", input$modelInput)

    # Main table
    psntab <- glue::glue(
      "$TABLE {vars}\n  ONEHEADER NOPRINT FILE=sdtab{runid}\n",
      vars = paste(tenvars(tmp), collapse = "\n  "),
      .trim = FALSE
    )

    # Parameter table
    if ( length(tableParams) > 0 ){
      tmp <- c( init, tableParams, tableETAs )

      # Remove duplicate variables, empty strings, and DV PRED RES WRES
      tmp <- unique(tmp)
      tmp <- tmp[ which( tmp != "" ) ]
      tmp <- tmp[ which(!tmp %in% c("DV", "PRED", "RES", "WRES")) ]

      psntab <- c(
        psntab,
        glue::glue(
          "$TABLE {vars}\n  ONEHEADER NOPRINT FILE=patab{runid}\n",
          vars = paste(tenvars(tmp), collapse = "\n  "),
          .trim = FALSE
        )
      )
    }

    # Random categorical covariate table
    if ( isTruthy(input$mapContVarInput) ){
      tmp <- c( init, input$mapContVarInput )

      # Remove duplicate variables, empty strings, and DV PRED RES WRES
      tmp <- unique(tmp)
      tmp <- tmp[ which( tmp != "" ) ]
      tmp <- tmp[ which(!tmp %in% c("DV", "PRED", "RES", "WRES")) ]

      psntab <- c(
        psntab,
        glue::glue(
          "$TABLE {vars}\n  ONEHEADER NOPRINT FILE=cotab{runid}\n",
          vars = paste(tenvars(tmp), collapse = "\n  "),
          .trim = FALSE
        )
      )
    }

    # Random categorical covariate table
    if ( isTruthy(input$mapCatVarInput) ){
      tmp <- c( init, input$mapCatVarInput )

      # Remove duplicate variables, empty strings, and DV PRED RES WRES
      tmp <- unique(tmp)
      tmp <- tmp[ which( tmp != "" ) ]
      tmp <- tmp[ which(!tmp %in% c("DV", "PRED", "RES", "WRES")) ]

      psntab <- c(
        psntab,
        glue::glue(
          "$TABLE {vars}\n  ONEHEADER NOPRINT FILE=catab{runid}\n",
          vars = paste(tenvars(tmp), collapse = "\n  "),
          .trim = FALSE
        )
      )
    }

    new <- sub(
      "@TABLE",
      paste(psntab, collapse = "\n"),
      new
    )
  }

  new

}

#' Replace $TAGs tag
#'
#' @param input Internal parameter for \code{shiny}
#' @param new Text template

replace_tags <- function(
    input,
    new
) {

  if ( input$nmFlavorInput != "Standard style" ){
    # No needs for tags for PsN style control streams
    return( sub("@TAGS", "", new) )
  }

  tmp <- c()

  # Get parameters with IIV
  if ( isTruthy(input$varianceTable) ){

    # Get parameters
    varianceTable <- hot_to_r(input$varianceTable)
    parameters <- varianceTable %>%
      dplyr::filter( .data$Variability != "None" ) %>%
      dplyr::pull( .data$Parameter )

    if ( length(parameters) > 0){
      tmp <- c(
        tmp,
        glue::glue(
          ";--parms- {vars}",
           vars = paste(parameters, collapse = " ")
        )
      )
    }

    # Get ETA's
    if ( any(varianceTable$Variability != "None")){
      etas <- paste0("ETA", 1:sum(varianceTable$Variability != "None"))
      tmp <- c(
        tmp,
        glue::glue(
          ";--randparms- {vars}",
          vars = paste(etas, collapse = " ")
        )
      )
    }
  }

  # Get covariables
  if ( isTruthy(input$mapContVarInput) ){
    tmp <- c(
      tmp,
      glue::glue(
        ";--contcovs- {covars}",
        covars = paste(input$mapContVarInput, collapse = " ")
      )
    )
  }
  if ( isTruthy(input$mapCatVarInput) ){
    tmp <- c(
      tmp,
      glue::glue(
        ";--catcovs- {cavars}",
        cavars = paste(input$mapCatVarInput, collapse = " ")
      )
    )
  }

  # Replace @TAGS tag
  new <- sub("@TAGS", paste(tmp, collapse = "\n"), new)

  new

}

#' Get NONMEM model parameter code lines as list
#'
#' @param input Internal parameter for \code{shiny}
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param mu A logical indicator for mu transformation

get_parms_code <- function(input, parms, varianceTable, mu){

  tvPK <- tvPD <- tvOT <- PK <- PD <- OT <- NULL
  multipleType <- FALSE
  neta <- 0

  for ( type in unique(parms$Type) ){
    typical <- glue::glue(
      "  {pre}; {type} parameters",
      pre = ifelse(multipleType, "\n  ", ""),
      .trim = FALSE
    )
    individual <- NULL

    # Code block for typical parameter values
    ieta <- neta
    if ( input$pdInput != "ordcat" ){
      individual <- ""
      # Include only parameters of the current type
      for ( iparm in which( parms$Type == type ) ){
        parm <- parms$Parameter[iparm]
        scale <- parms$Scale[iparm]
        variability <- varianceTable$Variability[iparm]
        parm_min <- parms$Min[iparm]
        parm_max <- parms$Max[iparm]
        typical <- c(
          typical,
          glue::glue(
            dplyr::case_when(
              scale == "Linear" & variability != "Logit" ~ "  TV{parm} = THETA({iparm})",
              scale == "Linear" & variability == "Logit" & parm_min == "0" & parm_max == "1" ~ paste(
                # Individual parameter within 0 and 1
                "  L{parm} = LOG(THETA({iparm}) / (1 - THETA({iparm}) ) )",
                "  TV{parm} = 1 / (1 + EXP(-L({parm})) )",
                sep = "\n"
              ),
              scale == "Linear" & variability == "Logit" & parm_min == "0" & parm_max != "1" ~ paste(
                # Individual parameter between 0 and a positive value different from 1
                "  L{parm} = LOG(THETA({iparm})/{parm_max} / (1 - THETA({iparm})/{parm_max} ) )",
                "  TV{parm} = {parm_max} / (1 + EXP(-L({parm})) )",
                sep = "\n"
              ),
              scale == "Linear" & variability == "Logit" & parm_min != "0" ~ paste(
                # Individual parameter boundaries different from 0 and 1
                "  L{parm} = LOG((TV{parm} - {parm_min})/({parm_max} - {parm_min})/(1 - (TV{parm} - {parm_min})/({parm_max} - {parm_min})))",
                "  TV{parm} = {parm_max} / (1 + EXP(-L({parm})) )",
                sep = "\n"
              ),
              scale == "Log" ~ "  TV{parm} = EXP(THETA({iparm}))",
              scale == "Logit" & parm_min == "0" & parm_max == "1" ~ paste(
                # Individual parameter within 0 and 1
                "  L{parm} = THETA({iparm})",
                "  TV{parm} = 1 / (1 + EXP(-L({parm})) )",
                sep = "\n"
              ),
              scale == "Logit" & parm_min == "0" & parm_max != "1" ~ paste(
                # Individual parameter between 0 and a positive value different from 1
                "  L{parm} = THETA({iparm})",
                "  TV{parm} = {parm_max} / (1 + EXP(-L({parm})) )",
                sep = "\n"
              ),
              TRUE ~ paste(
                # Individual parameter boundaries different from 0 and 1
                "  L{parm} = THETA({iparm})",
                "  TV{parm} = {parm_min} + ({parm_max} - {parm_min}) / (1 + EXP(-L({parm})) )",
                sep = "\n"
              )
            ),
            .trim = FALSE
          )
        )
        if ( mu ){
          # Increment ieta if necessary
          if ( varianceTable$Variability[iparm] != "None" ){
            ieta <- ieta + 1
            typical <- c(
              typical,
              glue::glue(
                dplyr::case_when(
                  # Linear scale
                  scale == "Linear" & variability == "Additive" ~ "  MU_{ieta} = TV{parms$Parameter[iparm]}",
                  scale == "Linear" & variability == "Exponential" ~ "  MU_{ieta} = LOG(TV{parms$Parameter[iparm]})",
                  scale == "Log" & variability == "Additive" ~ "  MU_{ieta} = TV{parms$Parameter[iparm]}",
                  scale == "Log" & variability == "Exponential" ~ "  MU_{ieta} = THETA({iparm})",
                  # Logit scale (additive and exponential variability are not possible with logit scale)
                  variability == "Logit" ~ "  MU_{ieta} = L{parm}"
                )
              )
            )
          }
        }
      }
    } else {

      req( input$minCategoryInput, input$maxCategoryInput )

      if ( input$minCategoryInput <= input$maxCategoryInput ){
        minCategory <- floor(input$minCategoryInput)
        maxCategory <- ceiling(input$maxCategoryInput) - 1
      } else {
        minCategory <- floor(input$maxCategoryInput)
        maxCategory <- ceiling(input$minCategoryInput) - 1
      }
      nCategories <- maxCategory - minCategory + 1
      # Include only parameters of the current type
      for ( iparm in which( parms$Type == type ) ){
        typical <- c(
          typical,
          glue::glue(
            "  TV{parm} = {parm2}THETA({iparm})",
            parm = parms$Parameter[iparm],
            parm2 = ifelse(
              iparm == 1 | iparm > nCategories,
              "",
              glue::glue("TV{parms$Parameter[iparm - 1]} + ")
            )
          )
        )
        if (iparm == nCategories){
          typical <- c(typical, "  ")
        }
      }
    }

    # Code block for individual parameter values
    ieta <- neta
    for ( iparm in which( parms$Type == type ) ){
      # Increment ieta if necessary
      if ( varianceTable$Variability[iparm] != "None" ){
        ieta <- ieta + 1
      }
      if ( input$pdInput != "ordcat" ){
        individual <- c(
          individual,
          get_individual_parm_code(parms, varianceTable, iparm, ieta, mu)
        )
      } else {
        if ( iparm > nCategories ){
          individual <- c(
            individual,
            get_individual_parm_code(parms, varianceTable, iparm, ieta, mu)
          )
        }
      }
    }

    if ( type == "PK" ){
      tvPK <- typical
      PK <- individual
    } else if ( type == "PD" ){
      tvPD <- typical
      PD <- individual
    } else {
      tvOT <- typical
      OT <- individual
    }

    multipleType <- TRUE
    neta <- ieta

  }

  split_line <- function(s){
    if ( length(s) > 0 & is.character(s)){
      unlist(strsplit(s, "\n"))
    } else {
      s
    }
  }

  list(
    tvPK = split_line(tvPK),
    PK = split_line(PK),
    tvPD = split_line(tvPD),
    PD = split_line(PD),
    tvOT = split_line(tvOT),
    OT = split_line(OT)
  )

}

#' Get line of code for individual parameter value
#'
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param iparm Index of parameter in parms data frame
#' @param ieta Index of ETA associated with parameter
#' @param mu A logical indicator for mu transformation


get_individual_parm_code <- function(parms, varianceTable, iparm, ieta, mu){

  parm <- parms$Parameter[iparm]
  scale <- parms$Scale[iparm]
  variability <- varianceTable$Variability[iparm]
  parm_min <- parms$Min[iparm]
  parm_max <- parms$Max[iparm]

  if ( mu ){
    glue::glue(
      dplyr::case_when(
        scale == "Linear" & variability == "None" ~ "  {parm} = TV{parm}",
        scale == "Linear" & variability == "Additive" ~ "  {parm} = MU_{ieta} + ETA({ieta})",
        scale == "Linear" & variability == "Exponential" ~ "  {parm} = EXP(MU_{ieta} + ETA({ieta}))",
        scale == "Log" & variability == "None" ~ "  {parm} = EXP(TV{parm})",
        scale == "Log" & variability == "Additive" ~ "  {parm} = MU_{ieta} + ETA({ieta})",
        scale == "Log" & variability == "Exponential" ~ "  {parm} = EXP(MU_{ieta} + ETA({ieta}))",
        # Logit variability not allowed with log scale
        # no variability not allowed with logit scale
        # Additive variability not allowed with logit scale
        # Exponential variability not allowed with logit scale
        variability == "Logit" & parm_min == "0" & parm_max == "1" ~
          # Individual parameter within 0 and 1
          "  {parm} = 1 / ( 1 + EXP( - (MU_{ieta} + ETA({ieta}) ) ) )",
        variability == "Logit" & parm_min == "0" & parm_max != "1" ~
          # Individual parameter between 0 and a positive value different from 1
          "  {parm} = {parm_max} / ( 1 + EXP( - (MU_{ieta} + ETA({ieta}) ) ) )",
        variability == "Logit" & parm_min != "0" ~
          # Individual parameter boundaries different from 0 and 1
          "  {parm} = {parm_min} + ({parm_max} - {parm_min}) / ( 1 + EXP( - (MU_{ieta} + ETA({ieta}) ) ) )"
      )
    )
  } else {
    glue::glue(
      dplyr::case_when(
        variability == "None" ~ glue::glue("  {parm} = TV{parm}"),
        variability == "Additive" ~ glue::glue("  {parm} = TV{parm} + ETA({ieta})"),
        variability == "Exponential" ~ glue::glue("  {parm} = TV{parm}*EXP(ETA({ieta}))"),
        variability == "Logit" & parm_min == "0" & parm_max == "1" ~
          # Individual parameter within 0 and 1
          "  {parm} = 1 / ( 1 + EXP( - (L{parm} + ETA({ieta}) ) ) )",
        variability == "Logit" & parm_min == "0" & parm_max != "1" ~
          # Individual parameter between 0 and a positive value different from 1
          "  {parm} = {parm_max} / ( 1 + EXP( - (L{parm} + ETA({ieta}) ) ) )",
        variability == "Logit" & parm_min != "0" ~
          # Individual parameter boundaries different from 0 and 1
          "  {parm} = {parm_min} + ({parm_max} - {parm_min}) / ( 1 + EXP( - (L{parm} + ETA({ieta}) ) ) )"
      )
    )
  }
}


