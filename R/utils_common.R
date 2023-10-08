
#' Replacement of @PURPOSE tag
#'
#' @param input Internal parameter for {shiny}.
#' @param new Text template
#' @param varianceTable Variance- table

replace_purpose <- function(
    input,
    new,
    varianceTable
) {

  pkPurpose <- ""
  if ( input$pkInput != "none" ){
    if ( input$pkInput == "pk" ){

      if ( input$ivInput == "zero" ){
        req( input$ivRateInput )
      }

      pkPurpose <- paste0(
        pkPurpose,
        input$pkCMTInput, "-compartment model",
        " with ",
        dplyr::case_when(
          input$ivInput == "none" ~ "",
          input$ivInput == "bolus" ~ "bolus",
          input$ivInput == "zero" && input$ivRateInput == "0" ~ "fixed-rate infusion",
          input$ivInput == "zero" && input$ivRateInput == "-1" ~ "estimated rate infusion",
          input$ivInput == "zero" && input$ivRateInput == "-2" ~ "estimated duration infusion",
          TRUE ~ ""
        ),
        dplyr::case_when(
          input$ivInput == "bolus" & grepl("first|sig", input$poInput) ~ " and ",
          input$ivInput == "bolus" & !grepl("first|sig", input$poInput) ~ " input ",
          input$ivInput == "zero" & input$poInput != "none" ~ " and ",
          TRUE ~ ""
        ),
        dplyr::case_when(
          input$poInput == "none" ~ "",
          input$poInput == "first" & input$ivInput != "bolus" ~ "first-order input",
          input$poInput == "first" & input$ivInput == "bolus" ~ "first-order inputs",
          input$poInput == "sig" & input$ivInput != "bolus"  ~ "sigmoid input",
          input$poInput == "sig" & input$ivInput == "bolus"  ~ "sigmoid inputs",
          input$poInput == "transit" ~ "input through transit compartments",
          TRUE ~ ""
        ),
        ifelse(
          isTruthy(as.logical(input$alagInput1)) | isTruthy(as.logical(input$alagInput2)),
          " with lag-time",
          ""
        ),
        " and ",
        dplyr::case_when(
          input$eliminationInput == "lin" ~ "linear elimination",
          input$eliminationInput == "mm" ~ "saturable elimination",
          input$eliminationInput == "mmlin" ~ "linear and saturable elimination",
          input$eliminationInput == "tmdd" ~ "target-mediated disposition",
          input$eliminationInput == "tmddqe" ~ "target-mediated disposition at quasi-equilibrium",
          input$eliminationInput == "tmddqer" ~ "target-mediated disposition at quasi-equilibrium (constant Rtot)",
          input$eliminationInput == "tmddqss" ~ "target-mediated disposition at quasi steady-state",
          input$eliminationInput == "tmddqssr" ~ "target-mediated disposition at quasi steady-state (constant Rtot)"
        )
      )
    } else if ( input$pkInput == "linmat" ){
      pkPurpose <- paste0(
        pkPurpose,
        "Custom model defined by first-order transfer rates"
      )
    } else if ( input$pkInput == "ode" ){
      pkPurpose <- paste0(
        pkPurpose,
        "Custom model defined by custom ODEs"
      )
    } else if ( input$pkInput == "pred" ){
      pkPurpose <- paste0(
        pkPurpose,
        "Custom model defined by explicit solutions"
      )
    }
  }

  pdPurpose <- ""
  if ( input$pdInput != "none" ){
    if ( input$pdInput == "er" ){
      pdPurpose <- "Exposure-response model"
    } else if ( input$pdInput == "pred" ){
      pdPurpose <- "PD model defined by custom explicit solutions"
    } else {
      pdPurpose <- paste0(
        pdPurpose,
        switch(
          input$pdInput,
          "direct" = "Direct effect model",
          "link" = "Biophase model",
          "idr" = "Indirect response model",
          "ode" = "Model defined by custom ODEs",
          "logistic" = "Logistic regression model",
          "ordcat" = "Logistic regression model for ordered categorical data"
        ),
        if ( input$pdInput %in% c("direct", "link") && isTruthy(input$effectFormInput) ){
          paste0(
            " - ",
            switch(
              input$effectFormInput,
              "lin" = "Linear drug effect",
              "pow" = "Power drug effect",
              "exp" = "Exponential drug effect",
              "mm" = "Saturable drug effect (Michael-Menten function)",
              "hill" = "Saturable drug effect (Hill function)",
            )
          )
        },
        if ( input$pdInput %in% c("er") && isTruthy(input$effectFormInput) ){
          paste0(
            " - ",
            switch(
              input$effectFormInput,
              "lin" = "Linear relationship",
              "pow" = "Power relationship",
              "exp" = "Exponential relationship",
              "mm" = "Michael-Menten relationship",
              "hill" = "Hill relationship"
            )
          )
        },
        if ( input$pdInput =="idr" && areTruthy(input$idrTypeInput, input$idrStimInput) ){
          paste0(
            " - ",
            switch(
              input$idrTypeInput,
              "idr1" = "Inhibition of production",
              "idr2" = "Inhibition of elimination",
              "idr3" = "Stimulation of production",
              "idr4" = "Stimulation of elimination"
            ),
            " - ",
            switch(
              input$idrStimInput,
              "lin" = "Linear drug effect",
              "pow" = "Power drug effect",
              "exp" = "Exponential drug effect",
              "mm" = "Saturable drug effect (Michael-Menten function)",
              "hill" = "Saturable drug effect (Hill function)"
            )
          )
        }
      )
    }
  }

  commentChar <- ifelse(input$platformInput == "mrgsolve", "//", ";;" )

  replacement <- paste0(
    ifelse(
      pkPurpose != "",
      sprintf(
        "%s%s",
        ifelse(
          pkPurpose != "" & pdPurpose != "",
          sprintf("\n%s    PK component: ", commentChar),
          ""
        ),
        pkPurpose),
      ""
    ),
    ifelse(
      pdPurpose != "",
      sprintf(
        "%s%s",
        ifelse(
          pkPurpose != "" & pdPurpose != "",
          sprintf("\n%s    PD component: ", commentChar),
          ""
        ),
        pdPurpose
      ),
      ""
    )
  )

  if ( input$platformInput == "NONMEM" & input$nmFlavorInput == "PsN/Xpose style" ){
    purpose <- c(
      "1. Based on:",
      ";; 2. Description:",
      ";; 3. Label:",
      ";; 4. Structural model:",
      ";; @STRUCTURE",
      ";; 5. Covariate model:",
      ";; 6. Interindividual variability",
      ";; @IIV",
      ";; 7. Interoccasion variability:",
      ";; 8. Residual variability:",
      "@RV",
      ";; 9. Estimation:",
      ";; @ESTIM"
    )

    # Structural model
    replacement <- sub(
      "@STRUCTURE",
      paste0(
        "   ",
        sub("^\n;;\\s+", "", replacement)
      ),
      purpose
    )

    # IIV
    if ( isTruthy(varianceTable) ){
      hasIIV <- varianceTable %>%
        dplyr::filter(.data$Variability != "none") %>%
        dplyr::pull(.data$Parameter)

      if ( length(hasIIV) ==0 ){
        replacement <- sub("@IIV", "   no IIV", replacement)
      } else {
        replacement <- sub(
          "@IIV",
          paste0("   ", paste(hasIIV, collapse = ", ")),
          replacement
        )
      }
    }

    # RV
    if ( isTruthy(input$pkRVInput) | isTruthy(input$pdRVInput) ){
      hasRV <- c()
      if ( isTruthy(input$pkRVInput) && input$pkRVInput != "none" ){
        hasRV <- c(
          hasRV,
          switch(
            input$pkRVInput,
            "add" = "Additive",
            "ccv" = "Constant CV",
            "accv" = "Additive + Constant CV",
            "log" = "Logarithmic"
          )
        )
      }
      if ( isTruthy(input$pdRVInput) && input$pdRVInput != "none" ){
        hasRV <- c(
          hasRV,
          switch(
            input$pdRVInput,
            "add" = "Additive",
            "ccv" = "Constant CV",
            "accv" = "Additive + Constant CV",
            "log" = "Logarithmic"
          )
        )
      }
      if ( length(hasRV) == 0 ){
        hasRV <- ""
      }
      if ( length(hasRV) == 2 ){
        hasRV <- paste0(
          paste0("PK component: ", hasRV[1]),
          "\n",
          paste0(";;   PD component: ", hasRV[2])
        )
      }
      replacement <- sub("@RV", glue::glue(";;    {hasRV}", .trim = FALSE), replacement)
    }

    # Estimation
    if ( areTruthy(input$estimationInput, input$estimationTable) ){
      tmp <- hot_to_r(input$estimationTable) %>%
        dplyr::filter(.data$Method != "none") %>%
        dplyr::mutate(
          INFO = glue::glue(
            "{Method}{sup}",
            sup = ifelse(.data$Interaction == "Yes", "+Interaction", "")
          )
        ) %>%
        dplyr::pull(.data$INFO)
      tmp <- paste(tmp, collapse = ", ")
      replacement <- sub("@ESTIM", glue::glue("   {tmp}"), replacement)
    } else {
      replacement <- sub("@ESTIM", "", replacement)
    }

    replacement <- paste(replacement, collapse = "\n")

  } else {
    replacement <- paste0(
      "PURPOSE: ",
      replacement,
      "\n",
      commentChar,
      " ------------------------------------------------------------------------------"
    )
  }

  new <- sub("@PURPOSE", replacement, new )

  return(new)

}

#' Replacement of @PATH tag
#'
#' @param input Internal parameter for {shiny}.
#' @param new Text template

replace_path <- function(input, new ){

  if ( areTruthy(input$modelDirChoose, "path" %in% names(input$modelDirChoose)) ){
    path <- normalizePath(
      shinyFiles::parseDirPath(c(root = "/"), input$modelDirChoose)
    )
  } else {
    path <- NULL
  }

  if ( input$modelInput != "" ){
    model <- paste0(
      gsub("[.]ctl|[.]mod|[.]cpp|[.]mdd", "", input$modelInput),
      switch(
        input$platformInput,
        "NONMEM" = ifelse(
          input$nmFlavorInput == "Standard style",
          ".ctl",
          ".mod"
        ),
        "mrgsolve" = ".cpp",
        "Berkeley Madonna" = ".mdd"
      )
    )
    new <- sub(
      "@PATH",
      ifelse(
        is.null(path),
        model,
        gsub( "//", "/", file.path(path, model) )
      ),
      new
    )
  } else {
    new <- sub(
      "@PATH",
      ifelse(
        is.null(path),
        ifelse(
          input$platformInput == "NONMEM",
          "<Enter the control stream name>",
          "<Enter the model file name>"
        ),
        gsub(
          "//",
          "/",
          file.path(
            path,
            ifelse(
              input$platformInput == "NONMEM",
              "<Enter the control stream name>",
              "<Enter the model file name>"
            )
          )
        )
      ),
      new
    )
  }

  return(new)

}

#' Auto indent NONMEM code tags
#'
#' @param code Code lines containing comments to align across
#'

align_tags <- function(code ){

  # Find semicolon positions in each string on code
  position <- sapply(
    strsplit(code, split = ""),
    function(x) match(";", x)
  )

  # Insert spaces to align comments after lines of code that actually contain semi-colons
  for ( irow in seq_along(code) ){
    if ( !is.na(position[irow]) ){
      nspaces <- paste(
        rep(" ", max(position, na.rm = TRUE) - position[irow]),
        collapse = ""
      )
      code[irow] <- sub(
        "^(.+?);(.*)$",
        glue::glue("\\1{nspaces};\\2"),
        code[irow]
      )
    }
  }

  code

}

#' Auto indent mrgsolve code
#'
#' @param code Code lines containing comments to align across
#'

align_annotations <- function(code){

  # Replace first : with @@ and align first :
  tmp <- sub(":", "@@", code)

  colonPosition <- sapply(
    strsplit(tmp, split = ""),
    function(x) match("@@", x)
  )
  whichRowHasColon <- !is.na(colonPosition)

  for ( irow in seq_along(whichRowHasColon) ){
    if ( !is.na(colonPosition[irow]) ){
      tmp[irow] <- paste0(
        substr( tmp[irow], 1, colonPosition[irow] - 1 ),
        paste(
          rep(" ", max( colonPosition, na.rm = TRUE ) - colonPosition[irow]),
          collapse = ""
        ),
        substr( tmp[irow], colonPosition[irow], nchar(tmp[irow]))
      )
    }
  }

  # Align second :
  colonPosition <- sapply(
    strsplit(tmp, split = ""),
    function(x) match(":", x)
  )
  whichRowHasColon <- !is.na(colonPosition)

  for ( irow in seq_along(whichRowHasColon) ){
    if ( !is.na(colonPosition[irow]) ){
      tmp[irow] <- paste0(
        substr( tmp[irow], 1, colonPosition[irow] - 1 ),
        paste(
          rep(" ", max(colonPosition, na.rm = TRUE) - colonPosition[irow]),
          collapse = ""
        ),
        substr(tmp[irow], colonPosition[irow], nchar(tmp[irow]))
      )
    }
  }

  # Replace @@ by :
  tmp <- sub("@@", ":", tmp)


  # Return formatted code
  tmp

}


#' Get lines of preamble code for transit compartment absorption model and delayed
#' dosing records
#'
#' @param input Internal parameter for {shiny}
#' @param parms Parameter selection
#' @param vars Character vector of variable names

get_preamble_code <- function(
    input,
    parms,
    vars
) {

  tmp <- c()

  if ( input$pkInput == "pk" & input$poInput == "transit" ){

    if ( input$platformInput == "NONMEM" ) {

      # Ensure that variables are not used in $INPUT
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

      tmp <- c(
        tmp,
        glue::glue(
          paste(
            "  CALLFL = -2",
            "  ",
            "  ; Capture dose record information",
            "  IF ( NEWIND < 2) {doseCounter} = 0",
            "  IF ( AMT > 0 .AND. CMT == 1) THEN",
            "    {doseCounter} = {doseCounter} + 1",
            "    {doseTime}({doseCounter}) = TIME",
            "    {doseAmount}({doseCounter}) = AMT",
            "  ENDIF",
            "  ",
            sep = "\n"
          ),
          .trim = FALSE
        )
      )

    }

    if ( input$platformInput == "mrgsolve") {
      tmp <- c(
        tmp,
        "  // Capture dose record information",
        "  if ( NEWIND <= 1) {",
        "    int DOSECNT = -1;",
        "  }",
        "  if ( EVID == 1 ){",
        "    DOSECNT = DOSECNT + 1;",
        "    DOSETIME[NDOSE] = TIME;",
        "    DOSEAMT[NDOSE] = self.amt;",
        "  }",
        "  "
      )
    }

  }

  # Check if CALLFL=-2 is required
  if ( input$platformInput == "NONMEM" ) {
    if ( any(grepl("ALAG", parms$Parameter)) & !any(grepl("CALLFL", tmp)) ) {
      tmp <- c("  CALLFL = -2", "  ", tmp)
    }
    if ( length(vars()) > 0 &&
         ("ADDL" %in% vars() & !any(grepl("CALLFL", tmp)))
    ) {
      tmp <- c("  CALLFL = -2", "  ", tmp)
    }
  }

  tmp

}

#' Get code lines for scaling and bioavailability
#'
#' @param input Internal parameter for {shiny}
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param parm_lib Library of parameters
#' @param scaling  Library for scaling


get_scaling_code <- function(
    input,
    advan,
    trans,
    parm_lib,
    scaling
 ){

  tmp <- c()

  if ( !input$pkInput %in% c("none", "pred") ){

    if ( input$pkInput == "pk" ){

      req(advan(), trans())

      index <- get_model_lib_index(
        input = input, advan = advan, trans = trans, parm_lib = parm_lib
      )
      indexS <- ifelse(
        parm_lib[index, "ABSORPTION"] == "bolus_zero",
        1,
        2
      )
      if ( input$pkInput %in% c("none", "pred") ){
        scaleVar <- "V"
      } else {
        if ( input$pkInput == "pk" ){
          scaleVar <- parm_lib[index, "VCENTRAL"]
        } else {
          scaleVar <- sprintf("V%s", as.numeric(input$pkDefaultObsInput))
        }
      }
      indexF <- 1
    } else {
      indexS <- as.numeric(input$pkDefaultObsInput)
      scaleVar <- sprintf("V%s", indexS)
      indexF <- as.numeric(input$pkDefaultDoseInput)
    }

    # Adjust scale based upon molecular mass
    if (
      areTruthy( input$doseUnitInput, input$volumeUnitInput, input$cpUnitInput )
    ){
      doseUnit <- input$doseUnitInput
      concentrationUnit <- unlist(
        strsplit(
          input$cpUnitInput, split = "[/]"
        )
      )[1]


      if ( isTruthy(input$mmInput) & grepl("g", doseUnit) != grepl("g", concentrationUnit) ){
        scaleVar <- paste0(scaleVar, "*", input$mmInput)
      }

      # Adjust scale based upon differences in units
      scale <- scaling %>%
        dplyr::filter(
          .data$DOSE == input$doseUnitInput &
            .data$VOLUME == input$volumeUnitInput &
            .data$CONCENTRATION == input$cpUnitInput
        ) %>%
        dplyr::pull(.data$SCALING)
      if ( scale != 1 ){
        scaleVar <- paste0(scaleVar, "/", scale)
      }
    }

    if ( input$platformInput == "NONMEM" ){
      tmp <- c(
        tmp,
        "  \n  ; Scale and bioavailability",
        sprintf("  S%s = %s", indexS, scaleVar),
        sprintf("  F%s = %s", indexF, ifelse(input$poInput == "transit", 0, 1))
      )
    } else if ( input$platformInput == "mrgsolve" ){
      tmp <- c(
        tmp,
        "  \n  // Scale and Bioavailability",
        sprintf("  double SCALE = %s;", scaleVar),
        sprintf("  F_A%s = %s;", indexF, ifelse(input$poInput == "transit", 0, 1))
      )
    } else {
      tmp <- c(
        tmp,
        "\n  ; Scale and bioavailability",
        sprintf("  S%s = %s", indexS, scaleVar),
        sprintf("  F%s = %s", indexF, ifelse(input$poInput == "transit", 0, 1))
      )
    }

  }

  tmp

}

#' Get lines of code for derived parameters
#'
#' @param input Internal parameter for {shiny}
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isODE Reactive object - is model coded with ODEs?
#' @param isLINMAT Reactive object - is model coded as linear matrix?
#' @param parms Parameter selection
#' @param parm_lib Library of parameters
#'

get_derived_parms_code <- function(
    input,
    advan,
    trans,
    isPRED,
    isODE,
    isLINMAT,
    parms,
    parm_lib
) {

  tmp <- c()

  # Derived variables for PREDPP models
  if ( input$pkInput == "pk" ){

    req( advan(), trans() )

    index <- get_model_lib_index(
      input = input, advan = advan, trans = trans, parm_lib = parm_lib
    )
    tmp <- c(
      tmp,
      unlist(
        strsplit(
          parm_lib[index, "DERIVED_PREDPP"],
          split = "[|]"
        )
      )
    )
  }

  # Cases when typical PREDPP model needs to be converted to ODEs
  if ( input$pkInput == "pk" & isODE() ){
    index <- get_model_lib_index(
      input = input, advan = advan, trans = trans, parm_lib = parm_lib
    )
    tmp <- c(
      tmp,
      unlist(
        strsplit(
          parm_lib[index, "DERIVED"],
          split = "[|]"
        )
      )
    )
  }

  # Cases when PK model is defined as linear matrix (LINMAT)
  if ( isLINMAT() & input$poInput %in% c("first", "sig") ){
    tmp <- c(
      tmp,
      glue::glue("  K{input$pkDefaultDoseInput}{input$pkDefaultObsInput} = KA")
    )
  }

  # Cases when LINMAT PK model is associated with link PD model
  if ( isLINMAT() & !isODE() & input$pdInput == "link" ){
    tmp <- c(
      tmp,
      glue::glue("  K{input$pkDefaultObsInput}{input$pknCMTInput+1} = KE0"),
      glue::glue("  K{input$pknCMTInput+1}0 = KE0")
    )
  }

  # Add rate variable if they are fixed in dataset
  if ( input$pkInput != "none" & (input$ivInput == "zero" | input$poInput == "sig") ) {

    if ( input$ivInput == "zero" ){
      req( input$ivRateInput )
    }

    if ( input$poInput == "sig" ){
      req( input$poRateInput )
    }

    dual <- ifelse(input$ivInput != "none" & input$poInput != "none", 1, 0)
    if ( input$poInput == "sig" && input$poRateInput == 0 ) {
      tmp <- c(tmp, "  R1 = RATE")
    }
    if ( input$ivInput == "zero" && input$ivRateInput == 0 ){

      if ( input$pkInput == "pk" ){
        tmp <- c(tmp, glue::glue("  R{dual+1} = RATE"))
      } else {
        tmp <- c(tmp, glue::glue("  R{input$pkDefaultObsInput + dual} = RATE"))
      }
    }
    if ( input$platformInput == "Berkeley Madonna" ) {
      tmp <- c(
        tmp,
        glue::glue(
          "DTIME = if ( MD = 0) THEN {time1} ELSE INT(TIME/II)*II{time2}",
          time1 = ifelse(
            isTruthy(as.logical(input$alagInput1)) | isTruthy(as.logical(input$alagInput2)),
            "ALAG1",
            0
          ),
          time2 = ifelse(
            isTruthy(as.logical(input$alagInput1)) | isTruthy(as.logical(input$alagInput2)),
            " + ALAG1",
            ""
          )
        )
      )
      stop('Intentional failure')
      if ( input$zeroEstimationInput == -1 ) {
        tmp <- c(
          tmp,
          "RATE = R1",
          "DUR = DOSE/RATE"
        )
      } else if ( input$zeroEstimationInput == -2 ) {
        tmp <- c(
          tmp,
          "RATE = DOSE/D1",
          "DUR = D1"
        )
      }
    }
  }

  # IDR models
  if ( input$pdInput == "idr" ){
    indexModel <- which(
      parm_lib$TYPE == "idr" &
        parm_lib$FORM == input$idrTypeInput &
        parm_lib$TRANS == input$idrParmInput
    )

    tmp <- c(
      tmp,
      parm_lib %>%
        dplyr::slice(n = indexModel) %>%
        dplyr::pull(.data$DERIVED) %>%
        strsplit(split = "[|]" ) %>%
        unlist()
    )
  }

  # Adapt NONMEM code to platform-specific syntax
  if ( input$platformInput == "mrgsolve" ) {
    tmp <- gsub("^  ", "  double ", tmp)
    tmp <- gsub("$", ";", tmp)
    tmp <- gsub(" ([A-Za-z0-9]+)[*][*]([A-Za-z0-9]+)([ /*+-])", " pow(\\1, \\2)\\3", tmp)
    tmp <- gsub("EXP", "exp", tmp)
    tmp <- gsub("LOG", "log", tmp)
    tmp <- gsub("SQRT", "sqrt", tmp)
    tmp <- gsub("GAMLN", "lgamma", tmp)
  }
  if ( input$platformInput == "Berkeley Madonna" ) {
    tmp <- gsub("^ \\s+", "", tmp)
    tmp <- gsub("^\n  ", "\n", tmp)
    tmp <- gsub("[*][*]", "^", tmp)
    tmp <- gsub("GAMLN", "GAMMALN", tmp)
  }

  if ( length(tmp) > 0 ){
    tmp <- c(
      sprintf(
        ifelse(
          input$platformInput == "Berkeley Madonna",
          "\n%s Derived parameters",
          "  \n  %s Derived parameters"
        ),
        ifelse(input$platformInput == "mrgsolve", "//", ";")
      ),
      tmp
    )
  }

  tmp

}

#' Get the number of PK and PD compartments
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isPREDPP Reactive object - is mode coded with $PK?
#' @param model_lib Library for $MODEL replacement

get_ncmts <- function(
    input,
    new,
    model_lib,
    isPRED,
    isPREDPP
 ){

  if ( isPRED() | isPREDPP() ){
    nPKcmts <- 0
    nPDcmts <- 0
  } else {
    # PK compartments
    if ( input$pkInput == "pk" ) {
      tmp <- model_lib %>%
        dplyr::filter(
          .data$CMT == input$pkCMTInput &
            .data$ABSORPTION == sub(
              "zero_|first_|none_",
              "",
              paste(input$ivInput, input$poInput, sep = "_")
            ) &
            .data$ ELIMINATION == ifelse(
              grepl("tmdd", input$eliminationInput),
              input$eliminationInput,
              "mmlin")
        )
      tmp <- unlist(
        strsplit(
          ifelse(input$platformInput == "NONMEM", tmp$NONMEM, tmp$MRGSOLVE),
          split = "[|]"
        )
      )
      nPKcmts <- length(tmp)
    } else {
      if ( input$pkInput != "none") {
        nPKcmts <- input$pknCMTInput
      } else {
        nPKcmts <- 0
      }
    }

    # PD compartments
    if ( input$pdInput %in% c("link", "idr") ){
      nPDcmts <- 1
    } else if ( input$pdInput == "ode" ){
      nPDcmts <- input$pdnCMTInput
    } else {
      nPDcmts <- 0
    }
  }

  c(nPKcmts, nPDcmts)

}

#' Get compartment intialization block
#'
#' @param input Internal parameter for {shiny}
#' @param advan Reactive object - NONMEM ADVAN value
#' @param trans Reactive object - NONMEM TRANS value
#' @param nPKcmts,nPDcmts Number of compartments for PK and PD model components
#' @param parm_lib Library of parameters

get_init_code <- function(
    input,
    advan,
    trans,
    nPKcmts,
    nPDcmts,
    parm_lib
 ){

  tmp <- c()

  initBlockHeader <- ifelse(
    input$platformInput == "mrgsolve",
    "  \n  // Initialization block (adjust if necessary)",
    "  \n  ; Initialization block (adjust if necessary)"
  )

  indent <- ifelse(input$platformInput == "NONMEM", "    ", "  ")

  if ( input$pkInput == "pk" & input$eliminationInput %in% c("tmdd", "tmddqe", "tmddqss") ){
    tmp <- c(
      tmp,
      initBlockHeader,
      if ( input$platformInput == "NONMEM" ) "  if ( A_0FLG == 1) THEN",
      paste0(
        indent,
        parm_lib %>%
          dplyr::slice(
            n = get_model_lib_index(
              input = input, advan = advan, trans = trans, parm_lib = parm_lib
            )
          ) %>%
          dplyr::pull(.data$INITIALIZATION)
      )
    )
  }

  if ( input$pkInput == "ode" ){
    tmp <- c(
      tmp,
      initBlockHeader,
      if ( input$platformInput == "NONMEM" ) "  if ( A_0FLG == 1) THEN",
      sprintf(
        "%sA_0(%s) = 0",
        indent,
        1:nPKcmts
      )
    )
  }

  if ( input$pdInput == "idr" ){

    tmp <- c(
      tmp,
      if ( length(tmp) == 0) {
        c(
          initBlockHeader,
          if ( input$platformInput == "NONMEM") "  if ( A_0FLG == 1) THEN"
        )
      },
      sub(
        "<1>",
        nPKcmts + 1,
        paste0(
          ifelse(input$platformInput == "NONMEM", "    ", "  "),
          parm_lib %>%
            dplyr::filter(.data$TYPE == "idr") %>%
            dplyr::slice(n = 1) %>%
            dplyr::pull(.data$INITIALIZATION)
        )
      )
    )

  }

  if ( input$pdInput == "ode" ){

    req(nPKcmts, nPDcmts)

    tmp <- c(
      tmp,
      if ( length(tmp) == 0) {
        c(
          initBlockHeader,
          if ( input$platformInput == "NONMEM") "  if ( A_0FLG == 1) THEN"
        )
      },
      sprintf(
        "%sA_0(%s) = 0",
        indent,
        nPKcmts + 1:nPDcmts
      )
    )

  }

  # Adjust code to match platform coding syntax
  if ( input$platformInput == "mrgsolve" ) {
    tmp <- sub("_0[(]([0-9]+)[)] =", "\\1_0 =", tmp)
    tmp <- sub("$", ";", tmp)
    tmp <- sub("block;$", "block", tmp)
  }

  if ( length(tmp) > 0 & input$platformInput == "NONMEM" ){
    tmp <- c(
      tmp,
      "  ENDIF"
    )
  }

  tmp

}

#' Format variables by lines of 10.
#'
#' @param x  Character vectors of variables

tenvars <- function( x ){
  varsBy10 <- NULL
  while ( length(x) > 0 ) {
    varsBy10 <- c(
      varsBy10,
      paste(
        x[1:min(10, length(x) )],
        collapse = " "
      )
    )
    x <- x[-( 1:min(10, length(x) ) )]
  }
  varsBy10
}

#' Determines if a file path exists
#'
#' @param file a path to a file


file_exists <- function( file ){

  tryCatch(
    expr = { file.exists( file ) },
    error = function(e ){ FALSE },
    warning = function(e ){ FALSE },
    finally = {}
  )

}
