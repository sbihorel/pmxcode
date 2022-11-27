
#' Replacement of @PURPOSE tag
#'
#' @param input Internal parameter for {shiny}.
#' @param new Text template

replace_purpose <- function(
    input,
    new,
    variance
) {

  pkPurpose <- ""
  if (input$pkInput != "none"){
    if (input$pkInput == "pk"){
      pkPurpose <- paste0(
        pkPurpose,
        input$pkCMTInput, "-compartment model",
        " with ",
        switch(
          input$absorptionInput,
          "bolus" = "bolus input",
          "zero" = "zero-order input",
          "first" = "first-order input",
          "sig" = "sigmoid input",
          "transit" = "input through transit compartments"
        ),
        ifelse(
          input$absorptionInput %in% c("zero","sig") | as.logical(input$alagInput),
          " ",
          ""
        ),
        ifelse(
          input$absorptionInput %in% c("zero","sig"),
          switch(
            input$zeroEstInput,
            "0" = "(fixed rate",
            "-1" = "(estimated rate",
            "-2" = "(estimated duration"),
          ""
        ),
        ifelse(
          !input$absorptionInput %in% c("zero","sig") & as.logical(input$alagInput),
          "(",
          ""
        ),
        ifelse(
          as.logical(input$alagInput),
          ifelse(
            input$absorptionInput %in% c("zero","sig"),
            " with lag-time",
            "with lag-time"
          ),
          ""
        ),
        ifelse(
          input$absorptionInput %in% c("zero","sig") | as.logical(input$alagInput),
          ")",
          ""
        ),
        " and ",
        switch(
          input$eliminationInput,
          "lin" = "linear elimination",
          "mm" = "saturable elimination",
          "mmlin" = "linear and saturable elimination",
          "tmdd" = "target-mediated disposition",
          "tmddqe" = "target-mediated disposition at quasi-equilibrium",
          "tmddqer" = "target-mediated disposition at quasi-equilibrium (constant Rtot)",
          "tmddqss" = "target-mediated disposition at quasi steady-state",
          "tmddqssr" = "target-mediated disposition at quasi steady-state (constant Rtot)"
        )
      )
    } else if (input$pkInput == "linmat"){
      pkPurpose <- paste0(
        pkPurpose,
        "Custom model defined by first-order transfer rates"
      )
    } else if (input$pkInput == "ode"){
      pkPurpose <- paste0(
        pkPurpose,
        "Custom model defined by custom ODEs"
      )
    } else if (input$pkInput == "pred"){
      pkPurpose <- paste0(
        pkPurpose,
        "Custom model defined by explicit solutions"
      )
    }
  }

  pdPurpose <- ""
  if (input$pdInput != "none"){
    if (input$pdInput == "er"){
      pdPurpose <- "Exposure-response model"
    } else if (input$pdInput == "pred"){
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
          "logis" = "Logistic regression model",
          "ordcat" = "Logistic regression model for ordered categorical data"
        ),
        if (input$pdInput %in% c("direct", "link") && isTruthy(input$effectFormInput)){
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
        if (input$pdInput %in% c("er") && isTruthy(input$effectFormInput) ){
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
        if (input$pdInput =="idr" && areTruthy(input$idrTypeInput, input$idrStimInput) ){
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
      ";; 9. Estimation:"
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
    if ( isTruthy(variance) ){
      hasIIV <- variance[which(variance$Variability != "none"), 1]

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
      if ( length(hasRV) == 2){
        hasRV <- paste(
          paste0(";;   PK component: ", hasRV[1]),
          "\n",
          paste0(";;   PD component: ", hasRV[2])
        )
      }
      replacement <- sub("@RV", hasRV, replacement)
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

replace_path <- function(input, new){

  if ( areTruthy(input$modelDirChoose, "path" %in% names(input$modelDirChoose)) ){
    path <- normalizePath(
      shinyFiles::parseDirPath(c(root = "/"), input$modelDirChoose)
    )
  } else {
    path <- NULL
  }

  if (input$modelInput != ""){
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

#' Auto indent code tags
#'
#' @param code Code lines containing comments to align across
#'

align_tags <- function(code){

  # Find semicolon positions in each string on code
  position <- sapply(
    strsplit(code, split = ""),
    function(x) match(";", x)
  )

  # Insert spaces to align comments after lines of code that actually contain semi-colons
  for ( irow in seq_along(code) ){
    if ( !is.na(position[irow]) ){
      code[irow] <- sub(
        "^(.+?);(.*)$",
        sprintf(
          "\\1%s;\\2",
          paste(
            rep(" ", max(position, na.rm = TRUE) - position[irow]),
            collapse = ""
          )
        ),
        code[irow]
      )
    }
  }

  code

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

  preamble <- c()

  if ( input$pkInput == "pk" & input$absorptionInput == "transit" ){

    if ( input$platformInput == "NONMEM" ) {
      preamble <- c(
        preamble,
        "  CALLFL = -2",
        "  ",
        "  ; Capture dose record information",
        "  IF (NEWIND < 2) <ndose> = 0",
        "  IF (AMT > 0 .AND. CMT == 1) THEN",
        "    <ndose> = <ndose> + 1",
        "    <dosetime>(<ndose>) = TIME",
        "    <dose>(<ndose>) = AMT",
        "  ENDIF",
        "  "
      )

      # Ensure that variables are not used in $INPUT
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

      preamble <- gsub("<dose>", doseVariable, preamble)
      preamble <- gsub("<ndose>", ndoseVariable, preamble)
      preamble <- gsub("<dosetime>", dosetimeVariable, preamble)
    }

    if (input$platformInput == "mrgsolve") {
      preamble <- c(
        preamble,
        "  // Capture dose record information",
        "  if (NEWIND <= 1) {",
        "    int ndose = -1;",
        "  }",
        "  if (EVID == 1){",
        "    ndose = ndose + 1;",
        "    dosetime[ndose] = TIME;",
        "    dose[ndose] = self.amt;",
        "  }",
        "  "
      )
    }

  }

  # Check if CALLFL=-2 is required
  if ( input$platformInput == "NONMEM" ) {
    if ( any(grepl("ALAG", parms$Parameter)) & !any(grepl("CALLFL", preamble)) ) {
      preamble <- c("  CALLFL = -2", "  ", preamble)
    }
    if ( length(vars()) > 0 &&
         ("ADDL" %in% vars() & !any(grepl("CALLFL", preamble)))
    ) {
      preamble <- c("  CALLFL = -2", "  ", preamble)
    }
  }

  preamble

}

#' Get model parameter code lines as list
#'
#' @param input Internal parameter for {shiny}
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param mu A logical indicator for mu transformation

get_parms_code <- function(input, parms, varianceTable, mu){

  tvPK <- tvPD <- tvOT <- PK <- PD <- OT <- NULL
  multipleType <- FALSE
  neta <- 0
  for ( type in unique(parms$Type) ){
    typical <- sprintf(
      "  %s; %s parameters",
      ifelse(multipleType, "\n  ", ""),
      type
    )
    individual <- NULL

    # Code block for typical parameter values
    ieta <- neta
    if ( input$pdInput != "ordcat" ){
      individual <- ""
      for ( iparm in 1:nrow(parms) ){
        # Include only parameters of the current type
        if (parms$Type[iparm] != type){
          next
        }
        typical <- c(
          typical,
          sprintf("  TV%s = THETA(%d)", parms$Parameter[iparm], iparm)
        )
        if ( mu ){
          # Increment ieta if necessary
          if ( varianceTable$Variability[iparm] != "None" ){
            ieta <- ieta + 1
            if ( varianceTable$Variability[iparm] != "Exponential" ){
              typical <- c(
                typical,
                sprintf("  MU_%s = TV%s", ieta, parms$Parameter[iparm])
              )
            } else {
              typical <- c(
                typical,
                sprintf("  MU_%s = LOG(TV%s)", ieta, parms$Parameter[iparm])
              )
            }
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
      for ( iparm in 1:nrow(parms) ){
        # Include only parameters of the current type
        if ( parms$Type[iparm] != type ){
          next
        }
        typical <- c(
          typical,
          sprintf(
            "  TV%s = %sTHETA(%d)",
            parms$Parameter[iparm],
            ifelse(
              iparm == 1 | iparm > nCategories,
              "",
              sprintf("TV%s + ", parms$Parameter[iparm - 1])
            ),
            iparm
          )
        )
        if (iparm == nCategories){
          typical <- c(typical, "  ")
        }
      }
    }

    # Code block for individual parameter values
    ieta <- neta
    for ( iparm in 1:nrow(parms) ){
      # Include only parameters of the current type
      if ( parms$Type[iparm] != type ){
        next
      }
      # Increment ieta if necessary
      if ( varianceTable$Variability[iparm] != "None" ){
        ieta <- ieta + 1
      }
      if ( input$pdInput != "ordcat" ){
        individual <- c(
          individual,
          get_individual_parm_line(parms, varianceTable, iparm, ieta, mu)
        )
      } else {
        if ( iparm > nCategories ){
          individual <- c(
            individual,
            get_individual_parm_line(parms, varianceTable, iparm, ieta, mu)
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

  list(tvPK = tvPK, PK = PK, tvPD = tvPD, PD = PD, tvOT = tvOT, OT = OT)

}

#' Get line of code for individual parameter value
#'
#' @param parms Parameter selection
#' @param varianceTable Variability selection
#' @param iparm Index of parameter in parms data frame
#' @param ieta Index of ETA associated with parameter
#' @param my A logical indicator for mu transformation


get_individual_parm_line <- function(parms, varianceTable, iparm, ieta, mu){

  if ( mu ){
    switch(
      levels(varianceTable$Variability)[varianceTable$Variability[iparm]],
      "None" = sprintf(
        "  %s = TV%s",
        parms$Parameter[iparm],
        parms$Parameter[iparm]),
      "Additive" = sprintf(
        "  %s = MU_%s + ETA(%d)",
        parms$Parameter[iparm],
        ieta,
        ieta),
      "Exponential" = sprintf(
        "  %s = EXP(MU_%s+ETA(%d))",
        parms$Parameter[iparm],
        ieta,
        ieta),
      "Logit" =
        if ( parms$Low[iparm] == 0 & parms$High[iparm] == 1 ){
          # Individual parameter within 0 and 1
          sprintf(
            paste(
              "  L%s = LOG(TV%s/(1 - TV%s))",
              "  MU_%s = L%s + ETA(%d)",
              "  %s = EXP(MU_%s)/(1 + EXP(MU_%s))",
              sep = "\n"
            ),
            # First line
            parms$Parameter[iparm],
            parms$Parameter[iparm],
            parms$Parameter[iparm],
            # Second line
            ieta,
            parms$Parameter[iparm],
            ieta,
            # Third line
            parms$Parameter[iparm],
            ieta,
            ieta
          )
        } else if ( parms$Low[iparm] == 0 & parms$High[iparm] != 1 ){
          # Individual parameter between 0 and a positive value different from 1
          sprintf(
            paste(
              "  L%s = LOG(TV%s/%s/(1 - TV%s/%s))",
              "  MU_%s = L%s + ETA(%d)",
              "  %s = %s*EXP(MU_%s)/(1 + EXP(MU_%s))",
              sep = "\n"
            ),
            # First line
            parms$Parameter[iparm],
            parms$Parameter[iparm],
            parms$High[iparm],
            parms$Parameter[iparm],
            parms$High[iparm],
            # Second line
            ieta,
            parms$Parameter[iparm],
            ieta,
            # Third line
            parms$Parameter[iparm],
            parms$High[iparm],
            ieta,
            ieta
          )
        } else {
          # Individual parameter boundaries different from 0 and 1
          sprintf(
            paste(
              "  L%s = LOG((TV%s - %s)/(%s - %s)/(1 - (TV%s - %s)/(%s - %s)))",
              "  MU_%s = L%s + ETA(%d)",
              "  %s = %s + (%s - %s)*EXP(MU_%s)/(1 + EXP(MU_%s))",
              sep = "\n"
            ),
            # First line
            parms$Parameter[iparm],
            parms$Parameter[iparm],
            parms$Low[iparm],
            parms$High[iparm],
            parms$Low[iparm],
            parms$Parameter[iparm],
            parms$Low[iparm],
            parms$High[iparm],
            parms$Low[iparm],
            # Second line
            ieta,
            parms$Parameter[iparm],
            ieta,
            # Third line
            parms$Parameter[iparm],
            parms$Low[iparm],
            parms$High[iparm],
            parms$Low[iparm],
            ieta,
            ieta
          )
        }
    )
  } else {
    switch(
      levels(varianceTable$Variability)[varianceTable$Variability[iparm]],
      "None" = sprintf
      ("  %s = TV%s",
        parms$Parameter[iparm],
        parms$Parameter[iparm]),
      "Additive" = sprintf(
        "  %s = TV%s + ETA(%d)",
        parms$Parameter[iparm],
        parms$Parameter[iparm],
        ieta),
      "Exponential" = sprintf(
        "  %s = TV%s*EXP(ETA(%d))",
        parms$Parameter[iparm],
        parms$Parameter[iparm],
        ieta),
      "Logit" =
        # Numerically stable of logit transform
        if ( parms$Low[iparm] == 0 & parms$High[iparm] == 1 ){
          # Individual parameter within 0 and 1
          sprintf(
            "  %s = 1/((1/TV%s - 1)*EXP(-ETA(%d)) + 1)",
            parms$Parameter[iparm],
            parms$Parameter[iparm],
            ieta
          )
        } else if ( parms$Low[iparm] == 0 & parms$High[iparm] != 1 ){
          # Individual parameter between 0 and a positive value different from 1
          sprintf(
            "  %s = %s/((1/TV%s - 1)*EXP(-ETA(%d)) + 1)",
            parms$Parameter[iparm],
            parms$High[iparm],
            parms$Parameter[iparm],
            ieta
          )
        } else {
          # Individual parameter boundaries different from 0 and 1
          sprintf(
            "  %s = %s + (%s - %s)/((1/TV%s - 1)*EXP(-ETA(%d)) + 1)",
            parms$Parameter[iparm],
            parms$Low[iparm],
            parms$High[iparm],
            parms$Low[iparm],
            parms$Parameter[iparm],
            ieta
          )
        }
    )
  }
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
  if ( isLINMAT() & input$absorptionInput %in% c("first", "sig")){
    tmp <- c(
      tmp,
      sprintf("  K%s%s = KA", input$pkDefaultDoseInput, input$pkDefaultObsInput)
    )
  }

  # Cases when LINMAT PK model is associated with link PD model
  if ( isLINMAT() & !isODE() & input$pdInput == "link" ){
    tmp <- c(
      tmp,
      sprintf("  K%s%s = KE0", input$pkDefaultObsInput, input$pknCMTInput+1),
      sprintf("  K%s0 = KE0", input$pknCMTInput+1)
    )
  }

  # Add rate variable if they are fixed in dataset
  if ( input$pkInput != "none" & input$absorptionInput %in% c("zero", "sig") ) {
    if ( input$zeroEstInput == 0 ) {
      tmp <- c(tmp, "  R1 = RATE")
    }
    if ( input$platformInput == "Berkeley Madonna" ) {
      if ( input$zeroEstInput == -1 ) {
        tmp <- c(
          tmp,
          sprintf(
            "DTIME = IF (MD = 0) THEN %s ELSE INT(TIME/II)*II%s",
            ifelse(
              as.logical(input$alagInput),
              "ALAG1",
              0
            ),
            ifelse(
              as.logical(input$alagInput),
              " + ALAG1",
              ""
            )
          ),
          "RATE = R1",
          "DUR = DOSE/RATE"
        )
      } else if ( input$zeroEstInput == -2 ) {
        tmp <- c(
          tmp,
          sprintf(
            "DTIME = IF (MD = 0) THEN %s ELSE INT(TIME/II)*II%s",
            ifelse(
              as.logical(input$alagInput),
              "ALAG1",
              0
            ),
            ifelse(
              as.logical(input$alagInput),
              " + ALAG1",
              ""
            )
          ),
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
      unlist(
        strsplit(
          parm_lib[indexModel, "DERIVED"],
          split = "[|]"
        )
      )
    )
  }

  # Adapt NONMEM code to platform-specific syntax
  if ( input$platformInput == "mrgsolve" ) {
    tmp <- gsub("^  ", "  double ", tmp)
    tmp <- gsub("$", ";", tmp)
    tmp <- gsub(" ([A-Za-z0-9]+)[*][*]([A-Za-z0-9]+)([ /*+-])", " pow(\\1, \\2)\\3", tmp)
    tmp <- gsub("GAMLN", "lgamma", tmp)
    tmp <- gsub("EXP", "exp", tmp)
    tmp <- gsub("LOG", "log", tmp)
    tmp <- gsub("SQRT", "sqrt", tmp)
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
    if ( areTruthy(
      input$doseUnitInput, input$volumeUnitInput, input$cpUnitInput, input$mmInput
    )
    ){
      doseUnit <- input$doseUnitInput
      concentrationUnit <- unlist(
        strsplit(
          input$cpUnitInput, split = "[/]"
        )
      )[1]

      if (grepl("g", doseUnit) != grepl("g", concentrationUnit)){
        scaleVar <- paste0(scaleVar, "*", input$mmInput)
      }

      # Adjust scale based upon differences in units
      scale <- scaling %>%
        dplyr::filter(
          DOSE == input$doseUnitInput &
            VOLUME == input$volumeUnitInput &
            CONCENTRATION == input$cpUnitInput
        ) %>%
        dplyr::pull(SCALING)
      if (scale != 1){
        scaleVar <- paste0(scaleVar, "/", scale)
      }
    }

    tmp <- c(
      tmp,
      "  \n  ; Scale and bioavailability",
      sprintf("  S%s = %s", indexS, scaleVar),
      sprintf("  F%s = %s", indexF, ifelse(input$absorptionInput == "transit", 0, 1))
    )

  }

  tmp

}

#' Get the number of PK and PD compartments
#'
#' @param input Internal parameter for {shiny}
#' @param new Text template
#' @param isPRED Reactive object - is model coded with $PRED?
#' @param isPREDD Reactive object - is mode coded with $PK?
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
          CMT == input$pkCMTInput &
            ABSORPTION == input$absorptionInput &
            ELIMINATION == ifelse(
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
      if (input$pkInput != "none") {
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

  if ( input$platformInput == "mrgsolve" ) {
    initBlockHeader <- "  \n  // Initialization block (adjust if necessary)"
  } else {
    initBlockHeader <- "  \n  ; Initialization block (adjust if necessary)"
  }

  indent <- ifelse(input$platformInput == "NONMEM", "    ", "  ")

  if ( input$pkInput == "pk" & input$eliminationInput %in% c("tmdd", "tmddqe", "tmddqss") ){
    tmp <- c(
      tmp,
      initBlockHeader,
      if ( input$platformInput == "NONMEM" ) "  IF (A_0FLG == 1) THEN",
      paste0(
        indent,
        parm_lib[
          get_model_lib_index(
            input = input, advan = advan, trans = trans, parm_lib = parm_lib
          ),
          "INITIALIZATION"
        ]
      )
    )
  }

  if ( input$pkInput == "ode" ){
    tmp <- c(
      tmp,
      initBlockHeader,
      if ( input$platformInput == "NONMEM" ) "  IF (A_0FLG == 1) THEN",
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
      if (length(tmp) == 0) {
        c(
          initBlockHeader,
          if (input$platformInput == "NONMEM") "  IF (A_0FLG == 1) THEN"
        )
      },
      sub(
        "<1>",
        nPKcmts + 1,
        paste0(
          ifelse(input$platformInput == "NONMEM", "    ", "  "),
          parm_lib %>%
            dplyr::filter(TYPE == "idr") %>%
            dplyr::slice(1) %>%
            dplyr::pull(INITIALIZATION)
        )
      )
    )

  }

  if ( input$pdInput == "ode" ){

    req(nPKcmts, nPDcmts)

    tmp <- c(
      tmp,
      if (length(tmp) == 0) {
        c(
          initBlockHeader,
          if (input$platformInput == "NONMEM") "  IF (A_0FLG == 1) THEN"
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
