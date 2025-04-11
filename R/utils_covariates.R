
#' Get NONMEM code blocks
#'
#' Process a character string containing NONMEM model code and returns a
#' character vector in which each element contain a particular $ block
#'
#' @param code A character string containing the control stream
#' @noRd

get_nonmem_blocks <- function( code ){

  # Remove $ if it is 1st character (no avoid creating empty element in strsplit)
  new_code <- sub("^[$]", "", code)

  # dos2unix
  new_code <- gsub("\r\n", "\n", new_code)

  # Collapse code by replacing new lines with @@@ tags
  new_code <- gsub("\n", " @@@", new_code)

  # Split new_code in text blocks starting with $
  tmp <- unlist(strsplit(new_code, split = " @@@[$]"))

  # Add $ back at the beginning of each new_code element
  if ( grepl("^[$]", code) ) {
    new_code <- paste0("$", tmp)
  } else {
    new_code <- paste0(
      c( "", rep("$", length(tmp) -1) ),
      tmp
    )
  }

  new_code

}

#' Get number of THETA parameters
#'
#' @param code A character string containing the control stream
#' @noRd

get_theta_number <- function( code ){

  if (code == ""){
    # Returns 0 if the code is empty
    return(0)
  } else {

    new_code <- get_nonmem_blocks( code )

    # Find and extract all $THETA blocks
    theta_index <- which( grepl("^[$]THETA", new_code) )

    if ( length(theta_index) == 0 ){
      thetas <- NULL
    } else {
      thetas <- unlist(
        strsplit(
          paste( new_code[theta_index], collapse = " @@@"),
          " @@@"
        )
      )

      # Process $THETA blocks which are now into a vector of character
      # replace $THETA with space
      #thetas <- thetas[ !grepl("^[$]THETA", thetas) ]
      thetas <- gsub( "[$]TH[E]*[T]*[A]*", " ", thetas )
      # convert any tab character into spaces
      thetas <- gsub( "\t", " ", thetas )
      # remove comments
      thetas <- gsub( ";.*$", "", thetas )
      # remove FIX(ED)
      thetas <- gsub( "FIX[E]*[D]*", "", thetas )
      # remove leading and trailing spaces
      thetas <- gsub( "^\\s*|\\s*$", "", thetas )
      # collapse onto 1 vector
      thetas <- paste( thetas, collapse = " " )
      # replace any text construct with boundaries (low, initial, high) by 1
      thetas <- gsub( "?[(].*?[)]", "1", thetas )
      # replace multiple spaces by single spaces
      thetas <- gsub( "\\s+", " ", thetas )
      # remove leading and trailing spaces
      thetas <- gsub( "^\\s*|\\s*$", "", thetas )
      # split the vector using a space delimiter- there should be one element per theta
      thetas <- unlist(strsplit(thetas, " "))

    }

    length(thetas)

  }
}

#' Convert reference code for univariate model creation
#'
#' @param code A character string containing NONMEM code
#' @param parms A data.frame of parameter information extracted from the
#' description expected from the control stream header
#' @noRd


convert_reference_code <- function(code, parms ){

  if (code == ""){
    return("")
  } else {

    new_code <- get_nonmem_blocks( code )

    # Determine if there are any MU equation expected
    mu <- any( grepl("MU_", parms$MU) )

    # Find and process $PRED or $PK
    predpk_index <- which( grepl("^[$]PRED|^[$]PK", new_code) )
    if ( length(predpk_index) == 0 ){
      return(
        new_code_object <- list(
          code = "Invalid reference model",
          status = "danger",
          text = list(
            "No $PK or $PRED block present in NONMEM control stream"
          )
        )
      )
    }
    if ( length(predpk_index) > 1 ){
      return(
        new_code_object <- list(
          code = "Invalid reference model",
          status = "danger",
          text = list(
            "More than 1 $PK or $PRED block present in NONMEM control stream"
          )
        )
      )
    }
    predpk_code <- unlist(
      strsplit(new_code[predpk_index], split = " @@@")
    )
    if ( grepl(" @@@$", new_code[predpk_index]) ){
      predpk_code <- c(predpk_code, "")
    }

    # Find all TV* and L* variables
    parms <- parms %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        tvx_index = which(
          grepl( paste0("^\\s*TV", Parameter, "\\s*="), predpk_code )
        )[1],
        tvx_times = sum(
          grepl( paste0("^\\s*TV", Parameter, "\\s*="), predpk_code )
        ),
        lx_index = which(
          grepl( paste0("^\\s*L", Parameter, "\\s*="), predpk_code )
        )[1]
      )

    if ( any( is.na(parms$tvx_index) ) ){
      return(
        new_code_object <- list(
          code = "Invalid reference model",
          status = "danger",
          text = glue::glue(
            "TVx equation not found for the following parameters: {hits}",
            hits = parms %>%
              dplyr::filter( is.na(tvx_index) ) %>%
                dplyr::pull( Parameter ) %>%
                paste( collapse = ", " )
          )
        )
      )
    }

    # Find all L* variable for
    logit_parms <- parms %>% dplyr::filter( Scale == "logit" )
    if ( nrow(logit_parms) > 0 & any( is.na(logit_parms$lx_index) ) ) {
      return(
        new_code_object <- list(
          code = "Invalid reference model",
          status = "danger",
          text = glue::glue(
            "Lx equation not found for the following parameters: {hits}",
            hits = parms %>%
              dplyr::filter( is.na(lx_index) ) %>%
              dplyr::pull( Parameter ) %>%
              paste( collapse = ", " )
          )
        )
      )
    }

    # Check mu's
    if ( mu ){
      parms <- parms %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          MU_times = sum( grepl( glue::glue( "^\\s*{MU}\\s*=" ), predpk_code ) ),
          MU_index = which( grepl( glue::glue( "^\\s*{MU}\\s*=" ), predpk_code ) )[1],
          MU_match = sum(
            grepl(
              glue::glue( "^\\s*{MU}\\s*=.*TV{Parameter}|^\\s*{MU}\\s*=.*L{Parameter}" ),
              predpk_code
            )
          )
        )

      if ( any( !is.na(parms$MU) & parms$MU_times == 0 ) ){
        return(
          new_code_object <- list(
            code = "Invalid reference model",
            status = "danger",
            text = glue::glue(
              "MU equation not found for the following parametes: {hits}",
              hits = parms %>%
                dplyr::filter( !is.na(MU) & MU_times == 0 ) %>%
                dplyr::pull( Parameter ) %>%
                paste( collapse = ", " )
            )
          )
        )
      }

      if ( any( !is.na(parms$MU) & parms$MU_times > 1 ) ){
        return(
          new_code_object <- list(
            code = "Invalid reference model",
            status = "danger",
            text = glue::glue(
              "Multiple MU equations found for the following parametes: {hits}",
              hits = parms %>%
                dplyr::filter( !is.na(MU) & MU_times > 1 ) %>%
                dplyr::pull( Parameter ) %>%
                paste( collapse = ", " )
            )
          )
        )
      }


      if ( any( !is.na(parms$MU) & parms$MU_match == 0 ) ){
        return(
          new_code_object <- list(
            code = "Invalid reference model",
            status = "danger",
            text = glue::glue(
              "Mismatch between the list of parameters in the structural model description and MU equations: {hits}",
              hits = parms %>%
                dplyr::filter( !is.na(MU) & MU_match == 0 ) %>%
                dplyr::pull( Parameter ) %>%
                paste( collapse = ", " )
            )
          )
        )
      }

    }

    # Make code adjustement for covariate effect insertion
    complex_equation <- FALSE
    for ( iparm in 1:nrow(parms) ){
      parm <- parms$Parameter[iparm]
      scale <- parms$Scale[iparm]
      variability <- parms$Variability[iparm]
      tvx_index <- parms$tvx_index[iparm]
      lx_index <- parms$lx_index[iparm]

      if ( !mu ){
        # Model not coded with MU referencing

        if ( scale == "linear" & variability != "logit" ){

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("TV{parm}\\s*=\\s*THETA[(][0-9]+[)]\\s*$"),
              predpk_code[tvx_index]
            )
          }

          # Replace TVx equation by Lx equation
          predpk_code[tvx_index] <- sub(
            glue::glue("^(\\s*)TV{parm}(.*)$"),
            glue::glue("\\1L{parm}\\2\n\\1TV{parm} = L{parm}"),
            predpk_code[tvx_index]
          )

        } else if ( scale == "linear" & variability == "logit" ){

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("TV{parm}\\s*=.*EXP[(]-L{parm}[)]\\s*[)]\\s*$"),
              predpk_code[tvx_index]
            )
          }

        } else if ( scale == "log" ){

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("TV{parm}\\s*=\\s*EXP[(]\\s*THETA[(][0-9]+[)]\\s*[)]\\s*$"),
              predpk_code[tvx_index]
            )
          }

          # Replace TVx equation by Lx equation
          predpk_code[tvx_index] <- sub(
            glue::glue("^(\\s*)TV{parm}\\s*=\\s*EXP[(]\\s*(THETA[(][0-9]+[)].*)[)](.*)$"),
            glue::glue("\\1L{parm} = \\2\n\\1TV{parm} = EXP(L{parm})\\3"),
            predpk_code[tvx_index]
          )

        } else if ( scale == "logit" ){

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("L{parm}\\s*=\\s*THETA[(][0-9]+[)]\\s*$"),
              predpk_code[lx_index]
            )
          }

        }

      } else {
        # Model coded with MU referencing
        MU_index <- parms$MU_index[iparm]

        if ( scale == "linear" & variability != "logit" ){

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("TV{parm}\\s*=\\s*THETA[(][0-9]+[)]\\s*$"),
              predpk_code[tvx_index]
            )
          }

          # Replace TVx equation by Lx equation
          predpk_code[tvx_index] <- sub(
            glue::glue("^(\\s*)TV{parm}(.*)$"),
            glue::glue("\\1L{parm}\\2\n\\1TV{parm} = L{parm}"),
            predpk_code[tvx_index]
          )

          # Replace MU equation
          predpk_code[ MU_index ] <- sub(
            glue::glue("TV{parm}"),
            glue::glue("L{parm}"),
            predpk_code[ MU_index ]
          )

        } else if ( scale == "linear" & variability == "logit" ){

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("TV{parm}\\s*=.*EXP[(]-L{parm}[)]\\s*[)]\\s*$"),
              predpk_code[tvx_index]
            )
          }

          # Replace MU equation
          predpk_code[ MU_index ] <- sub(
            glue::glue("TV{parm}"),
            glue::glue("L{parm}"),
            predpk_code[ MU_index ]
          )

        } else if ( scale == "log" ) {

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("TV{parm}\\s*=\\s*EXP[(]\\s*THETA[(][0-9]+[)]\\s*[)]\\s*$"),
              predpk_code[tvx_index]
            )
          }

          # Replace TVx equation by Lx equation
          predpk_code[tvx_index] <- sub(
            glue::glue("^(\\s*)TV{parm}\\s*=\\s*EXP[(]\\s*(THETA[(][0-9]+[)].*)[)](.*)$"),
            glue::glue("\\1L{parm} = \\2\n\\1TV{parm} = EXP(L{parm})\\3"),
            predpk_code[tvx_index]
          )

          # Replace MU equation
          if ( variability == "exp" ){
            predpk_code[ MU_index ] <- sub(
              glue::glue("LOG[(]\\s*TV{parm}\\s*[)]"),
              glue::glue("L{parm}"),
              predpk_code[ MU_index ]
            )
          }

        } else if ( scale == "logit" ) {

          # Check for simple TVx equation
          if ( !complex_equation ) {
            complex_equation <- !grepl(
              glue::glue("L{parm}\\s*=\\s*THETA[(][0-9]+[)]\\s*$"),
              predpk_code[lx_index]
            )
          }
        }
      }
    }

    # Add covariate start and end tags
    if ( parms$tvx_index[1] == 2 ){
      predpk_code[1] <- paste0(
        predpk_code[1],
        " @@@;-- COVARIATE EFFECT START @@@",
        " @@@;-- COVARIATE EFFECT END @@@"
      )
    } else {
      continue <- TRUE
      index <- parms$tvx_index[1]
      while (continue){
        if ( grepl("^\\s*;", predpk_code[index - 1]) ){
          # Skip commented code line
          index <- index - 1
        } else {
          # No comment at the start of the previous line
          predpk_code[index - 1] <- paste0(
            predpk_code[index - 1],
            " @@@;-- COVARIATE EFFECT START @@@",
            " @@@;-- COVARIATE EFFECT END @@@"
          )
          continue <- FALSE
        }
      }
    }

    # Collapse $PRED or $PK
    predpk_code <- paste(predpk_code, collapse = " @@@")

    # Replace $PRED or $PK
    new_code[predpk_index] <- predpk_code

    # Final collapsing and replacement of @@@ tags by \n
    new_code <- paste(new_code, collapse = " @@@")
    new_code <- gsub(" @@@", "\n", new_code)

    # Create output object
    if ( any( parms$tvx_times > 1 ) ) {
      complex_equation <- TRUE
    }

    if ( complex_equation ) {
      new_code_object <- list(
        code = new_code,
        status = "info",
        text = "Complex TV parameter definition was used - Code check required"
      )
    } else {
      new_code_object <- list(
        code = new_code,
        status = "success",
        text = ""
      )
    }

    return(new_code_object)

  }
}

#' Check covariate definition table
#'
#' @param table a data.frame with expected columns
#' @param parms a data.frame of parameter information extracted from the
#' description expected from the control stream header
#' @param timevarying a logical value indicating where at least one covariate is
#' time varying
#' @param check_step a logical value indicating where to check content of Step column or not
#' @noRd


check_covariate_table <- function(
    table = NULL,
    parms = NULL,
    timevarying = FALSE,
    check_step = FALSE
){

  req( parms )

  if ( length(table) == 0 ){
    return( table )
  }

  # Stage
  if ( !all( is.na(table$Stage) ) ) {
    if (
      !(
        ( inherits(table$Stage, "character") | inherits(table$Stage, "factor") ) &
        all( is.na(table$Stage) | table$Stage %in% c("Forward", "Backward") )
      )
    ){
      stop("Invalid content in Stage column")
    }
  }

  # Step
  if ( isTRUE(check_step) && !all( is.na(table$Step) ) ) {
    if (
      # ( inherits(table$Step, "character") | inherits(table$Step, "factor") ) &
      # !all( is.na(table$Step) | grepl("^[FB][0-9]+$", table$Step)  )
      !is.numeric(table$Step)
    ) {
      stop("Invalid content in Step column")
    }
  }

  # Parameter
  if ( !all( is.na(table$Parameter) ) ) {
    if (
      !(
        ( inherits(table$Parameter, "character") | inherits(table$Parameter, "factor") ) &
        all( is.na(table$Parameter) | grepl("^[[:alnum:]_]+$", table$Parameter)  )
      )
    ) {
      stop("Invalid content in Parameter column")
    }
  }

  # Covariate
  if ( !all( is.na(table$Covariate) ) ) {
    if (
      !(
        ( inherits(table$Covariate, "character") | inherits(table$Covariate, "factor") ) &
        all( is.na(table$Covariate) | grepl("^[[:alnum:]_]+$", table$Covariate)  )
      )
    ){
      stop("Invalid content in Covariate column")
    }
  }

  # Type
  if ( !all( is.na(table$Type) ) ) {
    if (
      !(
        ( inherits(table$Type, "character") | inherits(table$Type, "factor") ) &
        all( is.na(table$Type) | table$Type %in% c("Continuous", "Discrete") )
      )
    ){
      stop("Invalid content in Type column")
    }
  }

  # Function
  mu <- !all( is.na(parms$MU) )

  if ( !all( is.na(table$Function) ) ) {
    if (
      !(
        ( inherits(table$Function, "character") | inherits(table$Function, "factor") ) &
        all( is.na(table$Function) | table$Function %in% c("Linear", "Power", "Exponential", "Additive", "Proportional", "Direct proportional") )
      )
    ) {
      stop("Invalid content in Function column")
    }
  }

  chk <- table %>%
    dplyr::left_join( parms, by = "Parameter" ) %>%
    dplyr::mutate(
      complete = !is.na(Parameter) & !is.na(Covariate) & !is.na(Type) & !is.na(Scale) & !is.na(Function),
      invalid = dplyr::case_when(
        !complete ~ FALSE,
        # with or without MU referencing regardless of covariate time variance
        Scale == "log" & !Function %in% c("Power", "Direct proportional") ~ TRUE,
        # no MU referencing regardless of covariate time variance
        !mu & Type == "Continuous" & Function %in% c("Additive", "Proportional", "Direct proportional") ~ TRUE,
        !mu & Type != "Continuous" & Function %in% c("Linear", "Power") ~ TRUE,
        # MU referencing without covariate time variance
        mu & !timevarying & Type == "Continuous" &
          Function %in% c("Additive", "Proportional", "Direct proportional") ~ TRUE,
        mu & !timevarying & Type != "Continuous" &
          Function %in% c("Linear", "Power") ~ TRUE,
        # MU referencing with covariate time variance
        mu & timevarying & Scale == "linear" & Variability %in% c("none", "exp") & Type == "Continuous" &
          Function %in% c("Additive", "Proportional", "Direct proportional") ~ TRUE,
        mu & timevarying & Scale == "linear" & Variability %in% c("none", "exp") & Type != "Continuous" &
          Function %in% c("Linear", "Power") ~ TRUE,
        mu & timevarying & Scale == "linear" & Variability == "add" & Type == "Continuous" &
          Function != "Linear" ~ TRUE,
        mu & timevarying & Scale == "linear" & Variability == "add" & Type != "Continuous" &
          Function != "Additive" ~ TRUE,
        mu & timevarying & Variability == "logit" & Type == "Continuous" &
          Function != "Linear" ~ TRUE,
        mu & timevarying & Variability == "logit" & Type != "Continuous" &
          Function != "Additive" ~ TRUE,
        TRUE ~ FALSE
      )
    )

  if ( any( chk$invalid ) ){
    stop(
      paste(
        c(
          "Invalid functional form(s) for the selection of covariate type, scale, MU referencing, and/or time-varying covariates:",
          chk %>%
            dplyr::filter( invalid ) %>%
            dplyr::mutate( combo = paste( Stage, Step, Parameter, Covariate, Function, sep = " / ") ) %>%
            dplyr::pull( combo )
        ),
        collapse = "@@@"
      )
    )
  }

  # Center
  if ( !all( is.na(table$Center) ) ) {
    if ( !is.numeric(table$Center) ){
      stop("Invalid content in Center column")
    }
  }

  # Flags
  if ( !all( is.na(table$Flags) ) ) {
    if (
      !(
        ( inherits(table$Flags, "character") | inherits(table$Flags, "factor") ) &
        all( is.na(table$Flags) | grepl("^[a-zA-Z0-9:]+$", table$Flags)  )
      )
    ){
      stop("Invalid content in Flags column")
    }

    if ( any( grepl("^:|:$", table$Flags) ) ) {
      stop("Invalid content in Flags column")
    }
  }

  nFlags <- sapply(
    table$Flags,
    function(x) {
      ifelse(
        is.na(x),
        length( NA ) ,
        length( unlist(strsplit(x, ":")) )
      )
    }
  )

  # Initial
  if ( !all( is.na(table$Initial) ) ) {
    if (
      !(
        ( inherits(table$Initial, "numeric") | inherits(table$Initial, "character") | inherits(table$Initial, "factor") ) &
        all( is.na(table$Initial) | grepl("[0-9:.]+$", table$Initial)  )
      )
    ){
      stop("Invalid content in Initial column")
    }

    if ( any( grepl("^:|:$", table$Initial) ) ) {
      stop("Invalid content in Initial column")
    }
  }

  if ( !inherits(table$Initial, "numeric") ){
    nInitials <- sapply(
      table$Initial,
      function(x) length( unlist(strsplit(x, ":")) )
    )
  } else {
    nInitials <- rep(1, nrow(table) )
  }

  if ( !all(nFlags == nInitials) ) {
    stop("Inconsistent number of elements in Flags and Initial columns")
  }

  # Action
  if ( !all( is.na(table$Action) ) ) {
    if (
      !(
        ( inherits(table$Action, "character") | inherits(table$Action, "factor") ) &
        all( is.na(table$Action) | table$Action %in% c("Create", "Do not create", "Select", "Remove") )
      )
    ) {
      stop("Invalid content in Action column")
    }
  }

  if (
    table %>%
    dplyr::filter(Stage == "Forward" & Action == "Remove") %>%
    nrow() > 0
  ){
    stop("Invalid content in Action column")
  }
  if (
    table %>%
    dplyr::filter(Stage == "Backward" & Action == "Select") %>%
    nrow() > 0
  ){
    stop("Invalid content in Action column")
  }

  return(table)

}

#' Check if covariate definition table contains rows with incomplete data
#'
#' @param table a data.frame with expected columns
#' @noRd


check_incomplete_covariate_table <- function(table){

  table <- table %>%
    dplyr::filter(
      !(
        is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
          is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
      )
    )

  # All rows must have at minimum Parameter, Covariate, Type, Function, Initial estimate, and Action
  if (
    table %>%
    dplyr::filter(
      is.na(Covariate) | is.na(Parameter) | is.na(Type) | is.na(Function) |
      is.na(Initial) | is.na(Action)
    ) %>%
    nrow() > 0
  ){
    return("Data in one or more rows are incomplete")
  }

  # All rows with Center must be Continuous Type
  tmp <- table %>%
    dplyr::filter( !is.na(Center) )
  if ( any(tmp$Type == "Discrete") ){
    return("Centering values should only be provided for continuous covariate")
  }

  # All rows with Flags must be Discrete Type
  tmp <- table %>%
    dplyr::filter( !is.na(Flags) )
  if ( any(tmp$Type == "Continuous") ){
    return("Dichotomous flags should only be provided for discrete covariate")
  }

  return("Valid")

}

#' Convert covariate handsontable data to R data.frame without factor
#'
#' @param ... passed to rhandsontable::hot_to_r
#' @noRd

hot_to_r_raw <- function(...){

  DF <- rhandsontable::hot_to_r(...)
  names(DF) <- c(
    "Stage", "Step", "Parameter", "Covariate", "Type", "Function", "Center",
    "Flags", "Initial", "Action"
  )
  DF <- DF %>%
    dplyr::mutate(
      Type = levels(Type)[Type],
      Function = levels(Function)[Function],
      Action = levels(Action)[Action]
    ) %>%
    # Convert "" to NA
    dplyr::mutate(
      Parameter = as.character( ifelse( Parameter == "", NA_character_, Parameter ) ),
      Covariate = as.character( ifelse( Covariate == "", NA_character_, Covariate ) ),
      Type = as.character( ifelse( Type == "", NA_character_, Type ) ),
      Function = as.character( ifelse( Function == "", NA_character_, Function ) ),
      Center = as.numeric( ifelse( Center == "", NA_real_, Center ) ),
      Flags = as.character( ifelse( Flags == "", NA_character_, Flags ) ),
      Initial = as.character( ifelse( Initial == "", NA, Initial ) ),
      Action = as.character( ifelse( Action == "", NA_character_, Action ) )
    )

}


#' Generate univariate model code
#'
#' @param code A character string containing the code of the reference NONMEM model
#' @param parms A data.frame of parameter information extracted from the
#' description expected from the control stream header
#' @param referenceName The name of the reference model file
#' @param nThetas The number of THETA parameters in the reference NONMEM model
#' @param table The table of covariate relationship definition
#' @param timeVarying A logical value indicating if at least one covariate is
#' time varying
#' @param style Either PsN or standard
#' @param prefix If standard style, the prefix to start the name of the univariable
#' model file with
#' @param startNumber If PsN style, the number to start univariate model run files
#' @param path The directory in which the univariate models will be saved and run
#' @noRd

create_univariate_models <- function(
    code,
    parms,
    referenceName,
    nThetas,
    table,
    timeVarying,
    style,
    prefix,
    startNumber,
    path
) {
  if ( missing(code) )
    return("Error")

  # Clean up table ----
  table <- table %>%
    dplyr::filter(
      !(
        is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
          is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
      )
    ) %>%
    dplyr::filter(
      !Action %in% c( "Do not create", "Select" )
    )

  # Add parameter information, name and info variables in table ----
  table <- table %>%
    dplyr::left_join( parms, by = "Parameter" ) %>%
    # Create version #
    dplyr::group_by( Parameter, Covariate, Function ) %>%
    dplyr::mutate( Version = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    # Get name
    dplyr::mutate(
      Prefix = prefix,
      AbbreviatedFunction = dplyr::case_when(
        Function == "Linear" ~ "lin",
        Function == "Power" ~ "pow",
        Function == "Exponential" ~ "exp",
        Function == "Additive" ~ "add",
        Function == "Proportional" ~ "prop",
        Function == "Direct proportional" ~ "dirprop"
      ),
      name = glue::glue(
        paste(
          "{tolower(Prefix)}{Step}",
          "{tolower(Parameter)}",
          "{tolower(Covariate)}",
          "{AbbreviatedFunction}",
          "{sprintf('%02d', Version)}",
          sep = "-"
        )
      ),
      info = glue::glue(
        paste(
          "{Stage}",
          "{Step}",
          "{Parameter}",
          "{Covariate}",
          "{Type}",
          "{Function}",
          "{ifelse(is.na(Center), '-', Center)}",
          "{ifelse(is.na(Flags), '-', Flags)}",
          sep = " / "
        )
      )
    ) %>%
    dplyr::arrange(name)

  # Get reference model name
  referenceDir <- dirname(referenceName)
  referenceFilename <- basename(referenceName)
  referenceName <- sub("[.].*$", "", referenceFilename)
  if ( style == "PsN" ){
    runno <- sub(".*?([0-9]+)$", "\\1", referenceName)
  } else {
    runno <- NA_character_
  }

  # Get stage and steo
  stage <- table$Stage[1]
  step <- table$Step[1]

  code <- unlist( strsplit(gsub("\n", " @@@", code), split = " @@@") )

  # Forward selection ----
  if ( stage == "Forward" ){

    # Add tags in header section of temple code ---
    if ( step == 1 ) {
      if ( style == "Standard" ){
        insertAfter <- which(grepl("^;;--[-]*", code))[2]
        insertLines <- c()
        if ( timeVarying & !any( grepl( ";;    Time-varying covariates", code) )){
          insertLines <- c( insertLines, ";;    Time-varying covariates" )
        }
        insertLines <- c(
          insertLines,
          ";; Covariate selection:",
          ";;    Forward step 1: effect of @COVARIATE on @PARAMETER as @FUNCTION relationship"
        )
      } else {
        insertAfter <- which(grepl("^;; 2. Description:", code))[1]
        insertLines <- ";;    Covariate selection"
        code <- c(
          code[1:insertAfter],
          insertLines,
          code[(insertAfter+1):length(code)]
        )
        insertAfter <- which(grepl("^;; 5. Covariate model:", code))[1]
        insertLines <- c()
        if ( timeVarying & !any( grepl( ";;    Time-varying covariates", code) ) ) {
          insertLines <- c( insertLines, ";;    Time-varying covariates" )
        }
        insertLines <- c(
          insertLines,
          ";;    Forward step 1: effect of @COVARIATE on @PARAMETER as @FUNCTION relationship"
        )
      }
    } else {
      insertAfter <- which(
        grepl(glue::glue("^;;    Forward step {step-1}"), code)
      )[1]
      insertLines <- glue::glue(
        ";;    Forward step {step}: effect of @COVARIATE on @PARAMETER as @FUNCTION relationship"
      )
    }

    if ( style == "Standard" & !grepl("^;;\\s*$", code[insertAfter+1]) ) {
      insertLines <- c(insertLines, ";;")
    }

    code <- c(
      code[1:insertAfter],
      insertLines,
      code[(insertAfter+1):length(code)]
    )

    # Get all NONMEM blocks
    blockCode <- get_nonmem_blocks( paste(code, collapse = " @@@") )

    # Add @THETACOV placeholder tag at the end of the last $THETA block for later
    # replacement
    thetaBlock <- which(grepl("^[$]THE|^[$]THET|^[$]THETA", blockCode))
    blockCode[ max(thetaBlock) ] <- paste(
      c(
        unlist( strsplit(blockCode[max(thetaBlock)] , split = " @@@") ),
        "@THETACOV @@@"
      ),
      collapse = " @@@"
    )

    # Add @CODECOV tag
    pkpredBlock <- which(grepl("^[$]PK|^[$]PRE|^[$]PRED", blockCode))
    pkpred <- unlist( strsplit(blockCode[min(pkpredBlock)] , split = " @@@") )
    if ( grepl("@@@$", blockCode[min(pkpredBlock)]) ){
      pkpred <- c(pkpred, "")
    }
    index <- which(grepl(";-- COVARIATE EFFECT START", pkpred))
    if ( length(index ) == 0){
      index <- min(
        which( grepl("^\\s*CALLFL\\s*=", pkpred) ),
        which( grepl("^[$]PK", pkpred) ),
        which( grepl("^[$]PRED", pkpred) )
      )
    } else {
      index <- max(
        index,
        which( grepl(glue::glue("^\\s+COV{step-1}"), pkpred) )
      )
    }

    pkpred <- c(
      pkpred[1:index],
      "@CODECOV",
      pkpred[(index+1):length(pkpred)]
    )

    blockCode[ pkpredBlock ] <- paste(pkpred, collapse = " @@@")

    # Convert from NONMEM blocks to line by line format
    code <- paste(blockCode, collapse = " @@@")
    code <- unlist( strsplit(gsub("\n", " @@@", code), split = " @@@") )

    # Add @COVDEF tag
    index <- which(grepl("^;--covdef-", code))
    if ( length(index) > 0 && index != length(code) ){
      index <- index[length(index)]
      code <- c(
        code[1:index],
        ";--covdef- @COVDEF",
        code[(index+1):length(code)]
      )
    } else {
      if ( length(index) == 0) {
        code <- c( code, "" )
      }
      code <- c( code, ";--covdef- @COVDEF" )
    }

    # * Create univariate control streams ----

    univariates <- vector("list", nrow(table))
    mu <- !all(is.na(table$MU))

    for ( iEffect in 1:nrow(table) ){

      # Create a copy
      tmp <- code

      # Add MCOV_x and ACOV_x code
      parm <- table$Parameter[iEffect]
      variability <- table$Variability[iEffect]
      scale <- table$Scale[iEffect]

        # MCOV and ACOV definition rows
      mcovx_index <- which( grepl( glue::glue( "^\\s*MCOV_{parm}\\s*=" ), tmp ) )
      acovx_index <- which( grepl( glue::glue( "^\\s*ACOV_{parm}\\s*=" ), tmp ) )

        # MCOV and ACOV usage rows
      mcovx_used <- setdiff(
        which( grepl( glue::glue( "=.*MCOV_{parm}" ), tmp ) ),
        mcovx_index
      )
      acovx_used <- setdiff(
        which( grepl( glue::glue( "=.*ACOV_{parm}" ), tmp ) ),
        acovx_index
      )
        # MCOV_X not possible for log scale and for linear scale and additive
        # variability or logit scale when MU referencing and time varying covariates
        # are used

      if (
        scale == "log" |
        ( scale == "linear" & variability == "add" & mu & timeVarying ) |
        ( scale == "logit" & mu & timeVarying )
      ){
        if ( length(mcovx_index) > 0 ){
          univariates[[iEffect]] <- glue::glue(
            paste(
              "MCOV_{parm} was defined in the reference control stream but",
              "cannot be used with {parm} which is parameterized {text}"
            ),
            text = dplyr::case_when(
              scale == "linear" & variability == "add" & mu & timeVarying ~
                "on the linear scale with an additive variability model , with MU referencing and time varying covariates",
              scale == "logit" & mu & timeVarying ~
                "on the logit scale, with MU referencing and time varying covariates",
              TRUE ~ "on the log scale"
            )
          )
          next
        }
      }

      if ( length(mcovx_index) + length(acovx_index) == 1 ){
        if (
          !(
            scale == "log" |
            ( scale == "linear" & variability == "add" & mu & timeVarying ) |
            ( scale == "logit" & mu & timeVarying ) )
        ) {
          univariates[[iEffect]] <- glue::glue(
            "Both MCOV_{parm} and ACOV_{parm} must be defined in the reference control stream",
          )
          next
        }
      }
      if ( length(mcovx_index) > 1 ){
        univariates[[iEffect]] <- glue::glue(
          "MCOV_{parm} was defined more than once in the reference control stream"
        )
        next
      }
      if ( length(acovx_index) > 1 ){
        univariates[[iEffect]] <- glue::glue(
          "ACOV_{parm} was defined more than once in the reference control stream"
        )
        next
      }
      if ( length(mcovx_index) + length(acovx_index) == 0 ){

        if ( length(mcovx_used) + length(acovx_used) > 0 ){
          univariates[[iEffect]] <-  glue::glue(
            "MCOV_{parm} and/or ACOV_{parm} were used but not defined in the reference control stream"
          )
          next
        }

        covblock_end_index <- which( tmp == ";-- COVARIATE EFFECT END" )

        # Insert ACOV_x and/or MCOV_x
        if ( scale == "log" ){
          tmp <- c(
            tmp[ 1:covblock_end_index-1 ],
            glue::glue( "  ACOV_{parm} = 0" ),
            tmp[ covblock_end_index:length(tmp) ]
          )

          if ( !(mu & timeVarying) | variability == "none"  ){
            # Add ACOV_x on Lx equation only if not already present
            if ( !any(grepl( glue::glue( "^\\s*L{parm}.*ACOV_{parm}" ), tmp ) ) ) {
              tmp <- sub(
                glue::glue( "^(\\s*)L{parm}\\s*=\\s*(.*)" ),
                glue::glue( "\\1L{parm} = ACOV_{parm} + ( \\2 ) "),
                tmp
              )
            }
          } else {
            # Add ACOV_x on TVx and X equations only if not already present
            if ( !any(grepl( glue::glue( "^\\s*TV{parm}.*ACOV_{parm}" ), tmp ) ) ) {
              tmp <- sub(
                glue::glue( "^(\\s*)TV{parm}\\s*=\\s*EXP[(]\\s*(.*)\\s*[)]" ),
                glue::glue( "\\1TV{parm} = EXP( ACOV_{parm} + ( \\2 ) )"),
                tmp
              )
            }
            if ( !any(grepl( glue::glue( "^\\s*{parm}.*ACOV_{parm}" ), tmp ) ) ) {
              tmp <- sub(
                glue::glue( "^(\\s*){parm}\\s*=\\s*(.*)" ),
                glue::glue( "\\1{parm} = EXP( ACOV_{parm} ) * ( \\2 )"),
                tmp
              )
            }
          }
        } else if ( variability == "logit" & mu & timeVarying ) {
          tmp <- c(
            tmp[ 1:covblock_end_index-1 ],
            glue::glue( "  ACOV_{parm} = 0" ),
            tmp[ covblock_end_index:length(tmp) ]
          )
          # Add ACOV_x on TVx and X equations only if not already present
          if ( !any(grepl( glue::glue( "^\\s*TV{parm}.*ACOV_{parm}" ), tmp ) ) ) {
            tmp <- sub(
              glue::glue( "^(\\s*)TV{parm}\\s*=(.*)L{parm}(.*)" ),
              glue::glue( "\\1TV{parm} =\\2(ACOV_{parm} + L{parm})\\3"),
              tmp
            )
          }
          if ( !any(grepl( glue::glue( "^\\s*{parm}.*ACOV_{parm}" ), tmp ) ) ) {
            tmp <- sub(
              glue::glue( "^(\\s*){parm}\\s*=(.*)MU_(.*)" ),
              glue::glue( "\\1{parm} =\\2ACOV_{parm} + (MU_\\3 )"),
              tmp
            )
          }

        } else if ( scale == "linear" & variability == "add"  & mu & timeVarying ) {

          tmp <- c(
            tmp[ 1:covblock_end_index-1 ],
            glue::glue( "  ACOV_{parm} = 0" ),
            tmp[ covblock_end_index:length(tmp) ]
          )
          # Add ACOV_x on TVx and X equations only if not already present
          if ( !any(grepl( glue::glue( "^\\s*TV{parm}.*ACOV_{parm}" ), tmp ) ) ) {
            tmp <- sub(
              glue::glue( "^(\\s*)TV{parm}\\s*=(.*)L{parm}(.*)" ),
              glue::glue( "\\1TV{parm} =\\2ACOV_{parm} + L{parm}\\3"),
              tmp
            )
          }
          if ( !any(grepl( glue::glue( "^\\s*{parm}.*ACOV_{parm}" ), tmp ) ) ) {
            tmp <- sub(
              glue::glue( "^(\\s*){parm}\\s*=(.*)MU_(.*)" ),
              glue::glue( "\\1{parm} =\\2ACOV_{parm} + ( MU_\\3 )"),
              tmp
            )
          }
        } else if ( scale == "linear" & variability == "exp"  & mu & timeVarying ) {
          tmp <- c(
            tmp[ 1:covblock_end_index-1 ],
            glue::glue( "  MCOV_{parm} = 1" ),
            glue::glue( "  ACOV_{parm} = 0" ),
            tmp[ covblock_end_index:length(tmp) ]
          )
          # Add MCOV_x and ACOV_x on TVx and X equations only if not already present
          if ( !any(grepl( glue::glue( "^\\s*TV{parm}.*ACOV_{parm}" ), tmp ) ) ) {
            tmp <- sub(
              glue::glue( "^(\\s*)TV{parm}\\s*=(.*)L{parm}(.*)" ),
              glue::glue( "\\1TV{parm} =\\2(ACOV_{parm} + MCOV_{parm}*L{parm})\\3"),
              tmp
            )
          }
          if ( !any(grepl( glue::glue( "^\\s*{parm}.*ACOV_{parm}" ), tmp ) ) ) {
            tmp <- sub(
              glue::glue( "^(\\s*){parm}\\s*=\\s*(.*)EXP[(]\\s*MU_(.*)" ),
              glue::glue( "\\1{parm} = \\2(ACOV_{parm}/L{parm} + MCOV_{parm}) * EXP(MU_\\3"),
              tmp
            )
          }
        } else {
          tmp <- c(
            tmp[ 1:covblock_end_index-1 ],
            glue::glue( "  MCOV_{parm} = 1" ),
            glue::glue( "  ACOV_{parm} = 0" ),
            tmp[ covblock_end_index:length(tmp) ]
          )
          # Add MCOV_x and ACOV_x on Lx equation only if not already present
          if (
            !any( grepl( glue::glue( "^\\s*L{parm}.*ACOV_{parm}" ), tmp ) )&
            !any( grepl( glue::glue( "^\\s*L{parm}.*MCOV_{parm}" ), tmp ) )
          ){
            tmp <- sub(
              glue::glue( "^(\\s*)L{parm}\\s*=\\s*(.*)" ),
              glue::glue( "\\1L{parm} = ACOV_{parm} + MCOV_{parm} * ( \\2 )"),
              tmp
            )
          }
        }
      }

      # Update date in header section
      index <- which(grepl("^;; Created on", tmp))[1]
      tmp[index] <- glue::glue(
        ";; Created on {date} by {Sys.info()['user']}",
        date = format(Sys.time(), "%b %d, %Y %H:%M:%S %Z")
      )

      # Update path in header section
      index <- which(grepl("^;; Name:", tmp))[1]
      tmp[index] <- glue::glue(
        ";; Name: {path}/{referenceFilename}",
        path = ifelse(
          length(path) == 0,
          referenceDir,
          path
        )
      )

      # Update other instance of filename
      if ( style == "Standard") {
        tmp <- gsub(
          pattern = referenceName,
          replacement = table$name[iEffect],
          tmp
        )
      } else {
        tmp <- gsub(
          pattern = referenceName,
          replacement = sprintf(
            ifelse(
              startNumber + nrow(table) < 1000,
              "run%03d",
              "run%04d"
            ),
            startNumber + iEffect - 1
          ),
          tmp
        )
        tmp <- gsub(
          glue::glue("(FILE\\s*=\\s*[[:alpha:]]+){runno}"),
          sprintf(
            ifelse(
              startNumber + nrow(table) < 1000,
              "\\1%03d",
              "\\1%04d"
            ),
            startNumber + iEffect - 1
          ),
          tmp
        )
      }

      # Replace @COVARIATE tag
      tmp <- sub("@COVARIATE", table$Covariate[iEffect], tmp)

      # Replace @PARAMETER tag
      tmp <- sub("@PARAMETER", parm, tmp)

      # Replace @FUNCTION tag
      tmp <- sub(
        "@FUNCTION",
        paste(
          ifelse( table$Function[iEffect] %in% c("Additive", "Exponential"), "an", "a"),
          tolower(table$Function[iEffect]),
          "effect"
        ),
        tmp
      )

      # Replace @COVDEF tag
      tmp <- sub("@COVDEF", table$info[iEffect], tmp)

      # Replace @THETACOV tag
      initials <- unlist( strsplit(table$Initial[iEffect], ":") )
      tag_replacement <- c()

      for ( iTheta in 1:length(initials) ){
        covariate <- ifelse(
          length(initials) == 1,
          table$Covariate[iEffect],
          unlist( strsplit(table$Flags[iEffect], ":") )[iTheta]
        )

        tag_replacement <- c(
          tag_replacement,
          sprintf(
            glue::glue(
              "  ({lo}, {init}, +INF)  ;--th{iTheta+nThetas}- {key}: {label}",
              lo = dplyr::case_when(
                table$Function[iEffect] == "Proportional" ~ "-1",
                table$Function[iEffect] == "Direct proportional" ~ "0",
                TRUE ~ "-INF"
              ),
              init = initials[iTheta],
              key = parm,
              label = dplyr::case_when(
                table$Function[iEffect] == "Linear" ~ glue::glue(
                  "Slope of {effect} effect on {scale_start}{parm}{scale_end}",
                  effect = ifelse(
                    is.na(table$Center[iEffect]),
                    covariate,
                    glue::glue(
                      "{covariate}{sign}{abs(table$Center[iEffect])}",
                      sign = ifelse( table$Center[iEffect] < 0, "+", "-")
                    )
                  ),
                  scale_start = ifelse( variability == "logit", "logit(", "" ),
                  scale_end = ifelse( variability == "logit", ")", "" )
                ),
                table$Function[iEffect] == "Power" ~ glue::glue(
                  "Exponent of {effect} effect on {scale_start}{parm}{scale_end}",
                  effect = ifelse(
                    is.na(table$Center[iEffect]),
                    covariate,
                    glue::glue(
                      "{sign}{covariate}/{abs(table$Center[iEffect])}",
                      sign = ifelse(table$Center[iEffect] < 0, "-", "")
                    )
                  ),
                  scale_start = ifelse( variability == "logit", "logit(", "" ),
                  scale_end = ifelse( variability == "logit", ")", "" )
                ),
                variability != "logit" & table$Function[iEffect] == "Exponential" & table$Type[iEffect] == "Continuous" ~ glue::glue(
                  "Slope of {effect} effect on log({parm})",
                  effect = ifelse(
                    is.na(table$Center[iEffect]),
                    covariate,
                    glue::glue(
                      "{covariate}{sign}{abs(table$Center[iEffect])}",
                      sign = ifelse(table$Center[iEffect] < 0, "+", "-")
                    )
                  )
                ),
                variability == "logit" & table$Function[iEffect] == "Exponential" & table$Type[iEffect] == "Continuous" ~ glue::glue(
                  "Exponent of {effect} exponential effect on logit({parm})",
                  effect = ifelse(
                    is.na(table$Center[iEffect]),
                    covariate,
                    glue::glue(
                      "{covariate}{sign}{abs(table$Center[iEffect])}",
                      sign = ifelse(table$Center[iEffect] < 0, "+", "-")
                    )
                  )
                ),
                table$Function[iEffect] == "Exponential" & table$Type[iEffect] == "Discrete" ~ glue::glue(
                  "Fold-change in {scale_start}{parm}{scale_end} for {covariate} = 1 [log]",
                  scale_start = ifelse( variability == "logit", "logit(", "" ),
                  scale_end = ifelse( variability == "logit", ")", "" )
                ),
                table$Function[iEffect] == "Additive" ~ glue::glue(
                  "Additive shift in {scale_start}{parm}{scale_end} for {covariate} = 1",
                  scale_start = ifelse( variability == "logit", "logit(", "" ),
                  scale_end = ifelse( variability == "logit", ")", "" )
                ),
                table$Function[iEffect] == "Proportional" ~ glue::glue(
                  "Proportional shift in {scale_start}{parm}{scale_end} for {covariate} = 1",
                  scale_start = ifelse( variability == "logit", "logit(", "" ),
                  scale_end = ifelse( variability == "logit", ")", "" )
                ),
                table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                  "Fold-change in {scale_start}{parm}{scale_end} for {covariate} = 1",
                  scale_start = ifelse( variability == "logit", "logit(", "" ),
                  scale_end = ifelse( variability == "logit", ")", "" )
                )
              )
            )
          )
        )
      }

      tmp <- sub("@THETACOV", paste(tag_replacement, collapse = "\n"), tmp)

      # Replace @CODECOV tag nom
      tag_replacement <- c()

      for ( iTheta in 1:length(initials) ){
        covariate <- ifelse(
          length(initials) == 1,
          table$Covariate[iEffect],
          unlist( strsplit(table$Flags[iEffect], ":") )[iTheta]
        )

        if ( iTheta == 1 ){
          tag_replacement <- ifelse(
            table$Type[iEffect] == "Continuous",
            # Continuous effects
            dplyr::case_when(
              # Log scale
              table$Scale[iEffect] == "log" & table$Function[iEffect] == "Power" ~ glue::glue(
                "LOG({sign}{covariate}/{abs(table$Center[iEffect])})*THETA({iTheta + nThetas})",
                sign = ifelse( table$Center[iEffect] < 0, "-", "")
              ),
              # Linear and logit scale
              table$Function[iEffect] == "Linear" ~ glue::glue(
                "THETA({iTheta + nThetas})*({covariate}{sign}{abs(table$Center[iEffect])})",
                sign = ifelse( table$Center[iEffect] < 0, "+", "-")
              ),
              table$Function[iEffect] == "Exponential" ~ glue::glue(
                "EXP(THETA({iTheta + nThetas})*({covariate}{sign}{abs(table$Center[iEffect])}))",
                sign = ifelse( table$Center[iEffect] < 0, "+", "-")
              ),
              table$Function[iEffect] == "Power" ~ glue::glue(
                "({sign}{covariate}/{abs(table$Center[iEffect])})**THETA({iTheta + nThetas})",
                sign = ifelse( table$Center[iEffect] < 0, "-", "")
              )
            ),
            # Discrete effects
            dplyr::case_when(
              # Log scale
              table$Scale[iEffect] == "log" & table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                "THETA({iTheta + nThetas})*{covariate}"
              ),
              # Linear and logit scale
              table$Function[iEffect] == "Additive" ~ glue::glue(
                "THETA({iTheta + nThetas})*{covariate}"
              ),
              table$Function[iEffect] == "Proportional" ~ glue::glue(
                "1 + THETA({iTheta + nThetas})*{covariate}"
              ),
              table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                "THETA({iTheta + nThetas})**{covariate}"
              ),
              table$Function[iEffect] == "Exponential" ~ glue::glue(
                "EXP(THETA({iTheta + nThetas})*{covariate}"
              )
            )
          )
        }

        # Categorical covariate effects
        if ( iTheta > 1 ){
          tag_replacement <- paste0(
            tag_replacement,
            dplyr::case_when(
              # Log scale
              table$Scale[iEffect] == "log" & table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                " + THETA({iTheta + nThetas})*{covariate}"
              ),
              # Linear and logit scale
              table$Scale[iEffect] != "log" & table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                " * THETA({iTheta + nThetas})**{covariate}"
              ),
              TRUE ~ glue::glue( " + THETA({iTheta + nThetas})*{covariate}" )
            )
          )
        }

        if ( iTheta == length(initials) &
             table$Scale[iEffect] == "linear" &
             table$Type[iEffect] == "Discrete" &
             table$Function[iEffect] == "Exponential"){
          tag_replacement <- paste0(tag_replacement, ")")
        }

      }

      tmp <- sub("@CODECOV", sprintf("  COV%s = %s", step, tag_replacement), tmp)
      rm(tag_replacement)

      # Insert covariate on the appropriate MCOV or ACOV line
      if (
        table$Scale[iEffect] == "log" |
        ( table$Function[iEffect] != "log" & table$Function[iEffect] %in% c("Additive", "Linear") )
      ) {
        # Additive effects
        index <- which( grepl( glue::glue( "^\\s*ACOV_{parm}\\s* = " ), tmp ) )
        tmp[index] <- sub(
          " 0 [+] ",
          " ",
          paste0( tmp[index], glue::glue(" + COV{step}") )
        )
      } else {
        # Multiplicative effects
        index <- which( grepl( glue::glue( "^\\s*MCOV_{parm}\\s* = " ), tmp ) )
        tmp[index] <- sub(
          " 1 [*] ",
          " ",
          paste0( tmp[index], glue::glue(" * COV{step}") )
        )
      }

      # Finalize
      univariates[[iEffect]] <- paste(tmp, collapse = "\n")
      names(univariates)[iEffect] <- ifelse(
        style == "Standard",
        paste0(table$name[iEffect], ".ctl"),
        sprintf(
          ifelse(
            startNumber + nrow(table) < 1000,
            "run%03d.mod",
            "run%04d.mod"
          ),
          startNumber + iEffect - 1
        )
      )
    }

  # Backward elimination ----

  } else {

    # Add tags in header section of temple code ---
    if ( step == 1 ) {
      if ( style == "Standard" ){
        insertAfter <- which(grepl("^;;--[-]*", code))[2]
        insertLines <- c(
          ";; Backward elimination:",
          ";;    Backward step 1: removal of effect of @COVARIATE on @PARAMETER as @FUNCTION relationship"
        )
      } else {
        insertAfter <- which(grepl("^;; 5. Covariate model:", code))[1]
        insertLines <- ";;    Backward step 1: removal of effect of @COVARIATE on @PARAMETER as @FUNCTION relationship"
      }
    } else {
      insertAfter <- which(
        grepl(glue::glue("^;;    Backward step {step-1}"), code)
      )[1]
      insertLines <- glue::glue(
        ";;    Backward step {step}: removal of effect of @COVARIATE on @PARAMETER as @FUNCTION relationship"
      )
    }

    if ( style == "Standard" & !grepl("^;;\\s*$", code[insertAfter+1]) ) {
      insertLines <- c(insertLines, ";;")
    }

    code <- c(
      code[1:insertAfter],
      insertLines,
      code[(insertAfter+1):length(code)]
    )

    # Find regex pattern to turn off covariate effect
    table <- table %>%
      dplyr::mutate(
        pattern = dplyr::case_when(
          Function == "Linear" ~ glue::glue("Slope of .*{Covariate}.* effect on {Parameter}"),
          Function == "Power" ~ glue::glue("Exponent of .*{Covariate}.* effect on {Parameter}"),
          Function == "Linear" ~ glue::glue("Slope of .*{Covariate}.* effect on {Parameter}"),
          Function == "Exponential" & Type == "Continuous" ~ glue::glue("Slope of .*{Covariate}.* effect on log({Parameter})"),
          Function == "Exponential" & Type != "Continuous" ~ glue::glue( "Additive shift in log({Parameter}) for {Covariate} = 1"),
          Function == "Additive" ~ glue::glue( "Additive shift in {Parameter} for {Covariate} = 1"),
          Function == "Proportional" ~ glue::glue( "Proportional shift in {Parameter} for {Covariate} = 1"),
          TRUE ~ glue::glue("Fold-change in {Parameter} for {Covariate} = 1")
        )
      )

    # * Create univariate control streams ----

    univariates <- vector("list", nrow(table))

    for ( iEffect in 1:nrow(table) ){

      # Create a copy
      tmp <- code

      # Update date in header section
      index <- which(grepl("^;; Created on", tmp))[1]
      tmp[index] <- glue::glue(
        ";; Created on {date} by {Sys.info()['user']}",
        date = format(Sys.time(), "%b %d, %Y %H:%M:%S %Z")
      )

      # Update path in header section
      index <- which(grepl("^;; Name:", tmp))[1]
      tmp[index] <- glue::glue(
        ";; Name: {path}/{referenceFilename}",
        path = ifelse(
          length(path) == 0,
          referenceDir,
          path
        )
      )

      # Update other instance of filename
      if ( style == "Standard") {
        tmp <- gsub(
          pattern = referenceName,
          replacement = table$name[iEffect],
          tmp
        )
      } else {
        tmp <- gsub(
          pattern = referenceName,
          replacement = sprintf(
            ifelse(
              startNumber + nrow(table) < 1000,
              "run%03d",
              "run%04d"
            ),
            startNumber + iEffect - 1
          ),
          tmp
        )
        tmp <- gsub(
          glue::glue("(FILE\\s*=\\s*[[:alpha:]]+){runno}"),
          sprintf(
            ifelse(
              startNumber + nrow(table) < 1000,
              "\\1%03d",
              "\\1%04d"
            ),
            startNumber + iEffect - 1
          ),
          tmp
        )
      }

      # Replace @COVARIATE tag
      tmp <- sub("@COVARIATE", table$Covariate[iEffect], tmp)

      # Replace @PARAMETER tag
      tmp <- sub("@PARAMETER", parm, tmp)

      # Replace @FUNCTION tag
      tmp <- sub(
        "@FUNCTION",
        paste(
          ifelse( table$Function[iEffect] %in% c("Additive", "Exponential"), "an", "a"),
          tolower(table$Function[iEffect]),
          "effect"
        ),
        tmp
      )


      # Find line of code defining the covariate THETA to be turned off
      index <- grepl(table$pattern[iEffect], tmp)

      if ( length(index) == 0 ){
        tmp <- "Covariate definition was not found!"
      } else {
        tmp[index] <- sub(
          pattern = glue::glue(".*(;--th[0-9]+-.*{table$pattern[iEffect]})"),
          replacement = glue::glue("  {table$Initial[iEffect]} FIXED \\1"),
          tmp[index]
        )
      }

      # Finalize
      univariates[[iEffect]] <- paste(tmp, collapse = "\n")
      names(univariates)[iEffect] <- ifelse(
        style == "Standard",
        paste0(table$name[iEffect], ".ctl"),
        sprintf(
          ifelse(
            startNumber + nrow(table) < 1000,
            "run%03d.mod",
            "run%04d.mod"
          ),
          startNumber + iEffect - 1
        )
      )

    }

  }

  return(univariates)

}
