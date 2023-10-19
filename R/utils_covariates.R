
#' Get NONMEM code blocks
#'
#' Process a character string containing NONMEM model code and returns a
#' character vector in which each element contain a particular $ block
#'
#' @param code A character string containing the control stream
#'

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


convert_reference_code <- function(code){

  if (code == ""){
    return("")
  } else {

    new_code <- get_nonmem_blocks( code )

    # Find and process $PRED or $PK
    predpk_index <- which( grepl("^[$]PRED|^[$]PK", new_code) )
    if ( length(predpk_index) == 0 ){
      return(
        new_code_object <- list(
          code = new_code,
          tvs = NULL,
          warnings = list(
            'No $PK or $PRED block present in NONMEM control stream'
          )
        )
      )
    }
    if ( length(predpk_index) > 1 ){
      return(
        new_code_object <- list(
          code = new_code,
          tvs = NULL,
          warnings = list(
            'More than 1 $PK or $PRED block present in NONMEM control stream'
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

    # Find indexes of all lines starting by TV*
    tvs_index <- which( grepl("^\\s*TV[A-Z0-9]+\\s*=", predpk_code) )

    # Find all TV* variables
    tvs <- gsub( "^(\\s*)(TV[A-Z0-9]+)(.*)", "\\2", predpk_code[tvs_index] )

    # Extract the definition of all TV* variables
    tvs_def <- gsub("^(\\s*TV[A-Z0-9]+\\s*=\\s*)(.*)", "\\2", predpk_code[tvs_index])

    # Implements TVXI/TVX parameterization
    predpk_code[tvs_index] <- gsub(
      "^(\\s*)(TV[A-Z0-9]+)(.*)",
      "\\1\\2I\\3 @@@\\1\\2 = \\2I",
      predpk_code[tvs_index]
    )

    # Add covariate start and end tags
    if ( length(tvs_index) > 0 ){
      if ( tvs_index[1] == 2 ){
        predpk_code[1] <- paste0(
          predpk_code[1],
          " @@@;-- COVARIATE EFFECT START @@@",
          " @@@;-- COVARIATE EFFECT END @@@"
        )
      } else {
        continue <- TRUE
        index <- tvs_index[1]
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
    }

    # Collapse $PRED or $PK
    predpk_code <- paste(predpk_code, collapse = " @@@")

    # Replace $PRED or $PK
    new_code[predpk_index] <- predpk_code

    # Final collapsing and replacement of @@@ tags by \n
    new_code <- paste(new_code, collapse = " @@@")
    new_code <- gsub(" @@@", "\n", new_code)

    # Create output object
    new_code_object <- list(
      code = new_code,
      tvs = tvs,
      warnings = list(
        no_tvs = (length(tvs) == 0),
        multiple_tvs = (length(tvs) != length(unique(tvs))),
        complex_tvs = !all(gsub("[(][0-9]+[)]", "", tvs_def) == "THETA")
      )
    )

    return(new_code_object)

  }
}

#' Check covariate definition table
#'
#' @param table a data.frame with expected columns
#' @param check_step a logical indicator to check content of Step column or not


check_covariate_table <- function(
  table = NULL,
  check_step = FALSE
){

  if ( length(table) == 0 ){
    return( table )
  }

  # Stage
  if (
    !(
      ( inherits(table$Stage, "character") | inherits(table$Stage, "factor") ) &
      all( is.na(table$Stage) | table$Stage %in% c("Forward", "Backward") )
    )
  ){
    stop("Invalid content in Stage column")
  }

  # Step
  if (
    isTRUE(check_step) &&
    (
      ( inherits(table$Step, "character") | inherits(table$Step, "factor") ) &
      !all( is.na(table$Step) | grepl("^[FB][0-9]+$", table$Step)  )
    )
  ) {
    stop("Invalid content in Step column")
  }

  # Parameter
  if (
    !(
      ( inherits(table$Parameter, "character") | inherits(table$Parameter, "factor") ) &
      all( is.na(table$Parameter) | grepl("^[[:alnum:]]+$", table$Parameter)  )
    )
  ) {
    stop("Invalid content in Parameter column")
  }

  # Covariate
  if (
    !(
      ( inherits(table$Covariate, "character") | inherits(table$Covariate, "factor") ) &
      all( is.na(table$Covariate) | grepl("^[[:alnum:]]+$", table$Covariate)  )
    )
  ){
    stop("Invalid content in Covariate column")
  }

  # Type
  if (
    !(
      ( inherits(table$Type, "character") | inherits(table$Type, "factor") ) &
      all( is.na(table$Type) | table$Type %in% c("Continuous", "Discrete") )
    )
  ){
    stop("Invalid content in Type column")
  }

  # Function
  if (
    !(
      ( inherits(table$Function, "character") | inherits(table$Function, "factor") ) &
      all( is.na(table$Function) | table$Function %in% c("Linear", "Power", "Exponential", "Additive", "Proportional", "Direct proportional") )
    )
  ) {
    stop("Invalid content in Function column")
  }

  continuous <- table %>%
    dplyr::filter( Type == "Continuous" )
  discrete  <- table %>%
    dplyr::filter( Type != "Continuous" )
  if (
    any( continuous$Function %in% c("Additive", "Proportional", "Direct proportional") ) |
    any( discrete$Function %in% c("Linear", "Power") )
  ){
    stop("Invalid functional form for selected covariate type")
  }

  # Center
  if ( !is.numeric(table$Center) ){
    stop("Invalid content in Center column")
  }

  # Flags
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

  nFlags <- sapply(
    table$Flags,
    function(x) length( unlist(strsplit(x, ":")) )
  )

  # Initial
  if (
    !(
      ( inherits(table$Initial, "character") | inherits(table$Initial, "factor") ) &
      all( is.na(table$Initial) | grepl("[0-9:.]+$", table$Initial)  )
    )
  ){
    stop("Invalid content in Initial column")
  }

  if ( any( grepl("^:|:$", table$Initial) ) ) {
    stop("Invalid content in Initial column")
  }

  nInitials <- sapply(
    table$Initial,
    function(x) length( unlist(strsplit(x, ":")) )
  )

  if ( !all(nFlags == nInitials) ) {
    stop("Inconsistent number of elements in Flags and Initial columns")
  }

  # Action
  if (
    !(
      ( inherits(table$Action, "character") | inherits(table$Action, "factor") ) &
      all( is.na(table$Action) | table$Action %in% c("Create", "Do not create", "Select", "Remove") )
    )
  ) {
    stop("Invalid content in Action column")
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
      Parameter = as.character( ifelse( Parameter == "", NA, Parameter ) ),
      Covariate = as.character( ifelse( Covariate == "", NA, Covariate ) ),
      Type = as.character( ifelse( Type == "", NA, Type ) ),
      Function = as.character( ifelse( Function == "", NA, Function ) ),
      Center = as.numeric( ifelse( Center == "", NA_real_, Center ) ),
      Flags = as.character( ifelse( Flags == "", NA, Flags ) ),
      Initial = as.character( ifelse( Initial == "", NA, Initial ) ),
      Action = as.character( ifelse( Action == "", NA, Action ) )
    )

}


#' Generate univariate model code
#'
#' @param code A character string containing the code of the reference NONMEM model
#' @param referenceName The name of the reference model file
#' @param nThetas The number of THETA parameters in the reference NONMEM model
#' @param table The table of covariate relationship definition
#' @param style Either PsN or standard
#' @param prefix If standard style, the prefix to start the name of the univariable
#' model file with
#' @param startNumber If PsN style, the number to start univariate model run files
#' @param path The directory in which the univariate models will be saved and run

create_univariate_models <- function(
    code,
    referenceName,
    nThetas,
    table,
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
    )

  # Add name and info variables in table ----
  table <- table %>%
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
        insertLines <- c(
          ";; Covariate selection:",
          ";;    Forward step 1: effect of @COVARIATE on @PARAMETER as a @FUNCTION relationship"
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
        insertLines <- ";;    Forward step 1: effect of @COVARIATE on @PARAMETER as a @FUNCTION relationship"
      }
    } else {
      insertAfter <- which(
        grepl(glue::glue("^;;    Forward step {step-1}"), code)
      )[1]
      insertLines <- glue::glue(
        ";;    Forward step {step}: effect of @COVARIATE on @PARAMETER as a @FUNCTION relationship"
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
      tmp <- sub("@PARAMETER", table$Parameter[iEffect], tmp)

      # Replace @FUNCTION tag
      tmp <- sub("@FUNCTION", paste(tolower(table$Function[iEffect]), "effect"), tmp)

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
              key = table$Parameter[iEffect],
              label = dplyr::case_when(
                table$Function[iEffect] == "Linear" ~ glue::glue(
                  "Slope of {effect} effect on {table$Parameter[iEffect]}",
                  effect = ifelse(
                    is.na(table$Center[iEffect]),
                    covariate,
                    glue::glue(
                      "{covariate}{sign}{abs(table$Center[iEffect])}",
                      sign = ifelse( table$Center[iEffect] < 0, "+", "-")
                    )
                  )
                ),
                table$Function[iEffect] == "Power" ~ glue::glue(
                  "Exponent of {effect} effect on {table$Parameter[iEffect]}",
                  effect = ifelse(
                    is.na(table$Center[iEffect]),
                    covariate,
                    glue::glue(
                      "{sign}{covariate}/{abs(table$Center[iEffect])}",
                      sign = ifelse(table$Center[iEffect] < 0, "-", "")
                    )
                  )
                ),
                table$Function[iEffect] == "Exponential" & table$Type[iEffect] == "Continuous" ~ glue::glue(
                  "Slope of {effect} effect on log({table$Parameter[iEffect]})",
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
                  "Additive shift in log({table$Parameter[iEffect]}) for {covariate} = 1",
                ),
                table$Function[iEffect] == "Additive" ~ glue::glue(
                  "Additive shift in {table$Parameter[iEffect]} for {covariate} = 1",
                ),
                table$Function[iEffect] == "Proportional" ~ glue::glue(
                  "Proportional shift in {table$Parameter[iEffect]} for {covariate} = 1",
                ),
                table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                  "Fold-change in {table$Parameter[iEffect]} for {covariate} = 1",
                )
              )
            )
          )
        )
      }

      tmp <- sub("@THETACOV", paste(tag_replacement, collapse = "\n"), tmp)

      # Replace @CODECOV tag
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
              table$Function[iEffect] == "Linear" ~ glue::glue(
                "THETA({iTheta + nThetas})*({covariate}{sign}{abs(table$Center[iEffect])})",
                sign = ifelse( table$Covariate[iEffect] < 0, "+", "-")
              ),
              table$Function[iEffect] == "Exponential" ~ glue::glue(
                "EXP(THETA({iTheta + nThetas})*({covariate}{sign}{abs(table$Center[iEffect])}))",
                sign = ifelse( table$Covariate[iEffect] < 0, "+", "-")
              ),
              table$Function[iEffect] == "Power" ~ glue::glue(
                "({sign}{covariate}/{abs(table$Center[iEffect])})**THETA({iTheta + nThetas})",
                sign = ifelse( table$Covariate[iEffect] < 0, "-", "")
              )
            ),
            # Discrete effects
            dplyr::case_when(
              table$Function[iEffect] == "Additive" ~ glue::glue(
                "THETA({iTheta + nThetas})*{covariate}"
              ),
              table$Function[iEffect] == "Proportional" ~ glue::glue(
                "1 + THETA({iTheta + nThetas})*{covariate}"
              ),
              table$Function[iEffect] == "Direct proportional" ~ glue::glue(
                "(THETA({iTheta + nThetas})**{covariate}"
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
            ifelse(
              table$Function[iEffect] == "Direct proportional",
              glue::glue( " * THETA({iTheta + nThetas})**{covariate}" ),
              glue::glue( " + THETA({iTheta + nThetas})*{covariate}" )
            )
          )
        }

        if ( iTheta == length(initials) & table$Type[iEffect] == "Discrete" &
             table$Function[iEffect] == "Exponential"){
          tag_replacement <- paste0(tag_replacement, ")")
        }

      }

      tmp <- sub("@CODECOV", sprintf("  COV%s = %s", step, tag_replacement), tmp)
      rm(tag_replacement)

      # Insert covariate variable COVn on the line defining TVx (for additive
      # or linear effect) or TVxI (for other effects)
      if ( table$Function[iEffect] %in% c("Additive", "Linear")){
        index <- which( grepl( glue::glue("^\\s*TV{table$Parameter[iEffect]}\\s*="), tmp ) )
        if ( length(index) == 0 ){
          tmp <- glue::glue(
            paste(
              "TV{table$Parameter[iEffect]} was not present in reference model code",
              "",
              "Covariate effect definition:",
              "{table$info[iEffect]}",
              sep = "\n"
            )
          )
        } else {
          tmp[index] <- paste0( tmp[index], glue::glue(" + COV{step}") )
        }
      } else {
        index <- which( grepl( sprintf("^\\s*TV%sI\\s*=", table$Parameter[iEffect]), tmp ) )
        if ( length(index) == 0 ){
          tmp <- glue::glue(
            paste(
              "TV{table$Parameter[iEffect]}I was not present in reference model code",
              "",
              "Covariate effect definition:",
              "{table$info[iEffect]}",
              sep = "\n"
            )
          )
        } else {
          tmp[index] <- paste0( tmp[index], glue::glue(" * COV{step}") )
        }
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
          ";;    Backward step 1: removal of effect of @COVARIATE on @PARAMETER as a @FUNCTION relationship"
        )
      } else {
        insertAfter <- which(grepl("^;; 5. Covariate model:", code))[1]
        insertLines <- ";;    Backward step 1: removal of effect of @COVARIATE on @PARAMETER as a @FUNCTION relationship"
      }
    } else {
      insertAfter <- which(
        grepl(glue::glue("^;;    Backward step {step-1}"), code)
      )[1]
      insertLines <- glue::glue(
        ";;    Backward step {step}: removal of effect of @COVARIATE on @PARAMETER as a @FUNCTION relationship"
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
      tmp <- sub("@PARAMETER", table$Parameter[iEffect], tmp)

      # Replace @FUNCTION tag
      tmp <- sub("@FUNCTION", paste(tolower(table$Function[iEffect]), "effect"), tmp)


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
