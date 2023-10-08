#' New model module
#' @name covariates_server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param resources A list of internal resources
#'

covariates_server <- function(session, input, output, resources ){

  # Reference model ----

  # * Upload ----

  # Button to import reference NONMEM file + backend
  output$referenceFileUI <- renderUI({

    shinyFiles::shinyFilesButton(
      id = "referenceFileChoose",
      title = "Select reference model",
      label = "Select reference model",
      multiple = FALSE,
      style = "margin-bottom: 10px;",
      icon = icon("download")
    )

  })

  shinyFiles::shinyFileChoose(
    input,
    "referenceFileChoose",
    defaultPath = "/",
    roots = c(root = "/"),
    filetypes = c("ctl", "mod")
  )

  referenceFileReactive <- reactive({
    req( input$referenceFileChoose, "files" %in% names(input$referenceFileChoose))

    normalizePath(
      shinyFiles::parseFilePaths(c(root = "/"), input$referenceFileChoose)$datapath
    )
  })

  # * Import reference model ----
  referenceCode <- reactive({
    if ( file_exists(referenceFileReactive()) ){
      code <- scan(
        referenceFileReactive(),
        what = "character",
        n = -1,
        sep = "\n",
        blank.lines.skip = FALSE,
        quiet = TRUE
      )
      if (length(code) == 0){
        code <- ""
      } else {
        code <- paste(code, collapse = "\n")
      }
      return(code)
    }
  })

  # * ACE editor for reference model ----
  output$aceReferenceUI <- renderUI({

    if ( !file_exists(referenceFileReactive()) ) {
      return( NULL )
    }

    shinyAce::aceEditor(
      outputId = "aceReference",
      value = referenceCode(),
      mode = "nonmem",
      theme = "crimson_editor",
      height = "646px",
      fontSize = 14,
      wordWrap = TRUE
    )

  })

  # * Detect reference model style ----
  referenceFileStyle <- reactive({
    if ( file_exists(referenceFileReactive()) ){
      ifelse( tools::file_ext(referenceFileReactive()) == 'mod', 'PsN', 'Standard')
    }
  })

  # observeEvent(
  #   referenceFileReactive(),
  #   {
  #     if ( !is.null(input$aceReference) && input$aceReference == "" ){
  #       browser()
  #       shinyAce::updateAceEditor(
  #         session = session,
  #         editorId = "aceReference",
  #         value = referenceCode(),
  #         mode = "nonmem"
  #       )
  #     } else {
  #       # Do nothing
  #     }
  #   }
  # )

  # * Check reference model ----
  referenceValid <- reactive({

    req( input$aceReference, referenceFileStyle() )

    check <- TRUE

    code <- unlist( strsplit( input$aceReference, split = '\n') )

    # Header must contain at least 2 rows of separator hyphens
    headerCheck <- sum(which(grepl('^;;--[-]+', code))) >= 2

    # Header must contain a row starting with ;; Name:
    if ( headerCheck ){
      headerCheck <- any(grepl('^;; Name:', code))
    }

    # Header must contain a row starting with ;; Created on
    if ( headerCheck ){
      headerCheck <- any(grepl('^;; Created on', code))
    }

    # If standard style, header must contain a row starting with ;; PRUPOSE:
    if ( headerCheck & referenceFileStyle() == 'Standard' ){
      headerCheck <- any(grepl('^;; PURPOSE:', code))
    }

    # If PsN style, header must contain rows starting with
    # ;; 1. Based on
    # ;; 2. Description
    # ;; 5. Covariate model
    if ( headerCheck & referenceFileStyle() == 'PsN' ){
      headerCheck <- any(grepl('^;; 1. Based on', code))
    }
    if ( headerCheck & referenceFileStyle() == 'PsN' ){
      headerCheck <- any(grepl('^;; 6. Interindividual variability', code))
    }
    if ( headerCheck & referenceFileStyle() == 'PsN' ){
      headerCheck <- any(grepl('^;; 5. Covariate model', code))
    }

    if ( !headerCheck ){
      return('Invalid or missing descriptive header section (see Library for examples)')
    }

    if (
      !any( grepl(glue::glue(';; Name: {referenceFileReactive()}'), code) )
    ){
      return('Missing or inconsistent path and filename in header (see Library for examples)')
    }

    # Check expected NONMEM blocks
    code <- get_nonmem_blocks( input$aceReference )

    if ( !any( grepl("^[$]PRO", code) ) ){
      return( 'No $PROBELM block present in NONMEM control stream' )
    }

    predpk_index <- which( grepl("^[$]PRED|^[$]PK", code) )
    if ( length(predpk_index) == 0 ){
      return( 'No $PK or $PRED block present in NONMEM control stream' )
    }
    if ( length(predpk_index) > 1 ){
      return( 'More than 1 $PK or $PRED block present in NONMEM control stream' )
    }
    if ( !any( grepl("^[$]THE", code) ) ){
      return( 'No $THETA block present in NONMEM control stream' )
    }

    check

  })


  # * UI for conversion button ----
  output$convertBtnUI <- renderUI({

    if ( !file_exists(referenceFileReactive()) ) {
      return( NULL )
    }

    if ( !isTRUE(referenceValid()) ){
      return( NULL )
    }

    actionButton(
      inputId = "convertBtn",
      label = "Convert",
      icon = icon("edit")
    )

  })

  # * Convert reference control stream ----
  conversionObject <- eventReactive(
    input$convertBtn,
    {
      convert_reference_code( input$aceReference )
    }
  )
  convertedCode <- eventReactive(
    input$convertBtn,
    {
      req( conversionObject() )
      conversionObject()$code
    }
  )

  # * Reference info / error / warnings ----
  nThetas <- reactive({
    req( input$aceReference )
    get_theta_number( input$aceReference )
  })

  output$nthetaBox <- renderUI({
    req( nThetas() )
    bslib::value_box(
      title = NULL,
      theme_color = "info",
      value = paste(nThetas(), "THETAs"),
      showcase = bsicons::bs_icon("info-circle", size = "0.25em"),
      showcase_layout = bslib::showcase_left_center( width = 0.3 ),
      fill = TRUE,
      height = "46px",
    )
  })

  output$referenceBoxes <- renderUI({

    if ( !isTRUE(referenceValid()) ){
      return(
        danger_box(
          value = referenceValid(),
          width_left = 0.095
        )
      )
    }

    req( conversionObject() )

    if ( conversionObject()$warnings[[1]] ){
      return(
        danger_box(
          value = "No TV parameter was defined",
          width_left = 0.095
        )
      )
    }

    if ( conversionObject()$warnings[[2]] ){
      return(
        bslib::value_box(
          title = NULL,
          theme_color = "warning",
          value = "The same TV parameter was defined multiple times",
          showcase = bsicons::bs_icon("exclamation-octagon", size = "0.25em"),
          showcase_layout = bslib::showcase_left_center( width = 0.095 ),
          fill = TRUE,
          height = "48px",
          width = c("2.5%", "97.5%")
        )
      )
    }

  })


  output$warning_complex_tvs <- eventReactive(
    input$aceConverted,
    {
      if ( input$aceConverted %in% c("", " ") ){
        FALSE
      } else {
        conversionObject()$warnings[[3]]
      }
    }
  )

  outputOptions(output, "warning_complex_tvs", suspendWhenHidden = FALSE)

  # * ACE editor for converted control stream ----
  output$aceConvertShown <- renderUI({
    isTruthy( conversionObject() )
  })

  output$aceConvertUI <- renderUI({

    req( conversionObject() )

    shinyAce::aceEditor(
      outputId = "aceConverted",
      value = convertedCode(),
      mode = "nonmem",
      theme = "crimson_editor",
      height = "646px",
      fontSize = 14,
      wordWrap = TRUE
    )

  })

  # * Conversion info / error / warnings ----
  output$convertedBoxes <- renderUI({

    req( conversionObject() )

    tmp <- NULL

    if ( conversionObject()$warnings[[1]] | conversionObject()$warnings[[2]] ){
      tmp <- danger_box(
        value = "Invalid model",
        width_left = 0.3*0.25
      )
    }

    if ( !conversionObject()$warnings[[1]] & !conversionObject()$warnings[[2]] & !conversionObject()$warnings[[3]] ){
      tmp <- bslib::value_box(
        title = NULL,
        theme_color = "success",
        value = "Transformation complete",
        showcase = bsicons::bs_icon("check-lg", size = "0.25em"),
        showcase_layout = bslib::showcase_left_center( width = 0.3*0.25 ),
        fill = TRUE,
        height = "46px"
      )
    }

    if ( conversionObject()$warnings[[3]] ){
      tmp <- bslib::value_box(
        title = NULL,
        theme_color = "info",
        value = "Complex TV parameter definition was used - Code check required",
        showcase = bsicons::bs_icon("check-lg", size = "0.25em"),
        showcase_layout = bslib::showcase_left_center( width = 0.3*0.25 ),
        fill = TRUE,
        height = "46px"
      )
    }

    tmp

  })

  # Parameter / covariate relationships ----

  # * Create table toolbar ----
  output$covariateToolboxUI <- renderUI({
    fluidRow(
      col_4(
        shinyFiles::shinyFilesButton(
          id = "covariateLoadChoose",
          title = "Import covariate",
          label = NULL,
          icon = icon("download"),
          multiple = FALSE,
          width = "39px"
        ),
        downloadButton(
          outputId = "covariateSaveBtn",
          label = NULL,
          icon = icon("upload"),
          width = "39px",
          style = " margin-right: 10px;"
        ),
        actionButton(
          inputId = "covariateAddBtn",
          label = NULL,
          icon = icon("plus"),
          width = "39px"
        ),
        actionButton(
          inputId = "covariateDeleteBtn",
          label = NULL,
          icon = icon("minus"),
          width = "39px",
          style = "margin-right: 10px"
        ),
        actionButton(
          inputId = "covariateCopyBtn",
          label = NULL,
          icon = icon("copy"),
          width = "39px"
        ),
        actionButton(
          inputId = "covariatePasteBtn",
          label = NULL,
          icon = icon("paste"),
          width = "39px"
        ),
        actionButton(
          inputId = "covariateMoveBtn",
          label = NULL,
          # icon = icon("stamp"),
          width = "39px",
          icon = NULL,
          img(
            src = "www/duplicate.svg",
            padding = '3px'
          ),
          style = "margin-right: 10px;"
        ),
        actionButton(
          inputId = "covariateHelpBtn",
          label = NULL,
          icon = icon("question"),
          width = "39px"
        )
      ),
      col_3(
        fluidRow(
          col_7(
            selectizeInput(
              inputId = "fsbeInput",
              label = NULL,
              choices = c("Forward selection", "Backward elimination"),
              selected = "Forward selection"
            )
          ),
          col_5(
            div(
              id = "inlineStep",
              class = "inline",
              numericInput(
                inputId = "stepInput",
                label = "Step",
                value = 1,
                min = 1,
                max = 100,
                step = 1,
              )
            )
          )
        )
      )
    )
  })

  # Covariate file button backend
  shinyFiles::shinyFileChoose(
    input,
    "covariateLoadChoose",
    defaultPath = "/",
    roots = c(root = "/"),
    filetypes = c("csv")
  )

  covariateLoadReactive <- reactive({
    req( input$covariateLoadChoose, "files" %in% names(input$covariateLoadChoose))

    normalizePath(
      shinyFiles::parseFilePaths(c(root = "/"), input$covariateLoadChoose)$datapath
    )
  })

  # Current stage and step
  currentStage <- reactiveVal("Forward")
  currentStep <- reactiveVal(1)

  # Help modal
  observeEvent(
    input$covariateHelpBtn,
    {
      showModal(
        modalDialog(
          title = "Instructions",
          includeHTML('/home/sebastien/git/pmxcode/inst/resources/covariate_help.html'),
          easyClose = TRUE,
          size = "xl"
        )
      )
    }
  )

  # * Extract and check uploaded covariate definition file ----
  covariateFile <- reactive({

    req( covariateLoadReactive() )

    tryCatch(
      expr = {
        data <- readr::read_csv(
          file = covariateLoadReactive(),
          show_col_types = FALSE
        )

        ref <- c(
          "Stage", "Step", "Parameter", "Covariate", "Type", "Function", "Center",
          "Flags", "Initial", "Action"
        )

        if ( ncol(data) != length(ref) ){
          stop("Invalid number of headers")
        }

        if ( !all(names(data) == ref) ){
          stop("Invalid column headers")
        }

        check_covariate_table(table = data, check_step = TRUE)

      },
      error = function(e){
        structure(e$message, class = "try-error")
      },
      warning = function(e){
        structure(e$message, class = "try-error")
      },
      finally = {}
    )
  })

  isCovariateFileValid <- reactive({

    if ( isTruthy(input$covariateLoadChoose) ){
      res <- !inherits(covariateFile(), "try-error")
    } else {
      res <- TRUE
    }

    res

  })

  output$covariateFileUI <- renderUI({

    req( covariateLoadReactive() )

    if ( isCovariateFileValid() ){
      return(NULL)
    }

    danger_box(
      value = span(
        glue::glue("Covariate effect definition file: {covariateLoadReactive()}"),
        br(),
        glue::glue("Error: {covariateFile()}")
      ),
      width_left = 0.05
    )

  })

  covariateData <- reactiveVal(
    data.frame(
      Stage = rep("Forward", 20),
      Step = rep(1, 20),
      Parameter = rep(NA_character_, 20),
      Covariate = rep(NA_character_, 20),
      Type = rep(NA_character_, 20),
      Function = rep(NA_character_, 20),
      Center = rep(NA_real_, 20),
      Flags = rep(NA_character_, 20),
      Initial = rep(NA_character_, 20),
      Action = rep(NA_character_, 20),
      stringsAsFactors = FALSE
    )
  )
  covariateCopyData <- reactiveVal(NULL)
  covariateInvalidData <- reactiveVal('Valid')

  # * Update covariate data based upon reactivity ----
  # Deselect of handsontable
  observeEvent(
    input$covariateTableDeselect,
    {

      DF <- hot_to_r_raw( input$covariateTable )

      covariateInvalidData( check_incomplete_covariate_table(DF) )

      # Save current content of handsontable (if necessary)
      if (
        !identical(
          DF,
          covariateData() %>%
          dplyr::filter( Stage ==  currentStage() & Step %in% currentStep() )
        )
      ){
        covariateData(
          bind_rows(
            # Remove data for current and next step from covariate data
            covariateData() %>%
              dplyr::filter( !(Stage ==  currentStage() & Step %in% currentStep() ) ),
            # Save current step data
            DF
          ) %>%
            dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
        )
      }
    }
  )

  # Data upload
  observeEvent(
    input$covariateLoadChoose,
    {
      req( covariateFile() )
      covariateData(
        covariateFile() %>%
          dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
      )
    },
    ignoreInit = TRUE
  )

  # Data download
  output$covariateSaveBtn <- downloadHandler(
    filename = function() {
      paste("covariate-definition-", Sys.Date(), ".csv", sep = "")
    },
    content = function(downloadFile) {
      write.csv(
        covariateData() %>%
          dplyr::filter(
            !(
              is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
                is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
            )
          ) %>%
          dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function ),
        downloadFile,
        row.names = FALSE,
        na = ""
      )
    }
  )

  # Change in stage
  observeEvent(
    input$fsbeInput,
    {
      stage <- sub("(^\\w+)\\s.+", "\\1", input$fsbeInput)
      if ( currentStage() != stage ) {
        # Save content of table for previous step
        data <- covariateData()
        DF <- hot_to_r_raw( input$covariateTable )

        covariateData(
          bind_rows(
            # Remove data for current step from covariate data
            data  %>% dplyr::filter( !(Stage ==  currentStage() & Step == currentStep() ) ),
            # Add data for current step from rhandsontable
            DF
          ) %>%
            dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
        )
        currentStage(stage)
      }
    }
  )

  # Change in step
  isStepValid <- reactive({
    req( "stepInput" %in% names(input) )
    is.numeric(input$stepInput) && input$stepInput > 0
  })

  observeEvent(
    input$stepInput,
    {
      if ( currentStep() != input$stepInput) {
        # Save content of table for previous step
        stage <- sub("(^\\w+)\\s.+", "\\1", input$fsbeInput)
        data <- covariateData()
        DF <- hot_to_r_raw( input$covariateTable )

        covariateData(
          bind_rows(
            # Remove data for current step from covariate data
            data  %>% dplyr::filter( !(Stage ==  currentStage() & Step == currentStep() ) ),
            # Add data for current step from rhandsontable
            DF
          ) %>%
            dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
        )
        currentStep(input$stepInput)
      }
    }
  )

  # Add row
  observeEvent(
    input$covariateAddBtn,
    {
      data <- covariateData()
      DF <- hot_to_r_raw( input$covariateTable )

      covariateData(
        bind_rows(
          # Remove data for current step from covariate data
          data  %>% dplyr::filter( !(Stage ==  currentStage() & Step == currentStep() ) ),
          # Add data for current step from rhandsontable with added empty row
          bind_rows(
            DF,
            data.frame(
              Stage = currentStage(),
              Step = currentStep(),
              Parameter = NA_character_,
              Covariate = NA_character_,
              Type = NA_character_,
              Function = NA_character_,
              Center = NA_real_,
              Flags = NA_character_,
              Initial = NA_character_,
              Action = NA_character_,
              stringsAsFactors = FALSE
            )
          )
        ) %>%
          dplyr::arrange( desc(Stage), Step )
      )
    }
  )

  # Delete current row
  observeEvent(
    input$covariateDeleteBtn,
    {
      # Ensure that handsontable was selected at least once
      req( input$covariateTable_select$select$r )

      data <- covariateData()
      DF <- hot_to_r_raw( input$covariateTable )

      # Ensure that handsontable was selected again by checking that dimensions in callback were updated
      req( dim(DF) == input$covariateTable_select$params$rDataDim[[1]] )

      if ( nrow(DF) > 0 ){
        DF <- DF[-seq(input$covariateTable_select$select$r, input$covariateTable_select$select$r2), ]
      }

      if ( nrow(DF) == 0) {
        DF <- data.frame(
          Stage = currentStage(),
          Step = currentStep(),
          Parameter = NA_character_,
          Covariate = NA_character_,
          Type = NA_character_,
          Function = NA_character_,
          Center = NA_real_,
          Flags = NA_character_,
          Initial = NA_character_,
          Action = NA_character_,
          stringsAsFactors = FALSE
        )
      }

      # Update data
      covariateData(
        bind_rows(
          # Remove data for current step from covariate data
          data  %>% dplyr::filter( !(Stage ==  currentStage() & Step == currentStep() ) ),
          # Add data for current step from rhandsontable
          DF
        ) %>%
          dplyr::arrange( desc(Stage), Step )
      )
    }
  )

  # Copy content of current step
  observeEvent(
    input$covariateCopyBtn,
    {
      covariateCopyData( hot_to_r_raw( input$covariateTable ) )
    }
  )

  # Paste content of copied step
  observeEvent(
    input$covariatePasteBtn,
    {

      req( covariateCopyData() )
      req( nrow(covariateCopyData()) > 0 )
      req( !all(is.na(covariateCopyData())) )
      req( !all(is.na(covariateCopyData() %>% dplyr::select(-Stage, -Step))) )

      DF <- hot_to_r_raw( input$covariateTable )

      # Display a modal if the current handsontable is not empty
      if ( !all(is.na(DF %>% dplyr::select(-Stage, -Step))) ){
        showModal(
          modalDialog(
            title = "Overwrite current content?",
            glue::glue(
              paste(
                "Do you want to overwrite the current definition of",
                "{tolower(input$fsbeInput)}",
                "step {input$stepInput}?"
              )
            ),
            footer = tagList(
              actionButton(inputId = "yesCopy", label = "Yes"),
              modalButton("No")
            )
          )
        )
      } else {
        covariateData(
          bind_rows(
            # Remove data for current step from covariate data
            covariateData()  %>% dplyr::filter( !(Stage ==  currentStage() & Step == currentStep() ) ),
            # Add copied data and update stage and step
            covariateCopyData() %>%
              dplyr::mutate(
                Stage = currentStage(),
                Step = currentStep()
              )
          ) %>%
            dplyr::filter(
              !(
                is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
                  is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
              )
            ) %>%
            dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
        )
      }
    }
  )

  observeEvent(
    input$yesCopy,
    {
      covariateData(
        bind_rows(
          # Remove data for current step from covariate data
          covariateData() %>% dplyr::filter( !(Stage ==  currentStage() & Step == currentStep() ) ),
          # Add copied data and update stage and step
          covariateCopyData() %>%
            dplyr::mutate(
              Stage = currentStage(),
              Step = currentStep()
            )
        ) %>%
          dplyr::filter(
            !(
              is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
                is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
            )
          ) %>%
          dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
      )
      removeModal()
    }
  )

  # Create new stage
  observeEvent(
    input$covariateMoveBtn,
    {

      DF <- hot_to_r_raw( input$covariateTable )

      req( !all(is.na(DF %>% dplyr::select(-Stage, -Step))) )

      # Identify and filter selected covariate effect
      DF <- DF %>%
        dplyr::mutate( combo = paste(Covariate, Parameter) )
      selectCovariateEffect <- DF %>%
        dplyr::filter( Action == "Select" )
      if ( nrow(selectCovariateEffect) > 0 ){
        DF <- DF %>%
          dplyr::filter( !(combo %in% selectCovariateEffect$combo ) )
      }
      DF <- DF %>% select(-combo)

      req( nrow(DF) > 0 )
      req( !all(is.na(DF %>% dplyr::select(-Stage, -Step))) )

      # Get content of the next step
      nextDF <- covariateData() %>%
        dplyr::filter( Stage == currentStage() & Step == currentStep() + 1 )

      # Display a modal if the next handsontable is not empty
      if (
        nrow(nextDF) > 0 &
        !all(is.na(nextDF %>% dplyr::select(-Stage, -Step)))
      ){
        showModal(
          modalDialog(
            title = "Overwrite content of next step?",
            glue::glue(
              paste(
                "Covariate effects are alread defined for",
                "{tolower(input$fsbeInput)}",
                "step {input$stepInput + 1}<\br>",
                "Do you want to overwrite this information by importing the covariate effect definition of",
                "{tolower(input$fsbeInput)}",
                "step {input$stepInput}?"
              )
            ),
            footer = tagList(
              actionButton(inputId = "yesImport", label = "Yes"),
              modalButton("No")
            )
          )
        )
      } else {
        covariateData(
          bind_rows(
            # Remove data for current and next step from covariate data
            covariateData() %>%
              dplyr::filter( !(Stage ==  currentStage() & Step == currentStep()+1 ) ),
            # Save next step data
            DF %>%
              dplyr::mutate( Step = currentStep()+1 )
          ) %>%
            dplyr::filter(
              !(
                is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
                  is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
                )
            ) %>%
            dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
        )
        currentStep( currentStep() + 1)
        updateNumericInput(
          inputId = "stepInput",
          value = currentStep()
        )
      }
    }
  )

  observeEvent(
    input$yesImport,
    {

      DF <- hot_to_r_raw( input$covariateTable ) %>%
        dplyr::mutate( combo = paste(Covariate, Parameter) )
      selectCovariateEffect <- DF %>%
        dplyr::filter( Action == "Select" )
      if ( nrow(selectCovariateEffect) > 0 ){
        DF <- DF %>%
          dplyr::filter( !(combo %in% selectCovariateEffect$combo ) )
      }
      DF <- DF %>% select(-combo)

      covariateData(
        bind_rows(
          # Remove data for current and next step from covariate data
          covariateData() %>%
            dplyr::filter( !(Stage ==  currentStage() & Step == currentStep()+1 ) ),
          # Save next step data
          DF %>%
            dplyr::mutate( Step = currentStep()+1 )
        ) %>%
          dplyr::filter(
            !(
              is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
                is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
            )
          ) %>%
          dplyr::arrange( desc(Stage), Step, Parameter, Covariate, Function )
      )
      currentStep( currentStep() + 1)
      updateNumericInput(
        inputId = "stepInput",
        value = currentStep()
      )
      removeModal()
    }
  )

  # * Check step ----
  output$stepCheckUI <- renderUI({

    if ( isStepValid() ){
      return(NULL)
    }

    danger_box(
      value = "Error: Invalid step",
      width_left = 0.05
    )

  })

  # * Covariate table ----
  output$covariateTable <- rhandsontable::renderRHandsontable({

    if ( !isCovariateFileValid() | !isStepValid() ){
      return( NULL )
    }

    DF <- covariateData() %>%
      dplyr::filter(
        Stage == ifelse(input$fsbeInput == "Forward selection", "Forward", "Backward") &
          Step == input$stepInput
      )

    names(DF) <- c(
      "Stage",
      "Step",
      "Parameter",
      "Covariate",
      "Type",
      "Functional form",
      "Center value for <br> continuous covariate",
      "Dichotomous flags for <br> discrete covariate",
      "Initial estimate",
      "Action"
    )
    DF <- DF %>%
      dplyr::mutate(
        Type = factor(
          Type,
          levels = c("Continuous", "Discrete"),
          ordered = TRUE
        ),
        `Functional form` = factor(
          `Functional form`,
          levels = c("Linear", "Power", "Exponential", "Additive", "Proportional", "Direct proportional"),
          ordered = TRUE
        ),
        Action = factor(
          Action,
          levels = c("Create", "Do not create", "Select"),
          ordered = TRUE
        )
      )

    tmp <- rhandsontable::rhandsontable(
      data = DF,
      rowHeaders = TRUE,
      manualColumnMove = FALSE,
      manualRowMove = FALSE,
      colWidths = 150,
      selectCallback = TRUE,
      afterDeselect = htmlwidgets::JS(
        "function() {Shiny.onInputChange('covariateTableDeselect', Math.random())}"
      )
    )

    tmp <- tmp %>%
      rhandsontable::hot_col( col = "Stage", readOnly = TRUE, width = 1 ) %>%
      rhandsontable::hot_col( col = "Step", readOnly = TRUE, width = 0.1 ) %>%
      rhandsontable::hot_col(
        col = "Type",
        type = "dropdown",
        source = c("Continuous", "Discrete")
      ) %>%
      rhandsontable::hot_col(
        col = "Functional form",
        type = "dropdown",
        source = c("Linear", "Power", "Exponential", "Additive", "Proportional", "Direct proportional")
      ) %>%
      rhandsontable::hot_col(
        col = "Action",
        type = "dropdown",
        source = c("Create", "Do not create", "Select")
      ) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
       function(el, x) {
         var hot = this.hot;
         $('a[data-value=\"Mapping\"').on('click', function( ){
           setTimeout(function() {hot.render();}, 0);
         })
       }")

    tmp

  })

  outputOptions(output, "covariateTable", suspendWhenHidden = FALSE)

  output$covariateTableUI <- renderUI({

    rhandsontable::rHandsontableOutput("covariateTable")

  })

  # * Covariate table checks ----
  output$covariateCheckUI <- renderUI({

    req( isCovariateFileValid(), input$covariateTable )

    DF <- hot_to_r_raw( input$covariateTable )

    DF <- tryCatch(
      expr = {
        check_covariate_table( table =  DF )
      },
      error = function(e){
        structure(e$message, class = "try-error")
      },
      warning = function(e){
        structure(e$message, class = "try-error")
      },
      finally = {}
    )

    if ( !inherits(DF, "try-error") ){
      return( NULL )
    }

    danger_box(
      value = glue::glue("Error: {DF}"),
      width_left = 0.05
    )

  })

  output$covariateCheckUI2 <- renderUI({

    if ( covariateInvalidData() != "Valid" ){
      bslib::value_box(
        title = NULL,
        theme_color = "warning",
        value = covariateInvalidData(),
        showcase = bsicons::bs_icon("info-circle", size = "0.25em"),
        showcase_layout = bslib::showcase_left_center( width = 0.05 ),
        fill = TRUE,
        height = "46px",
      )
    }

  })

  # * Table box UI ----
  output$covariateBoxUI <- renderUI({
    if ( !file_exists(referenceFileReactive()) ) {
      return(
        wellPanel(
          col_3(
            danger_box(
              value = "No reference model was defined",
              width_left = 0.12
            )
          )
        )
      )
    }

    wellPanel(
      h4("Covariate effect definitions"),
      fluidRow(
        col_12( uiOutput("covariateToolboxUI") )
      ),
      fluidRow(
        col_7( uiOutput("covariateFileUI"))
      ),
      fluidRow(
        col_7(
          uiOutput("covariateCheckUI"),
          uiOutput("covariateCheckUI2")
        )
      ),
      fluidRow(
        col_7( uiOutput("stepCheckUI"))
      ),
      fluidRow(
        col_12( uiOutput("covariateTableUI") )
      )
    )

  })

  # Creation of univariate control stream ----

  # * Conditional UI elements ----
  output$univariateConditionalUI <- renderUI({

    req(referenceFileStyle)

    if ( referenceFileStyle() == "PsN" ){

      runno <- as.numeric(
        sub(
          ".*?([0-9]+)$",
          "\\1",
          sub("[.].*$", "", basename(referenceFileReactive()) )
        )
      )

      fluidRow(
        col_10(
          div(
            id = "inlineCond",
            class = "inline2",
            numericInput(
              inputId = "univariateMinRunInput",
              label = "Run nº start at",
              value = runno+1,
              min = runno+1,
              step = 1
            )
          )
        )
      )
    } else {
      div(
        id = "inlineCond",
        class = "inline2",
        textInput(
          inputId = "univariatePrefix",
          label = "FIlename prefix",
          value = ifelse(input$fsbeUnivariateInput == "Forward selection", "fs", "be")
        )
      )
    }

  })

  # * UI empty data warning  ----
  emptyData <- reactive({
    req( input$fsbeUnivariateInput, input$stepUnivariateInput )
    nrow(
      covariateData() %>%
        dplyr::filter(
          !(
            is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
              is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
          )
        ) %>%
        dplyr::filter(
          Stage == sub("(^\\w+)\\s.+", "\\1", input$fsbeUnivariateInput) &
            Step == input$stepUnivariateInput
        )
    ) == 0
  })

  output$emptyDataUI <- renderUI({
    if ( emptyData() ) {
      danger_box(
        value = "No covariate definition available for this step",
        width_left = 0.05
      )
    }
  })

  # * UI valid header warning  ----
  validStepHeader <- reactive({

    req( input$fsbeUnivariateInput, input$stepUnivariateInput )

    # Determine which code to use as reference
    # Converted code must be used if available and for forward step 1 otherwise
    # reference code must be used
    if (
      as.numeric(input$stepUnivariateInput) == 1 &
      input$fsbeUnivariateInput == "Forward selection" &
      ( length(input$aceConverted) && !input$aceConverted %in% c("", " "))
    ) {
      refCode <- input$aceConverted
    } else {
      refCode <- input$aceReference
    }

    stage <- tolower(input$fsbeUnivariateInput)

    res <- TRUE

    if ( input$stepUnivariateInput > 1 ){

      for ( iStep in 1:(input$stepUnivariateInput-1) ){

        if (
          stage == "forward selection" &
          !any( grepl(glue::glue(";;    Forward step {iStep}"), refCode) )
        ) {
          return(
            glue::glue(
              "Header section is missing information about step {iStep} of {stage}"
            )
          )
        }

        if (
          stage == "backward elimination" &
          !any( grepl(glue::glue(";;    Backward step {iStep}"), refCode) )
        ){

        }

      }
    }

    if (
      stage == "forward selection" &
      any( grepl(glue::glue(";;    Forward step {input$stepUnivariateInput}"), refCode) )
    ) {
      return(
        glue::glue(
          "Reference model already contains code for step {input$stepUnivariateInput} of {stage}"
        )
      )
    }

    return(TRUE)

  })

  output$invalidStepHeaderUI <- renderUI({

    req( validStepHeader() )

    if ( !isTRUE(validStepHeader()) ){
      danger_box(
        value = validStepHeader(),
        width_left = 0.05
      )
    }

  })

  output$univariateCreateBtnUI <- renderUI({
    if ( !emptyData() & isTRUE(validStepHeader()) ) {
      actionButton(
        inputId = "univariateCreateBtn",
        label = "Create",
        icon = icon("play")
      )
    }
  })

  output$univariateSaveBtnUI <- renderUI({
    req( input$univariateCreateBtn )
    if ( !emptyData() & isTRUE(validStepHeader()) ) {
      downloadButton(
        outputId = "univariateSaveBtn",
        label = "Export all",
        icon = icon("upload")
      )
    }
  })

  output$univariateBtnUI <- renderUI({
    fluidRow(
      col_5(
        uiOutput( "univariateCreateBtnUI" )
        ),
      col_7(
        uiOutput( "univariateSaveBtnUI" )
      )
    )
  })

  output$univariateCreateUI <- renderUI({

    if ( !file_exists(referenceFileReactive()) ) {
      return(
        wellPanel(
          col_3(
            danger_box(
              value = "No reference model was defined",
              width_left = 0.12
            )
          )
        )
      )
    }

    if ( !isTRUE( referenceValid() ) ){
      return(
        wellPanel(
          col_3(
            danger_box(
              value = "No valid reference model",
              width_left = 0.12
            )
          )
        )
      )
    }

    if ( !isTruthy( covariateData() ) ){
      return(
        wellPanel(
          col_3(
            danger_box(
              value = "No valid covariate definition",
              width_left = 0.12
            )
          )
        )
      )
    }

    tmp <- covariateData() %>%
      dplyr::filter(
        !(
          is.na(Covariate) & is.na(Parameter) & is.na(Type) & is.na(Function) &
            is.na(Center) & is.na(Flags) & is.na(Initial) & is.na(Action)
        )
      )

    if ( nrow(tmp) == 0 ){
      return(
        wellPanel(
          col_3(
            danger_box(
              value = "No valid covariate definition",
              width_left = 0.12
            )
          )
        )
      )
    }

    if (
      inherits( try( check_covariate_table( tmp ), silent = TRUE ), "try-error") |
      check_incomplete_covariate_table( tmp ) != "Valid"
    ){
      return(
        wellPanel(
          col_3(
            danger_box(
              value = "Invalid covariate definition",
              width_left = 0.12
            )
          )
        )
      )
    }

    wellPanel(
      h3("Univariate model creation"),
      fluidRow(
        col_9(
          fluidRow(
            col_4(
              shinyFiles::shinyDirButton(
                id = "univariateDirChoose",
                title = "Select model directory",
                label = "Select model directory",
                multiple = FALSE,
                style = "margin-bottom: 10px;"
              )
            ),
            col_4(
              verbatimTextOutput( "univariateDir" )
            )
          ),
          fluidRow(
            col_4(
              fluidRow(
                col_7(
                  selectizeInput(
                    inputId = "fsbeUnivariateInput",
                    label = NULL,
                    choices = c("Forward selection", "Backward elimination"),
                    selected = input$fsbeInput
                  )
                ),
                col_5(
                  div(
                    id = "inlineStep2",
                    class = "inline",
                    numericInput(
                      inputId = "stepUnivariateInput",
                      label = "Step",
                      value = input$stepInput,
                      min = 1,
                      max = 100,
                      step = 1,
                    )
                  )
                )
              )
            ),
            col_3( uiOutput( "univariateConditionalUI") ),
            col_3( uiOutput( "univariateBtnUI") )
          ),
          fluidRow(
            col_9(
              uiOutput( "invalidStepHeaderUI" ),
              uiOutput( "emptyDataUI" )
            )
          )
        ),
        col_3(
          uiOutput( "univariateInfoUI" )
        )
      ),
      fluidRow(
        col_12(
          uiOutput( "univariateCheckUI" )
        )
      )
    )

  })

  # Univariate model directory button backend
  shinyFiles::shinyDirChoose(
    input,
    "univariateDirChoose",
    roots = c(root = "/"),
    allowDirCreate = FALSE
  )

  univariateDirReactive <- reactive({
    if ( !isTruthy(input$univariateDirChoose) ){
      return(NULL)
    }
    if ( !isTruthy( "path" %in% names(input$univariateDirChoose) ) ){
      return(NULL)
    }
    normalizePath(
      shinyFiles::parseDirPath(c(root = "/"), input$univariateDirChoose)
    )
  })

  output$univariateDir <- renderText({
    req( univariateDirReactive() )
    univariateDirReactive()
  })

  # * Generate the univariate models ----
  univariateModels <- eventReactive(
    input$univariateCreateBtn,
    {

      # Determine which code to use as reference
      # Converted code must be used if available and for forward step 1 otherwise
      # reference code must be used
      if (
        as.numeric(input$stepUnivariateInput) == 1 &
        input$fsbeUnivariateInput == "Forward selection" &
        ( length(input$aceConverted) && !input$aceConverted %in% c("", " "))
      ) {
        refCode <- input$aceConverted
      } else {
        refCode <- input$aceReference
      }

      # Get stage
      stage <- sub("(^\\w+)\\s.+", "\\1", input$fsbeUnivariateInput)

      # Call utility function
      create_univariate_models(
        code = refCode,
        referenceName = referenceFileReactive(),
        nThetas = nThetas(),
        table = covariateData() %>%
          dplyr::filter(
            Stage == stage & Step == input$stepUnivariateInput
          ),
        prefix = ifelse(
          referenceFileStyle() == "PsN",
          ifelse( stage == "Forward", "fs", "be"),
          input$univariatePrefix
        ),
        style = referenceFileStyle(),
        startNumber = input$univariateMinRunInput,
        path = univariateDirReactive()
      )

    }
  )

  # * Check univariate models ----
  output$univariateInfoUI <- renderUI({
    req( univariateModels() )

    if ( !emptyData() & isTRUE(validStepHeader()) ) {
      bslib::value_box(
        title = "Univariate model created",
        theme_color = "success",
        value = length(univariateModels()),
        showcase = bsicons::bs_icon("file-earmark-text"),
        showcase_layout = bslib::showcase_left_center( width = 0.2 ),
        fill = TRUE
      )
    }

  })

  # Univariate model selector
  output$univariateSelectUI <- renderUI({
    req(univariateModels())
    selectInput(
      inputId = "univariateSelectInput",
      label = NULL,
      choices = names(univariateModels()),
      selected = names(univariateModels())[1],
      selectize = FALSE,
      size = min(10, length(univariateModels())),
    )
  })

  # Univariate model content as plain text
  output$univariateSelectedContent <- renderText ({
    req( univariateModels(), input$univariateSelectInput )
    univariateModels()[[which(names(univariateModels()) == input$univariateSelectInput)]]
  })

  # Cross univariate model checks
  output$crossChecks <- renderText ({
    req(univariateModels())
    tmp <- c()

    for (iFile in 1:length(univariateModels())){
      model <- unlist(strsplit(univariateModels()[[iFile]], "\n"))
      hit <- grep(
        glue::glue("^\\s*COV{input$stepUnivariateInput}\\s*="),
        model
      )
      tmp <- c(
        tmp,
        glue::glue(
          "\n{name}: {result}",
          name = names(univariateModels())[iFile],
          result = ifelse(
            length(hit) == 0,
            "Covariate definition was not found!",
            gsub("\n", "", model[hit])
          )
        )
      )
    }

    return( paste(tmp, collapse = "\n") )

  })


  output$univariateCheckUI <- renderUI({

    req(univariateModels())

    if ( !emptyData() & isTRUE(validStepHeader()) ) {
      bslib::navset_tab(
        bslib::nav_panel(
          title = "List of univariate models",
          fluidRow(
            col_2(
              uiOutput( "univariateSelectUI" ),
              style = "margin-top: 10px"
            ),
            col_10(
              verbatimTextOutput( "univariateSelectedContent" ),
              style = "margin-top: 10px"
            )
          )
        ),
        bslib::nav_panel(
          title = "Checks",
          col_10(
            verbatimTextOutput( "crossChecks" ),
            style = "margin-top: 10px"
          )
        )
      )
    }

  })

  # * Export univariateModels ----
  output$univariateSaveBtn <- downloadHandler(
    filename = function() {
      glue::glue(
        "univariate-{stage}-{step}-{date}.zip",
        stage = tolower(
          sub("(^\\w+)\\s.+", "\\1", input$fsbeUnivariateInput)
        ),
        step = input$stepUnivariateInput,
        date = Sys.Date()
      )
    },
    content = function(downloadFile) {
      tmpdir <- tempdir()
      fileNames <- names(univariateModels())
      files <- paste(tmpdir, fileNames, sep = "/")
      for (file in fileNames) {
        write(
          univariateModels()[[file]],
          file = glue::glue("{tmpdir}/{file}")
        )
      }
      zip(zipfile = downloadFile, files = files, flags = "-j")
    },
    contentType = "application/zip"
  )

}
