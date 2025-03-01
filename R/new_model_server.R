#' New model module
#' @name new_model_server
#'
#' @param input,output,session Internal parameters for \code{shiny}.
#' @param resources A list of internal resources
#'
#' @import rhandsontable

new_model_server <- function(session, input, output, resources ){

  # Resource expansion
  for ( i in seq_along(resources) ){
    assign(names(resources)[i], resources[[i]])
  }

  #---- Files tab ----

  output$filesUI <- renderUI({

    tagList(
      h4(strong("Model file")),
      fluidRow(
        col_4(
          shinyFiles::shinyDirButton(
            id = "modelDirChoose",
            title = "Select model directory",
            label = "Select model directory",
            multiple = FALSE,
            style = "margin-bottom: 10px;"
          )
        ),
        col_8(
          verbatimTextOutput("modelDir")
        )
      ),
      fluidRow(
        col_8(
          textInput(
            inputId = "modelInput",
            label = "Enter a model name (without extension)",
            value = ifelse(
              input$platformInput == "NONMEM" && input$nmFlavorInput != "Standard style",
              "run001",
              "mymodel"
            )
          )
        ),
        col_4(
          radioButtons(
            inputId = "modelExtensionInput",
            label = "Extension",
            choices = if ( input$platformInput == "NONMEM" ){
              c(".mod", ".ctl")
            } else if ( input$platformInput == "Berkeley Madonna" ){
              ".mdd"
            } else {
              ".cpp"
            },
            selected = if ( input$platformInput == "NONMEM" ){
              ifelse(
                input$nmFlavorInput == "Standard style",
                ".ctl",
                ".mod"
              )
            } else if ( input$platformInput == "Berkeley Madonna" ){
              ".txt"
            } else {
              ".cpp"
            },
            inline = FALSE
          )
        )
      ),
      conditionalPanel(
        condition = "input.platformInput == 'NONMEM'",
        h4(strong("Data file"))
      ),
      fluidRow(
        col_12(
          conditionalPanel(
            condition = "input.platformInput == 'NONMEM'",
            shinyFiles::shinyFilesButton(
              id = "dataFileChoose",
              title = "Select data file",
              label = "Select data file",
              multiple = FALSE,
              style = "margin-bottom: 10px;"
            )
          )
        )
      ),
      fluidRow(
        col_12(
          conditionalPanel(
            condition = "input.platformInput == 'NONMEM'",
            verbatimTextOutput("dataFile")
          )
        )
      )
    )

  })

  outputOptions(output, "filesUI", suspendWhenHidden = FALSE)

  # Model directory button backend
  shinyFiles::shinyDirChoose(
    input,
    "modelDirChoose",
    roots = c(root = "/"),
    allowDirCreate = FALSE
  )

  modelDirReactive <- reactive(input$modelDirChoose)

  output$modelDir <- renderText({
    req( modelDirReactive(), "path" %in% names(modelDirReactive()))
    normalizePath(
      shinyFiles::parseDirPath(c(root = "/"), modelDirReactive())
    )
  })

  # Data file button backend
  shinyFiles::shinyFileChoose(
    input,
    "dataFileChoose",
    defaultPath = "/",
    roots = c(root = "/"),
    filetypes = c("csv", "dat", "txt", "nmdat")
  )

  dataFileReactive <- reactive({
    req( input$dataFileChoose, "files" %in% names(input$dataFileChoose))

    normalizePath(
      shinyFiles::parseFilePaths(c(root = "/"), input$dataFileChoose)$datapath
    )
  })

  # Get top of the data file
  dataFile <- reactive({

    req( dataFileReactive())

    tryCatch(
      expr = {
        if ( tools::file_ext(dataFileReactive()) == "csv" ){
          readr::read_csv(
            file = dataFileReactive(),
            show_col_types = FALSE
          )
        } else {
          readr::read_delim(
            file = dataFileReactive(),
            delim = " ",
            show_col_types = FALSE
          )
        }
      },
      error = function(e ){
        structure('error', class = 'try-error')
      },
      warning = function(e ){
        structure('warning', class = 'try-error')
      },
      finally = {}
    )
  })

  # Data file content report
  output$dataFile <- renderText({

    req( dataFileReactive() )

    text <- glue::glue("Data file: {dataFileReactive()}")

    if ( inherits(dataFile(), "try-error") | !inherits(dataFile(), "data.frame") ){
      text <- paste0(
        text,
        "\n",
        "Error: Could not extract data file content",
        collapse = "\n"
      )
    } else {
      text <- paste(
        c(
          text,
          gsub(
            # ANSI tab character
            "\033",
            "\t",
            gsub(
              # Substitute colors returns by crayon package
              "[[]3m\033[[]38;5;246m|[[]39m\033[[]23m",
              "",
              utils::capture.output(pillar::glimpse(dataFile()))
            )
          )
        ),
        collapse = "\n"
      )
    }
    text
  })

  #----  Mapping tab ----

  # Error messages when there is no data file selection or the data set is invalid
  output$mapNAUI <- renderUI({
    if ( input$platformInput != "NONMEM") {
      return(
        message_box(
          text = "Variable mapping is not available for the selected software platform",
          icon = "info-circle-fill",
          theme = "info"
        )
      )
    } else {

      if ( inherits(try(dataFileReactive(), silent = TRUE), "try-error") ){
        return(
          message_box(
            text = "No data file was selected",
            icon = "info-circle-fill",
            theme = "info"
          )
        )
      }

      if ( inherits(dataFile(), "try-error") ){
        return(
          message_box(
            text = "Invalid data file",
            icon = "x-circle-fill",
            theme = "danger"
          )
        )
      }

      NULL

    }
  })

  # Data variables
  dataVars <- reactive({

    if ( inherits(try(dataFile(), silent = TRUE), "try-error") ){
      vars <- ""
    } else {
      # Get list of available variables
      vars <- c("", sort(unique(names(dataFile()))) )
    }

    if ( input$platformInput != "NONMEM" ){
      vars <- NULL
    }

    vars

  })


  # Default content of the table
  mapTableInputContent <- reactive({

    req( dataVars())

    if ( all(dataVars() == "") ){
      vars <- rep("", 8)
    } else {
      # Map variables
      idVar <- intersect(c("ID", "PAT"), dataVars())[1]
      idvVar <- intersect(c("TIME", "TSFE", "TSFD"), dataVars())[1]
      dvidVar <- intersect(c("DVID", "CMT"), dataVars())[1]
      tadVar <- intersect(c("TAD", "TPD", "TSPD", "TSLD"), dataVars())[1]
      blqVar <- intersect(c("BLQFN", "BQLFN", "BLQN", "BQLN", "BLQ", "BQL"), dataVars())[1]

      dvVar <- ifelse("DV" %in% dataVars(), "DV", "")
      cmtVar <- sort(dataVars()[grepl("^CMT", dataVars())])
      cmtVar <- ifelse( length(cmtVar) == 0, "", cmtVar[1] )
      amtVar <- sort(dataVars()[grepl("^AMT|^DOSE", dataVars())])
      amtVar <- ifelse( length(amtVar) == 0, "", amtVar[1] )
      rateVar <- sort(dataVars()[grepl("^RATE", dataVars())])
      rateVar <- ifelse( length(rateVar) == 0, "", rateVar[1] )

      vars <- c(idVar, idvVar, dvVar, cmtVar, dvidVar, tadVar, amtVar, rateVar, blqVar)

      vars <- factor(ifelse(is.na(vars), "", vars), levels = dataVars())
    }

    DF <- data.frame(
      Description = c(
        "Subject identifier variable",
        "Independent variable",
        "Dependent variable",
        "Compartment variable",
        "Endpoint identifier variable",
        "Time after dose variable",
        "Amount ariable",
        "Rate variable",
        "BLQ variable"
      ),
      NONMEM = c("ID", "TIME", "DV", "CMT", "", "", "AMT", "RATE", ""),
      Variable = vars,
      stringsAsFactors = FALSE
    )
  })

  # The table
  output$mapTable <- renderRHandsontable({

    req( dataVars())

    DF <- mapTableInputContent()

    tmp <- rhandsontable(
      data = DF,
      rowHeaders = FALSE,
      colHeaders = c("Definition", "Reserved keyword", "Dataset variable"),
      contextMenu = FALSE,
      manualColumnMove = FALSE,
      manualRowMove = FALSE,
      #width = 330#,
      height = 250  # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(col = 1, colWidths = 200, readOnly = TRUE) %>%
      hot_col(col = 2, colWidths = 100, readOnly = TRUE) %>%
      hot_col(col = 3, colWidths = 100, type = 'dropdown', source = dataVars()) %>%
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

  outputOptions(output, "mapTable", suspendWhenHidden = FALSE)

  output$mapTableUI <- renderUI({

    tagList(
      if ( !is.null(dataVars()) && !all(dataVars() == "") ){
        h4(strong("Dataset variable mapping"))
      },
      rHandsontableOutput("mapTable")
    )

  })

  # Variables to be dropped

  output$mapDropUI <- renderUI({

    req( dataVars())

    selectInput(
      inputId = "mapDropVarsInput",
      label = "Variables to be dropped in $INPUT",
      choices = dataVars(),
      selected = "",
      multiple = TRUE
    )

  })

  # List of continuous covariables

  output$mapContVarUI <- renderUI({

    req( dataVars())

    pattern <- paste(
      c("^AGE$","^ALB$","^ALP$","^ALT$","^AST$","^AUC.*$","^BILI$","^BMI$","^BSA$",
        "^BUN$","^BW$","^CAVG$","^CLAST$","^CMAX$","^CMIN$","^CPK$","^CRCL$",
        "^CSS$","^CTROUGH$","^EGFR.*$","^GG$","^HBA1C$","^HR$","^HTCM$","^IBW$",
        "^LBM$","^RBC$","^SCR$","^TBIL$","^WBC$","^WTKG$"),
      collapse = "|"
    )

    selectInput(
      inputId = "mapContVarInput",
      label = "List of continuous variables",
      choices = unique(dataVars()),
      selected = dataVars()[grepl(pattern, dataVars())],
      multiple = TRUE
    )

  })

  # List of categorical covariables

  output$mapCatVarUI <- renderUI({

    req( dataVars())

    pattern <- paste(
      c("^AGECAT$","^BLQ.*$","^COUNTRY$","^ELDERLY$","^DOSE.*$","^FED$","^FP$",
        "^GTRT$","^HFCAT$","^PNUM$","^POP.*$","^RAC.*$","^REGION$","^RFCAT$",
        "^SEX.*$","^STUDY$","^TGNUM$","^WTCAT$"),
      collapse = "|"
    )

    selectInput(
      inputId = "mapCatVarInput",
      label = "List of categorical variables",
      choices = unique(dataVars()),
      selected = dataVars()[grepl(pattern, dataVars())],
      multiple = TRUE
    )

  })

  # Mapped variables
  mappedVars <- reactive({

    req( input$platformInput )

    if ( notTruthy(dataVars()) ){
      return(NULL)
    }

    tmp <- dataVars()

    # Remove empty string
    if ( any(tmp == "") ){
      tmp <- tmp[ -which(tmp == "") ]
    }

    if ( notTruthy(input$mapTable) ){
      return(tmp)
    }

    #  Get index of variables to be dropped
    if ( length(input$mapDropVarsInput) > 0 && !any(input$mapDropVarsInput == '') ){
      matches <- which(tmp %in% input$mapDropVarsInput)
    } else {
      matches <- NULL
    }

    # Mapped variables
    mapTable <- hot_to_r(input$mapTable)

    oVars <- c("ID", "TIME", "DV", "CMT", NA, NA, "AMT", "RATE", NA)

    for (i in seq_along(oVars) ){
      if ( is.na(oVars[i]) ){
        next
      }

      if ( mapTable[i, "Variable"] != "" ) {
        if ( mapTable[i, "Variable"] != oVars[i] ){
          tmp[ match(oVars[i], tmp) ] <- paste0("O",  oVars[i])
        }
        tmp[match(mapTable[i, "Variable"], tmp)] <-  oVars[i]
      }

    }

    # Add "=DROP"
    if ( length(matches) > 0 ){
      tmp[matches] <- paste0(tmp[matches], "=DROP")
    }

    gsub("[^[:alnum:]]", "", tmp)

  })

  #---- PK Structure ----

  input_advan_lib <- data.frame(
    CMT = rep(1:3, each = 5),
    INPUT = rep(
      c( "bolus", "zero", "first", "sig", "transit" ),
      times = 3
    ),
    ADVAN = c( 1, 1, 2, 2, NA, 3, 3, 4, 4, NA, 11, 11, 12, 12, NA
    ),
    stringsAsFactors = FALSE
  )

  advan_trans_lib <- data.frame(
    ADVAN = c(NA_integer_, 1:15),
    TRANS = c(
      "1",
      rep("1,2", 2),
      rep("1,3,4,5,6", 2),
      rep("1", 6),
      rep("1,4,6", 2),
      rep("1", 3)
    ),
    DEFAULT = c(
      1,
      rep(2, 2),
      rep(4, 2),
      rep(1, 6),
      rep(4, 2),
      rep(1, 3)
    )
  )

  # Detect if this a $PRED model
  output$isPKpred <- isPKpred <- reactive({
    req( input$pkInput)
    input$pkInput == "pred"
  })
  output$isPDpred <- isPDpred <- reactive({
    req( input$pkInput, input$pdInput)
    input$pkInput %in% c("none", "pred") & input$pdInput %in% c("er", "pred", "logistic", "ordcat")
  })
  isPRED <- reactive({
    req( isPKpred, isPDpred)
    isPKpred() | isPDpred()
  })

  # Detect if this a $PK model defined with ODEs
  isODE <- reactive({
    req( input$pkInput, input$pdInput, input$eliminationInput, input$poInput )
    input$pkInput == "ode" |
      (input$pkInput == "pk" & (input$eliminationInput != "lin" | grepl("transit", input$poInput)) ) |
      input$pdInput %in% c("ode", "idr") |
      (input$pdInput == "biophase" & input$pkInput != "linmat")
  })

  isODE_code <- reactive({
    req( input$pkInput, input$pdInput, input$eliminationInput )
    if ( input$platformInput == "NONMEM" ){
      isODE()
    } else {
      input$pkInput %in% c("ode", "pk", "linmat") | input$pdInput %in% c("ode", "idr", "biophase")
    }
  })

  # Define flag if this PK model is defined by first-order rates matrix
  isLINMAT <- reactive({
    req( input$pkInput, input$pdInput)
    input$pkInput == "linmat" & ! input$pdInput %in% c("idr", "ode")
  })

  # Define if this PK model can be parameterized using PREDPP in NONMEM
  output$isPREDPP <- isPREDPP <- reactive({
    req( isPRED, isODE, isLINMAT)
    !isPRED() & !isODE() & !isLINMAT()
  })

  outputOptions(output, "isPKpred", suspendWhenHidden = FALSE)
  outputOptions(output, "isPDpred", suspendWhenHidden = FALSE)
  outputOptions(output, "isPREDPP", suspendWhenHidden = FALSE)


  # Create UI component for number of PK compartment
  output$pkCmtUI <- renderUI({
    if ( input$pkInput == "pk" ){
      selectInput(
        inputId = "pkCMTInput",
        width = "100%",
        label = "Disposition",
        choices = c(
          "1-compartment" = 1,
          "2-compartment" = 2,
          "3-compartment" = 3
        ),
        selected = 1
      )
    }
  })

  # Create UI component for elimination
  output$eliminationUI <- renderUI({
    req( input$pkInput )
    selectInput(
      inputId = "eliminationInput",
      width = "100%",
      label = "Elimination",
      choices = list(
        "Basic" = c(
          "Linear"= "lin",
          "Saturable" = "mm",
          "Linear + saturable" = "mmlin"
        ),
        "TMDD" = c(
          "Full TMDD" = "tmdd",
          "QE" = "tmddqe",
          "QE, constant Rtot" = "tmddqer",
          "QSS" = "tmddqss",
          "QSS, constant Rtot" = "tmddqssr"
        )
      ),
      selected = "lin"
    )
  })

  # Create UI components for IV dosing
  output$ivDosingUI <- renderUI({
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      selectInput(
        inputId = "ivInput",
        width = "100%",
        label = "Intravascular dosing",
        choices = c(
          "None" = "none",
          "Bolus" = "bolus",
          "Infusion" = "zero"
        ),
        selected = "none"
      )
    }
  })
  output$ivRateUI <- renderUI({
    req( input$ivInput )
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      if ( input$ivInput == "zero" ) {
        if ( input$platformInput == "NONMEM" ) {
          choices <- c(
            "Fixed in dataset" = 0,
            "Estimated rate" = -1,
            "Estimated duration" = -2
          )
        } else {
          choices <- c(
            "Set rate" = -1,
            "Set duration" = -2
          )
        }
        selected <- choices[0]
        selectInput(
          inputId = "ivRateInput",
          width = "100%",
          label = "Zero-order rate",
          choices = choices,
          selected = selected
        )
      }
    }
  })

  # Create UI components for non-IV dosing
  output$poDosingUI <- renderUI({
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      choices <- c(
        "None" = "none",
        "First-order" = "first",
        "Sigmoid" = "sig",
        "Transit compartments" = "transit"
      )
      if ( input$pkInput == "linmat" ){
        choices <- choices[ !grepl( "transit", choices) ]
      }
      selectInput(
        inputId = "poInput",
        width = "100%",
        label = "Extravascular dosing",
        choices = choices,
        selected = "first"
      )
    }
  })
  output$poRateUI <- renderUI({
    req( input$poInput )
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      if ( input$poInput == "sig" ) {
        if ( input$platformInput == "NONMEM" ) {
          choices <- c(
            "Estimated rate" = -1,
            "Estimated duration" = -2
          )
        } else {
          choices <- c(
            "Set rate" = -1,
            "Set duration" = -2
          )
        }
        selected <- choices[2]
        selectInput(
          inputId = "poRateInput",
          width = "100%",
          label = "Zero-order rate",
          choices = choices,
          selected = selected
        )
          # offset = ifelse( input$ivInput == "zero", 0, 4)
      }
    }
  })
  output$alagUI1 <- renderUI({
    req( input$poInput )

    if ( grepl("transit|none", input$poInput) ){
      return(NULL)
    }

    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      radioButtons(
        inputId = "alagInput1",
        label = "Include dosing lag?",
        choices = choices <- c("Yes" = TRUE, "No" = FALSE),
        selected = FALSE,
        inline = FALSE
      )
        # offset = ifelse( input$ivInput == "zero", 0, 4)
    }
  })

  output$alagUI2 <- renderUI({
    req( input$poInput )

    if ( grepl("transit|none", input$poInput) ){
      return(NULL)
    }

    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      radioButtons(
        inputId = "alagInput2",
        label = "Include dosing lag?",
        choices = choices <- c("Yes" = TRUE, "No" = FALSE),
        selected = FALSE,
        inline = FALSE
      )
    }
  })

  # Create reactive with simplified absorption info
  absorptionInput <- reactive({
    req( input$ivInput, input$poInput )

    sub(
      "none_|zero_|bolus_",
      "",
      sub(
        "_none",
        "",
        paste(input$ivInput, input$poInput, sep = "_")
      )
    )

  })

  # Create UI components for disposition
  output$pknCMTUI <- renderUI({
    req( input$ivInput )
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      numericInput(
        inputId = "pknCMTInput",
        width = "100%",
        label = "Number of compartments",
        min = ifelse(grepl("bolus|zero", input$ivInput), 1, 2),
        value = ifelse(grepl("bolus|zero", input$ivInput), 1, 2),
        step = 1
      )
    }
  })
  output$pkDefaultDoseUI <- renderUI({
    req( input$pknCMTInput)
    numericInput(
      inputId = "pkDefaultDoseInput",
      width = "100%",
      label = "Default dosing compartment",
      min = 1,
      max = as.numeric(input$pknCMTInput),
      value = 1,
      step = 1
    )
  })
  output$pkDefaultObsUI <- renderUI({
    req( input$pknCMTInput, input$ivInput )
    numericInput(
      inputId = "pkDefaultObsInput",
      width = "100%",
      label = "Default observation compartment",
      min = 1,
      max = as.numeric(input$pknCMTInput),
      value = ifelse(grepl("bolus|zero", input$ivInput), 1, 2),
      step = 1
    )
  })

  # Create Ui for first row for LINMAT and ODE model
  output$pkFirstRowUI <- renderUI({
    req(input$pkInput)
    if ( input$pkInput %in% c("linmat", "ode") ){
      fluidRow(
        col_4( uiOutput("pknCMTUI") ),
        col_4( uiOutput("pkDefaultDoseUI") ),
        col_4( uiOutput("pkDefaultObsUI") )
      )
    }
  })

  # Create UI for second row
  output$pkSecondRowUI <- renderUI({
    req(input$pkInput)
    ui <- NULL
    if ( input$pkInput == "pk" ) {
      ui <- fluidRow(
        col_4( uiOutput("pkCmtUI") ),
        col_4( uiOutput("ivDosingUI") ),
        col_4( uiOutput("poDosingUI") )
      )
    } else if ( input$pkInput %in% c("linmat", "ode") ) {
      ui <- fluidRow(
        col_4( uiOutput("ivDosingUI") ),
        col_4( uiOutput("poDosingUI") )
      )
    }
    ui
  })

  # Create UI for third row
  output$pkThirdRowUI <- renderUI({
    req(input$pkInput, input$poInput)
    ui <- NULL
    if ( input$pkInput == "pk" ) {
      ui <- fluidRow(
        col_4( uiOutput("eliminationUI") ),
        col_4( uiOutput("ivRateUI") ),
        col_4(
          uiOutput( ifelse(input$poInput != "sig", "alagUI1", "poRateUI") )
        )
      )
    } else if ( input$pkInput %in% c("linmat", "ode") ) {
      ui <- fluidRow(
        col_4( uiOutput("ivRateUI") ),
        col_4(
          uiOutput( ifelse(input$poInput != "sig", "alagUI1", "poRateUI") )
        )
      )
    }
    ui
  })

  # Create UI for fourth row
  output$pkFourthRowUI <- renderUI({
    req(input$pkInput, input$eliminationInput, input$poInput)
    fluidRow(
      if ( input$pkInput == "pk" & input$eliminationInput %in% c("mm", "mmlin") ){
        col_4(
          selectInput(
            inputId = "kmScaleInput",
            width = "100%",
            label = "KM scale",
            choices = c(
              "Concentration"= TRUE, "Amount" = FALSE
            ),
            selected = TRUE
          )
        )
      },
      if ( input$pkInput == "pk" & grepl("^tmdd", input$eliminationInput) ) {
        col_8( uiOutput("tmddUI") )
      },
      if ( input$pkInput %in% c("pk", "linmat", "ode") & input$poInput == "sig" ) {
        col_4(
          uiOutput("alagUI2"),
          offset = dplyr::case_when(
            # TMDD model
            grepl( 'tmdd', input$eliminationInput ) ~ 0,
            # Saturable or linear+saturation elimination
            input$eliminationInput != 'lin' ~ 4,
            TRUE ~ 8
          )
        )
      }
    )
  })

  # Create UI for ADVAN/TRANS parameterization
  output$advanUI <- renderUI({

    if ( isPRED() ){
      return(NULL)
    }

    req( input$pkInput, input$eliminationInput )

    if ( input$pkInput == "ode" |
         (input$pkInput == "pk" & input$eliminationInput != "lin" | grepl("transit", input$poInput)) |
         input$pdInput %in% c("ode", "idr") | (input$pdInput == "biophase" & input$pkInput != "linmat")
    ) {
      choices <- c("ADVAN6" = 6, "ADVAN8" = 8, "ADVAN9" = 9, "ADVAN13" = 13, "ADVAN14" = 14, "ADVAN15" = 15)
      selected <- 13
    } else if ( input$pkInput == "linmat" & input$pdInput != "idr" & input$pdInput != "ode" ){
      choices <- c("ADVAN5" = 5, "ADVAN7" = 7)
      selected <- 5
    } else {
      choices <- paste0("ADVAN", advan())
      selected <- paste0("ADVAN", advan())
    }

    selectInput(
      inputId = "advanInput",
      width = "100%",
      label = "ADVAN",
      choices = choices,
      selected = selected
    )

  })

  advan <- reactive({
    if ( isPRED() ){
      advan <- NA
    } else {

      if ( isODE() | isLINMAT() ){
        advan <- input$advanInput

        req( advan %in% c(6, 8, 9, 13:15) )

      } else {

        req( input$ivInput, input$poInput, absorptionInput() )

        req( (input$ivInput != "none" | input$poInput != "none") )

        advan <- input_advan_lib %>%
          dplyr::filter(
            .data$CMT == as.integer(input$pkCMTInput) &
              .data$INPUT == absorptionInput() ) %>%
          dplyr::pull(.data$ADVAN)

      }
    }
    advan
  })

  output$transUI <- renderUI({

    if ( isPRED() ){
      return(NULL)
    }

    req( advan() )

    selectInput(
      inputId = "transInput",
      width = "100%",
      label = "TRANS",
      choices = paste0(
        "TRANS",
        if ( is.na(advan()) ){
          advan_trans_lib %>%
            dplyr::filter(is.na(.data$ADVAN)) %>%
            dplyr::pull(.data$TRANS)
        } else {
          unlist(
            strsplit(
              advan_trans_lib %>%
                dplyr::filter(.data$ADVAN == advan() & !is.na(.data$ADVAN)) %>%
                dplyr::pull(.data$TRANS),
              ","
            )
          )
        }
      ),
      selected = paste0(
        "TRANS",
        ifelse(
          is.na(advan()),
          advan_trans_lib %>%
            dplyr::filter(is.na(.data$ADVAN)) %>%
            dplyr::pull(.data$DEFAULT),
          advan_trans_lib %>%
            dplyr::filter( .data$ADVAN == advan() & !is.na(.data$ADVAN) ) %>%
            dplyr::pull(.data$DEFAULT)
        )
      )
    )
  })


  # Detect which TRANS should be used by default or was selected
  trans <- reactive({
    req( input$transInput )
    if ( isPRED() ){
      if ( isPKpred() ){
        trans <- NA
      }
    } else {
      if ( isODE() | isLINMAT() ){
        trans <- 1
      } else {
        if ( length(input$transInput) == 0 ){
          trans <- if ( is.na(advan()) ){
            1
          } else {
            advan_trans_lib %>%
              dplyr::slice( advan() ) %>%
              dplyr::pull(.data$DEFAULT)
          }
        } else {
          trans <- as.numeric(sub("TRANS", "", input$transInput))
        }
      }
    }
    trans
  })

  # Create UI for selection of TMDD estimated parameters
  output$tmddUI <- renderUI({
    req( parm_lib, input$eliminationInput, absorptionInput() )

    # Get info about TMDD parameters from parm_lib
    tmdd_parms <- parm_lib %>%
      dplyr::filter(
        .data$CMT == input$pkCMTInput &
          grepl(absorptionInput(), .data$ABSORPTION) &
          .data$ELIMINATION == input$eliminationInput
      )

    # Process parameter choices
    choices <- tmdd_parms$TRANS
    names(choices) <- gsub(
      "[|]+", ", ",
      gsub(
        "^[|]+|[|]+$", "",
        gsub(
          "CL|VC|Q|VP|CLD1|CLD2|VP1|VP2|KA|MTT|NN", "",
          tmdd_parms$PARMS
        )
      )
    )

    # Build UI
    selectInput(
      inputId = "tmddInput",
      width = "100%",
      label = "Estimated TMDD parameters",
      choices = choices,
      selected = choices[1]
    )
  })

  #---- PD Structure ----

  output$pdUI <- renderUI({

    if ( input$pkInput %in% c("pk", "linmat", "ode") ){
      choices <- c(
        "None" = "none",
        "Direct effect" = "direct",
        "Biophase / Link" = "biophase",
        "Indirect response" = "idr",
        "Defined by ODEs" = "ode"
      )
      selected <- "none"
    } else if ( input$pkInput == "pred" ){
      choices <- c(
        "None" = "none",
        "Exposure-Response" = "er",
        "Defined by explicit solutions" = "pred"
      )
      selected <- "none"
    } else {
      if ( input$platformInput != "Berkeley Madonna" ){
        choices <- c(
          "Exposure-Response" = "er",
          "Defined by ODEs" = "ode",
          "Defined by explicit solutions" = "pred",
          "Logistic regression" = "logistic",
          "Ordered categorical model" = "ordcat"
        )
      } else {
        choices <- c(
          "Exposure-Response" = "er",
          "Defined by ODEs" = "ode",
          "Defined by explicit solutions" = "pred"
        )
      }

      selected <- "er"
    }
    selectInput(
      inputId = "pdInput",
      width = "100%",
      label = NULL,
      choices = choices,
      selected = selected
    )

  })

  # Create UI for selection of parameterization of direct effect, biophase,
  # E-R, logistic regression, and ordered categorical PD models
  output$endpointUI <- renderUI({
    if ( input$pdInput == "ordcat" ) {
      textInput(
        inputId = "endpointInput",
        width = "100%",
        label = "Endpoint",
        placeholder = "Enter the endpoint name"
      )
    }
  })
  output$minCategoryUI <- renderUI({
    if ( input$pdInput == "ordcat" ) {
      numericInput(
        inputId = "minCategoryInput",
        width = "100%",
        label = "Min. category",
        min = 0,
        value = 0,
        step = 1
      )
    } else {
      NULL
    }
  })
  output$maxCategoryUI <- renderUI({
    if ( input$pdInput == "ordcat" ) {
      numericInput(
        inputId = "maxCategoryInput",
        width = "100%",
        label = "Max. category",
        min = 2,
        value = 2,
        step = 1
      )
    } else {
      NULL
    }
  })

  output$effectFormUI <- renderUI({
    if ( input$pdInput %in% c("logistic", "ordcat") ) {
      choices <- c(
        "None" = "base",
        "Linear" = "lin",
        "Power" = "power",
        "Exponential" = "exp",
        "Michaelis-Menten" = "mm",
        "Hill" = "hill",
        "Weibull" = "weibull"
      )
    } else {
      choices <- c(
        "Linear" = "lin",
        "Power" = "power",
        "Exponential" = "exp",
        "Michaelis-Menten" = "mm",
        "Hill" = "hill",
        "Weibull" = "weibull"
      )
    }
    selectInput(
      inputId = "effectFormInput",
      width = "100%",
      label = ifelse(input$pdInput == "er", "Functional form", "Drug effect form"),
      choices = choices,
      selected = "lin"
    )
  })
  output$effectParmUI <- renderUI({

    req( input$effectFormInput )

    type <- ifelse(
      input$pdInput %in% c("logistic", "ordcat"),
      "logistic_ordcat",
      "direct_er"
    )
    choices <- pdForm_lib %>%
      dplyr::filter(.data$TYPE == type & .data$FORM ==input$effectFormInput ) %>%
      dplyr::pull(.data$PARAMETERIZATION)

    selectInput(
      inputId = "effectParmInput",
      width = "100%",
      label = "Parameterization",
      choices = choices,
      selected = choices[1]
    )
  })

  output$effectStimUI <- renderUI({

    req( input$pdInput, input$effectFormInput, input$effectParmInput)

    if ( isTruthy(input$effectFormInput) && input$effectFormInput == "base" ) {
      NULL
    } else {

      type <- ifelse(
        input$pdInput %in% c("logistic", "ordcat"),
        "logistic_ordcat",
        "direct_er"
      )
      choices <- pdForm_lib %>%
        dplyr::filter(
          .data$TYPE == "logistic_ordcat" &
            .data$FORM == input$effectFormInput &
            .data$PARAMETERIZATION == as.numeric(input$effectParmInput)
        ) %>%
        dplyr::select(.data$INCREASE, .data$DECREASE) %>%
        unlist() %>%
        as.vector()

      if ( input$pdInput == "er" ){
        if ( all(choices == 1L) ) {
          choices <- c("Increasing" = TRUE, "Decreasing" = FALSE)
        } else if ( choices[1] == 0L & choices[2] == 1L ){
          choices <- c("Decreasing" = FALSE)
        } else {
          choices <- c("Increasing" = TRUE)
        }
      } else {
        if ( all(choices == 1L) ) {
          choices <- c("Stimulatory" = TRUE, "Inhibitory" = FALSE)
        } else if ( choices[1] == 0L & choices[2] == 1L ){
          choices <- c("Inhibitory" = FALSE)
        } else {
          choices <- c("Stimulatory" = TRUE)
        }
      }

      selectInput(
        inputId = "effectStimInput",
        width = "100%",
        label = ifelse(input$pdInput == "er", "Function direction", "Effect type"),
        choices = choices,
        selected = choices[1]
      )
    }
  })

  # Create UI showing prototypical math of selection model
  output$effectMathjax <- renderUI({

    req( input$effectFormInput, input$effectParmInput, input$effectStimInput )

    withMathJax(
      helpText(
        glue::glue(
          "Prototypical model: \\(\\quad {math}\\)",
          math = parm_lib %>%
            dplyr::filter(
              .data$TYPE == "function" &
                .data$FORM == input$effectFormInput &
                .data$TRANS == as.numeric(input$effectParmInput) &
                .data$INCREASE == as.integer(as.logical(input$effectStimInput))
            ) %>%
            dplyr::pull(.data$MATHJAX)
        )
      )
    )
  })

  output$exposureVarUI <- renderUI({
    req( input$pdInput)

    if ( input$pdInput != "er" | input$platformInput != "NONMEM" ){
      return(NULL)
    }

    if ( length(dataVars()) > 0 && any(dataVars() != "" ) ){
      selectInput(
        inputId = "exposureVarInput",
        width = "100%",
        label = "Exposure variable",
        choices = unique( c("", dataVars()[which(dataVars() != "DV")]) ),
        selected  = ""
      )
    } else {
      textInput(
        inputId = "exposureVarTextInput",
        width = "100%",
        label = "Exposure variable",
        placeholder = "Enter a valid variable name"
      )
    }
  })

  output$logisticDriverVarUI <- renderUI({
    req( input$pdInput )

    if ( !input$pdInput %in% c("logistic", "ordcat") | input$platformInput != "NONMEM" ){
      return(NULL)
    }

    if ( length(dataVars()) > 0 && any(dataVars() != "") ){
      selectInput(
        inputId = "logisticVarInput",
        width = "100%",
        label = "Response driver",
        choices = unique( c("", dataVars()[which(dataVars() != "DV")]) ),
        selected  = ""
      )
    } else {
      textInput(
        inputId = "logisticVarTextInput",
        width = "100%",
        label = "Response driver",
        placeholder = "Enter a valid variable name"
      )
    }
  })

  output$effectDriverUI <- renderUI({

    req( input$pkInput, input$pdInput )

    max <- value <- 1
    if ( input$pkInput == "pk" ){
      max <- as.numeric(input$pkCMTInput)
      value <- 1
      if ( !grepl("zero|bolus", input$ivInput) ){
        max <- max + 1
        value <- value + 1
      }
    } else if ( input$pkInput %in% c("linmat", "ode") ){
      max <- as.numeric(input$pknCMTInput)
      value <- as.numeric(input$pkDefaultObsInput)
    }
    min <- 1
    if ( input$pdInput == "biophase" ){
      min <- max <- value <- value + as.numeric(input$pkCMTInput)
    }

    numericInput(
      inputId = "effectCmtDriverInput",
      width = "100%",
      label = "Compartment driving effect",
      min = min,
      max = max,
      step = 1,
      value = value
    )
  })

  ## Create UI for the parameterization of IDR models
  output$idrStimUI <- renderUI({

    req( input$idrTypeInput )

    if ( input$idrTypeInput %in% c("idr3", "idr4") ){
      choices <- c(
        "Linear" = "lin",
        "Power" = "pow",
        "Exponential" = "exp",
        "Michaelis-Menten" = "mm",
        "Hill" = "hill")
    } else {
      choices <- c(
        "Michaelis-Menten" = "mm",
        "Hill" = "hill"
      )
    }
    selectInput(
      inputId = "idrStimInput",
      width = "100%",
      label = "Drug effect",
      choices = choices,
      selected = choices[1]
    )
  })

  # UI showing prototypical math model
  output$idrMathjax <- renderUI({

    req( input$idrTypeInput, input$idrParmInput, input$idrStimInput )

    fluidRow()
    withMathJax(
      helpText(
        glue::glue(
          "Prototypical model: \\(\\quad \\begin{{align}}{math1} \\\\ {math2}\\end{{align}}\\)",
          math1 = parm_lib %>%
            dplyr::filter(
              .data$TYPE == "idr" &
                .data$FORM == input$idrTypeInput &
                .data$TRANS == input$idrParmInput
            ) %>%
            dplyr::pull(.data$MATHJAX),
          math2 = parm_lib %>%
            dplyr::filter(
              .data$TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
                .data$FORM == input$idrStimInput
            ) %>%
            dplyr::pull(.data$MATHJAX)
        )
      )
    )

  })

  #---- Parameters tab ----

  output$parameterWarningUI <- renderUI({

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        fluidRow(
          col_12(
            message_box(
              text = "No model structure defined",
              icon = "info-circle-fill",
              theme = "info"
            )
          )
        )
      )
    }

  })

  scales <- reactive({

    req( input$pkInput, input$pdInput, input$nOTParmInput )

    req( input$pkInput != "none" | input$pdInput != "none" )

    scales <- c("Linear", "Log", "Logit")

    if ( any( input$pdInput %in% c("logistic", "ordcat") ) ){
      scales <- "Linear"
    }

    scales

  })

  ### Parameter matrix

  parameterDF <- reactive({

    req( input$pkInput, input$pdInput, scales(), input$nOTParmInput, input$new_menu == "Parameters" )

    req( input$pkInput != "none" | input$pdInput != "none" )

    nparms <- 0

    ### PK parameters
    if ( input$pkInput == "none" )
    {

      pkDF <- data.frame(
        Type = character(),
        SourceParam = character(),
        Parameter = character(),
        Label = character(),
        Unit = character(),
        Min = character(),
        Initial = character(),
        Max = character(),
        Fixed = character(),
        Variability = character(),
        Scale = character(),
        stringsAsFactors = FALSE
      )

    } else if ( input$pkInput == "pk")
    {
      req( input$ivInput, input$poInput, input$eliminationInput )

      dual <- ifelse( input$ivInput != "none" & input$poInput != "none", 1, 0 )

      index <- get_model_lib_index(
        input = input,
        advan = advan,
        trans = trans,
        parm_lib = parm_lib
      )
      parm_info <- parm_lib %>%
        dplyr::slice( index ) %>%
        tidyr::separate_rows(
          .data$PARMS, .data$VAR, .data$MIN, .data$INITIAL, .data$MAX,
          sep = "[|]"
        )

      req( nrow(parm_info) > 0 )

      pkDF <- data.frame(
        Type = rep("PK", nrow(parm_info)),
        SourceParam = parm_info$PARMS,
        Parameter = parm_info$PARMS,
        Label = parm_info$PARMS,
        Unit = parm_info$PARMS,
        Min = parm_info$MIN,
        Initial = parm_info$INITIAL,
        Max = parm_info$MAX,
        Fixed = "No",
        Variability = parm_info$VAR,
        Scale = "Linear",
        stringsAsFactors = FALSE
      )

      # Add zero-order parameters for infusion
      if ( input$ivInput == "zero" ){
        rate <- as.numeric(input$ivRateInput)
        if ( length(rate) > 0 && rate < 0 ){
          PARM <- ifelse(
            rate == -1,
            glue::glue("R{dual + 1}"),
            glue::glue("D{dual + 1}")
          )
          pkDF <- pkDF %>%
            dplyr::bind_rows(
              data.frame(
                Type = "PK",
                SourceParam = PARM,
                Parameter = PARM,
                Label = PARM,
                Unit = PARM,
                Min = "0",
                Initial = "1",
                Max = "+INF",
                Fixed = "No",
                Variability = "exp",
                Scale = "Linear",
                stringsAsFactors = FALSE
              )
            )
        }
      }
      # Add zero-order parameters for sigmoid absorption
      if ( input$poInput == "sig" ){
        rate <- as.numeric(input$poRateInput)
        if ( length(rate) > 0 && rate < 0 ){
          PARM <- ifelse(rate == -1, "R1", "D1")
          pkDF <- pkDF %>%
            dplyr::bind_rows(
              data.frame(
                Type = "PK",
                SourceParam = PARM,
                Parameter = PARM,
                Label = PARM,
                Unit = PARM,
                Min = "0",
                Initial = "1",
                Max = "+INF",
                Fixed = "No",
                Variability = "exp",
                Scale = "Linear",
                stringsAsFactors = FALSE
              )
            )
        }
      }
      if ( ( length(input$alagInput1) > 0 && as.logical(input$alagInput1) ) |
           ( length(input$alagInput2) > 0 && as.logical(input$alagInput2) )
      ){
        pkDF <- pkDF %>%
          dplyr::bind_rows(
            data.frame(
              Type = "PK",
              SourceParam = "ALAG1",
              Parameter = "ALAG1",
              Label = "ALAG1",
              Unit = "ALAG1",
              Min = "0",
              Initial = "1",
              Max = "+INF",
              Fixed = "No",
              Variability = "none",
              Scale = "Linear",
              stringsAsFactors = FALSE
            )
          )
      }
      if ( dual == 1 ){
        pkDF <- pkDF %>%
          dplyr::bind_rows(
            data.frame(
              Type = "PK",
              SourceParam = "F1",
              Parameter = "F1",
              Label = "F1",
              Unit = "F1",
              Min = "0",
              Initial = "0.9",
              Max = "1",
              Fixed = "No",
              Variability = "none",
              Scale = "Linear",
              stringsAsFactors = FALSE
            )
          )
      }
    } else
    {

      req( input$ivInput, input$poInput )

      dual <- ifelse( input$ivInput != "none" & input$poInput != "none", 1, 0 )

      PKparms <- paste0("TH", 1:abs(as.numeric(input$nPKParmInput)) )
      if ( !isPRED() & input$poInput %in% c("first", "sig") ){
        PKparms <- c(PKparms, "KA")
      }

      # Add zero-order parameters for infusion
      if ( !isPRED() & input$ivInput == "zero" ){
        rate <- as.numeric(input$ivRateInput)
        if ( rate < 0 ){
          PKparms <- c(
            PKparms,
            ifelse(
              rate == -1,
              glue::glue("R{input$pkDefaultDoseInput + dual}"),
              glue::glue("D{input$pkDefaultDoseInput + dual}")
            )
          )
        }
      }
      # Add zero-order parameters for sigmoid absorption
      if ( !isPRED() & input$poInput == "sig" ){
        rate <- as.numeric(input$poRateInput)
        if ( rate < 0 ){
          PKparms <- c(
            PKparms,
            ifelse(
              rate == -1,
              glue::glue("R{input$pkDefaultDoseInput}"),
              glue::glue("D{input$pkDefaultDoseInput}")
            )
          )
        }
      }
      if ( !isPRED() & absorptionInput() == "transit" ){
        PKparms <- c(PKparms, "MTT", "NN")
      }
      if ( !isPRED() & (
        ( length(input$alagInput1) > 0 && as.logical(input$alagInput1) ) |
        ( length(input$alagInput2) > 0 && as.logical(input$alagInput2) ) )
      ){
        PKparms <- c(
          PKparms,
          glue::glue("ALAG{input$pkDefaultDoseInput}")
        )
      }

      pkDF <- data.frame(
        Type = "PK",
        SourceParam = PKparms,
        Parameter = PKparms,
        Label = PKparms,
        Unit = PKparms,
        Min = "0",
        Initial = "1",
        Max = "+INF",
        Fixed = "No",
        Variability = ifelse(grepl("ALAG", PKparms), "none", "exp"),
        Scale = "Linear",
        stringsAsFactors = FALSE
      ) %>%
        dplyr::mutate(
          Min = ifelse(.data$Parameter == "NN", "1", "0"),
          Initial = ifelse(.data$Parameter == "NN", "2", "1")
        )

      if ( dual ){
        pkDF <- pkDF %>%
          dplyr::bind_rows(
            data.frame(
              Type = "PK",
              SourceParam = glue::glue("F{input$pkDefaultDoseInput}"),
              Parameter = glue::glue("F{input$pkDefaultDoseInput}"),
              Label = glue::glue("F{input$pkDefaultDoseInput}"),
              Unit = glue::glue("F{input$pkDefaultDoseInput}"),
              Min = "0",
              Initial = "0.9",
              Max = "1",
              Fixed = "No",
              Variability = "none",
              Scale = "Linear",
              stringsAsFactors = FALSE
            )
          )
      }

    }

    ### PD parameters
    if ( input$pdInput == "none" )
    {

      parm_info <- NULL

    } else if ( input$pdInput %in% c("direct", "er") )
    {

      req( input$effectFormInput, input$effectParmInput, input$effectStimInput)

      parm_info <- parm_lib %>%
        dplyr::filter(
          .data$TYPE == "function" &
            .data$FORM == input$effectFormInput &
            .data$TRANS == input$effectParmInput &
            .data$INCREASE == as.integer(as.logical(input$effectStimInput))
        ) %>%
        tidyr::separate_rows(
          .data$PARMS, .data$VAR, .data$MIN, .data$INITIAL, .data$MAX,
          sep = "[|]"
        )

    } else if ( input$pdInput == "biophase")
    {

      req( input$effectFormInput, input$effectParmInput, input$effectStimInput)

      parm_info <- parm_lib %>%
        dplyr::filter(
          .data$TYPE == "biophase" |
            (
              .data$TYPE == "function" &
                .data$FORM == input$effectFormInput &
                .data$TRANS == input$effectParmInput &
                .data$INCREASE == as.integer(as.logical(input$effectStimInput))
            )
        ) %>%
        tidyr::separate_rows(
          .data$PARMS, .data$VAR, .data$MIN, .data$INITIAL, .data$MAX,
          sep = "[|]"
        )

    } else if ( input$pdInput == "idr")
    {

      req( input$idrTypeInput, input$idrParmInput, input$idrStimInput)

      parm_info <- parm_lib %>%
        dplyr::filter(
          (
            .data$TYPE == "idr" & .data$FORM == input$idrTypeInput & .data$TRANS == input$idrParmInput
          ) |
            (
              .data$TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
                .data$FORM == input$idrStimInput
            )
        ) %>%
        tidyr::separate_rows(
          .data$PARMS, .data$VAR, .data$MIN, .data$INITIAL, .data$MAX,
          sep = "[|]"
        )

    } else if ( input$pdInput %in% c("logistic", "ordcat"))
    {

      req( input$effectFormInput, input$effectParmInput, input$effectStimInput)

      if ( input$pdInput == "logistic") {

        parm_info <- parm_lib %>%
          dplyr::filter(.data$TYPE == "logistic") %>%
          tidyr::separate_rows(
            .data$PARMS, .data$VAR, .data$MIN, .data$INITIAL, .data$MAX,
            sep = "[|]"
          )

      } else {

        req( input$minCategoryInput, input$maxCategoryInput)


        if ( areTruthy(input$maxCategoryInput, input$minCategoryInput) ){
          if ( input$minCategoryInput <= input$maxCategoryInput ){
            minCat <- floor(input$minCategoryInput)
            maxCat <- ceiling(input$maxCategoryInput) - 1
          } else {
            minCat <- floor(input$maxCategoryInput)
            maxCat <- ceiling(input$minCategoryInput) - 1
          }
        } else {
          minCat <- 0
          maxCat <- 1
        }
        ncats <- maxCat - minCat + 1
        PDparms <- paste0("LGI", minCat:maxCat)
        if ( ncats > 1) {
          parm_lib <- parm_lib %>%
            dplyr::bind_rows(
              data.frame(
                PARMS = PDparms,
                VAR = c("add", rep("none", ncats - 1)),
                MIN = c("-INF", rep("0", ncats -1)),
                INITIAL = rep("1", ncats),
                MAX = rep("+INF", ncats),
                stringsAsFactors = FALSE
              )
            )
        } else {
          parm_lib <- parm_lib %>%
            dplyr::bind_rows(
              data.frame(
                PARM = PDparms,
                VAR = "add",
                MIN = "-INF",
                INITIAL = "1",
                MAX = "+INF",
                stringsAsFactors = FALSE
              )
            )
        }

        parm_info <- parm_lib %>%
          dplyr::filter(.data$PARMS %in% PDparms)

      }

      # Remove baseline parameter in function parameters
      stim_parm_info <- parm_lib %>%
        dplyr::filter(
          .data$TYPE == "function" &
            .data$FORM == input$effectFormInput &
            .data$TRANS == input$effectParmInput &
            .data$INCREASE == as.integer(as.logical(input$effectStimInput))
        ) %>%
        tidyr::separate_rows(
          .data$PARMS, .data$VAR, .data$MIN, .data$INITIAL, .data$MAX,
          sep = "[|]"
        ) %>%
        dplyr::slice( -1 )

      parm_info <- parm_info %>%
        dplyr::bind_rows(
          stim_parm_info
        )

    } else
    {

      nPDparms <- abs(as.numeric(input$nPDParmInput))
      PDparms <- paste0("TH", nrow(pkDF) + 1:nPDparms)

      parm_lib <- parm_lib %>%
        dplyr::bind_rows(
          data.frame(
            PARMS = PDparms,
            VAR = rep("exp", nPDparms),
            MIN = rep("0", nPDparms),
            INITIAL = rep("1", nPDparms),
            MAX = rep("+INF", nPDparms),
            stringsAsFactors = FALSE
          )
        )

      parm_info <- parm_lib %>%
        dplyr::filter(.data$PARMS %in% PDparms)

    }

    if ( is.null(parm_info) )
    {
      pdDF <- data.frame(
        Type = character(),
        SourceParam = character(),
        Parameter = character(),
        Label = character(),
        Unit = character(),
        Min = character(),
        Initial = character(),
        Max = character(),
        Fixed = character(),
        Variability = character(),
        Scale = character(),
        stringsAsFactors = FALSE
      )
    } else
    {
      pdDF <- data.frame(
        Type = rep("PD", nrow(parm_info)),
        SourceParam = parm_info$PARMS,
        Parameter = parm_info$PARMS,
        Label = parm_info$PARMS,
        Unit = parm_info$PARMS,
        Min = parm_info$MIN,
        Initial = parm_info$INITIAL,
        Max = parm_info$MAX,
        Fixed = "No",
        Variability = parm_info$VAR,
        Scale = "Linear",
        stringsAsFactors = FALSE
      )
    }

    ### Other parameters
    nOTparms <- abs(as.numeric(input$nOTParmInput))
    if ( nOTparms == 0 )
    {
      otherDF <- data.frame(
        Type = character(),
        SourceParam = character(),
        Parameter = character(),
        Label = character(),
        Unit = character(),
        Min = character(),
        Initial = character(),
        Max = character(),
        Fixed = character(),
        Variability = character(),
        Scale = character(),
        stringsAsFactors = FALSE
      )
    } else
    {
      OTparms <- paste0("TH", nrow(pkDF) + nrow(pdDF) + 1:nOTparms)
      parm_lib <- parm_lib %>%
        dplyr::bind_rows(
          data.frame(
            PARMS = OTparms,
            VAR = rep("exp", length(OTparms)),
            MIN = rep("0", length(OTparms)),
            INITIAL = rep("1", length(OTparms)),
            MAX = rep("+INF", length(OTparms)),
            stringsAsFactors = FALSE
          )
        )
      parm_info <- parm_lib %>%
        dplyr::filter(.data$PARMS %in% OTparms)

      otherDF <- data.frame(
        Type = rep("OT", nrow(parm_info)),
        SourceParam = parm_info$PARMS,
        Parameter = parm_info$PARMS,
        Label = parm_info$PARMS,
        Unit = parm_info$PARMS,
        Min = parm_info$MIN,
        Initial = parm_info$INITIAL,
        Max = parm_info$MAX,
        Fixed = "No",
        Variability = parm_info$VAR,
        Scale = "Linear",
        stringsAsFactors = FALSE
      )

    }

    ### Create table of parameters
    DF <- dplyr::bind_rows(pkDF, pdDF, otherDF) %>%
      dplyr::mutate(
        Label = get_labelunit(
          input = input,
          parms = .data$Label,
          labelunit_lib = labelunit_lib,
          what = "label"
        ),
        Unit = get_labelunit(
          input = input,
          parms = .data$Unit,
          labelunit_lib = labelunit_lib,
          what = "unit"
        ),
        Fixed = factor(.data$Fixed, levels = c("Yes", "No"), ordered = TRUE),
        Variability = as.numeric(
          dplyr::case_when(
            .data$Variability == "none" ~ "0",
            .data$Variability == "add" ~ "1",
            .data$Variability == "exp" ~ "2",
            .data$Variability == "logit" ~ "3",
            TRUE ~ NA_character_
          )
        ),
        Scale = factor("Linear", levels = scales(), ordered = TRUE),
      )

    ### Check content of input$parameterTable and preserve custom inputs
    if ( length(isolate(input$parameterTable)) > 0 )
    {
      oDF <- hot_to_r(isolate(input$parameterTable))
      if ( nrow(DF) == 0 ){
        DF <- oDF
      }
      if ( !identical(DF, oDF)) {

        mDF <- merge(
          cbind(
            DF,
            data.frame("_SORT_" = 1:nrow(DF))
          ),
          oDF,
          by = "SourceParam",
          all.x = TRUE
        )
        mDF <- mDF[order(mDF[, "X_SORT_"]), ]

        for (col in names(oDF)[-2] ){
          DF[, col] <- ifelse(
            is.na(mDF[,paste(col, "y", sep = ".")]) |
              (col == "Label" & mDF$SourceParam %in% c("KM", "IC50", "SC50")),
            mDF[, paste(col, "x", sep = ".")],
            mDF[, paste(col, "y", sep = ".")]
          )
          if ( is.factor(oDF[, col]) ){
            # ifelse coerces factors to integers, must reset to factor
            DF[, col] <- factor(levels(oDF[, col])[DF[, col]], levels = levels(oDF[, col]), ordered = TRUE)
          }
        }
      }
    }

    ### Adjust based upon software platform
    if ( input$platformInput %in% c("mrgsolve", "Berkeley Madaonna") ){
      DF$Fixed <- NULL
    }
    if ( input$platformInput == "Berkeley Madaonna" ){
      DF$Min <- DF$Max <- DF$Variability <- NULL
    }

    DF

  })

  output$parameterTable <- renderRHandsontable({

    req( parameterDF(), scales() )

    DF <- parameterDF()

    # Convert select variable to character (remove the glue class)
    DF <- DF %>%
      dplyr::mutate(
        Type = as.character(Type),
        SourceParam = as.character(SourceParam),
        Parameter = as.character(Parameter)
      )

    tmp <- rhandsontable(
      data = DF,
      rowHeaders = TRUE,
      contextMenu = FALSE,
      manualColumnMove = FALSE,
      manualRowMove = TRUE,
      width = ifelse( input$platformInput == "NONMEM", 690, 640) + 60,
      height = max(200, (nrow(DF) + 1)*25 + 10)  # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(col = "Type", readOnly = TRUE, colWidths = 50) %>%
      hot_col(col = "SourceParam", colWidths = 0.1) %>% # Hide the merge key SourceParm
      hot_col(col = "Parameter", colWidths = 90) %>%
      hot_col(col = "Label", colWidths = 250) %>%
      hot_col(col = "Unit", colWidths = 50) %>%
      hot_col(col = "Min", colWidths = 50) %>%
      hot_col(col = "Initial", colWidths = 50) %>%
      hot_col(col = "Max", colWidths = 50) %>%
      hot_col(
        col = "Scale",
        type = "dropdown",
        source = scales(),
        colWidths = 60
      ) %>%
      hot_col(col = "Variability", colWidths = 0.1) %>% # Hide Variability
      hot_col(
        col = ifelse(
          input$platformInput != "Berkeley Madonna",
          c("Min", "Initial", "Max"),
          "Initial"
        ),
        renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          // Apply scientific notation for number x if x!=0 & |x| > 1e4 | |x| < 1e-2
          let str;
          if ( typeof value === 'number') {
            value = +value;
            if ( value !== 0 && (Math.abs(value) > 1e4 || Math.abs(value) < 1e-2)) {
              str = value.toExponential();
            } else {
              str = value;
            }
          } else {
            str = value;
          }
          td.innerHTML = str;
          return td;
        }"
      ) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot;
        $('a[data-value=\"Parameters\"').on('click', function( ){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    # Adjust based upon software platform
    if ( input$platformInput == "NONMEM") {
      tmp <- tmp %>%
        hot_col(
          col = "Fixed",
          type = "dropdown",
          source = c("Yes", "No"),
          colWidths = 50
        )
    }

    tmp

  })

  outputOptions(output, "parameterTable", suspendWhenHidden = FALSE)

  output$parameterTableUI <- renderUI({
    fluidRow(
      col_12( rHandsontableOutput("parameterTable") )
    )
  })

  parameterTable_content <- reactive({
    if ( is.null(input$parameterTable) | length(input$parameterTable$data) == 0) {
      return(NULL)
    } else {
      hot_to_r(input$parameterTable)
    }
  })

  output$parameterUI <- renderUI({

    req( input$pkInput, input$pdInput )

    if ( input$platformInput == 'NONMEM' & input$pdInput != 'logistic' & input$pdInput != 'ordcat' ){
      muBtn <- col_3(
        radioButtons(
          inputId = "muInput",
          label = "MU referencing",
          choices = c("Yes" = TRUE, "No" = FALSE),
          selected = FALSE,
          inline = TRUE
        )
      )
    } else {
      muBtn <- NULL
    }

    # Number of custom PK parameters
    if ( isPKpred() | input$pkInput == 'ode' | input$pkInput == 'linmat' ) {
      nPK <- col_3(
        numericInput(
          inputId = "nPKParmInput",
          width = "100%",
          label = "Additional PK parameters",
          value = 1,
          min = 1,
          step = 1
        )
      )
    } else {
      nPK <- NULL
    }

    # Number of additional PD parameters
    if ( input$pdInput == 'pred' | input$pdInput == 'ode' ) {
      nPD <- col_3(
        numericInput(
          inputId = "nPDParmInput",
          width = "100%",
          label = "Additional PD parameters",
          value = 1,
          min = 1,
          step = 1
        )
      )
    } else {
      nPD <- NULL
    }

    #

    fluidRow(
      col_12(
        fluidRow(
          # MU referencing
          muBtn,
          # Number of custom PK parameters
          nPK,
          # Number of additional PD parameters
          nPD,
          # Number of other parameters
          col_3(
            numericInput(
              inputId = "nOTParmInput",
              width = "100%",
              label = "Additional parameters",
              value = 0,
              min = 0,
              step = 1
            )
          )
        ),
        fluidRow(
          col_12(
            uiOutput("parameterTableUI")
          )
        )
      )
    )

  })

  outputOptions(output, "parameterUI", suspendWhenHidden = FALSE)

  parameterWarnings <- reactive({

    if ( is.null(input$parameterTable) | length(input$parameterTable$data) == 0 ){
      NULL
    } else {
      parms <- hot_to_r(input$parameterTable)

      # Check for duplicate parameters
      dupParms <- unique( parms[duplicated(parms$Parameter), "Parameter" ] )
      if ( length(dupParms) > 0 ){
        return(
          glue::glue(
            paste(
              "The parameter table includes duplicates ({vars}). Edit the parameter",
              "names or modify the parameterization of the PK or PD models."
            ),
            vars = paste(dupParms, collapse = ", ")
          )
        )
      }

      # Check for character initials
      if ( any( is.na( suppressWarnings( as.numeric(parms$Initial) ) ) ) ) {
        return(
          "Initial values must be numeric."
        )
      }

      if ( any(tolower(parms$Min) == '+inf') ) {
        return(
          "Minimum bounds cannot be set to +INF."
        )
      }

      if ( any(tolower(parms$Max) == '-inf') ) {
        return(
          "Maximum bounds cannot be set to -INF."
        )
      }

      mins <- parms$Min[ tolower(parms$Min) != "-inf" ]
      maxs <- parms$Max[ tolower(parms$Max) != "+inf" ]

      if ( any( is.na( suppressWarnings( as.numeric(mins) ) ) ) ) {
        return(
          "Lower boundaries must be numeric or -INF."
        )
      }
      if ( any( is.na( suppressWarnings( as.numeric(maxs) ) ) ) ){
        return(
          "Upper boundaries must be numeric or +INF."
        )
      }

      parms <- parms %>%
        dplyr::mutate(
          Min = as.numeric( sub('INF', 'Inf', Min) ),
          Initial = as.numeric( sub('INF', 'Inf', Initial) ),
          Max = as.numeric( sub('INF', 'Inf', Max) ),
          minmax_check = Min >= Max,
          initial_check = Min >= Initial | Initial >= Max,
          logit_check = ifelse(
            Scale == "Logit",
            Min == -Inf | Max == Inf,
            FALSE
          )
        )

      if ( any(parms$minmax_check) ){
        return(
          "Lower boundaries must be lower than upper boundaries."
        )
      }
      if ( any(parms$initial_check) ){
        return(
          "Initial values must be strictly between the lower and upper boundaries."
        )
      }
      if ( any(parms$logit_check) ){
        return(
          "Boundaries must be finite for parameters defined on the logit scale."
        )
      }
    }
  })

  output$parmInfoUI <- renderUI({
    if ( is.null(input$parameterTable) | length(input$parameterTable$data) == 0 ){
      NULL
    } else {
      parms <- hot_to_r(input$parameterTable)

      text <- "All initial values and boundaries must be provided on the linear scale. Transformations to the log or logit scale will be automatically applied, whenever applicable."

      if ( any( parms$Scale == 'Logit' ) & input$platformInput == 'NONMEM' ){
        text <- c(
          text,
          "If required for the calculation of magnitude of variability, the inv_logit function is defined as follows:",
          "inv_logit <- function(x, min = 0, max = 1){ min + (max-min)/(1+exp(-x)) }"
        )
      }

      if ( any( parms$Scale != 'Linear' ) ){
        message_box(
          text = text,
          icon = "info-circle-fill",
          theme = "info"
        )
      } else {
        NULL
      }

    }
  })

  output$duplicateParmWarningUI <- renderUI({

    if ( !isTruthy(parameterWarnings()) ) {
      NULL
    } else {
      message_box(
        text = parameterWarnings(),
        icon = "cone-striped",
        theme = "warning"
      )
    }
  })

  output$importParameterUI <- renderUI({

    if ( input$platformInput != 'mrgsolve' | notTruthy(input$pkInput, input$pdInput) ){
      return(NULL)
    }

    fluidRow(
      col_3(
        radioButtons(
          inputId = "posthocInput",
          label = "Include POSTHOC estimates",
          choices = c("Yes", "No"),
          selected = "No",
          inline = TRUE
        )
      ),
      col_3(
        radioButtons(
          inputId = "nmextInput",
          label = "Import NONMEM estimates",
          choices = c("Yes", "No"),
          selected = "No",
          inline = TRUE
        )
      ),
      col_6(
        conditionalPanel(
          condition = "input.nmextInput == 'Yes'",
          shinyFiles::shinyDirButton(
            id = "nmextDirChoose",
            title = "Select NONMEM run directory",
            label = "Select NONMEM run directory",
            multiple = FALSE,
            style = "margin-bottom: 10px;"
          )
        )
      )
    )

  })

  # Model directory button backend
  shinyFiles::shinyDirChoose(
    input,
    "nmextDirChoose",
    roots = c(root = "/"),
    allowDirCreate = FALSE
  )

  nmextDirReactive <- reactive(input$nmextDirChoose)

  output$nmextDir <- renderText({
    req( nmextDirReactive(), "path" %in% names(nmextDirReactive()))
    normalizePath(
      shinyFiles::parseDirPath(c(root = "/"), nmextDirReactive())
    )
  })

  #---- Variance tab ----

  output$varianceWarningUI <- renderUI({

    if ( notTruthy(input$pkInput, input$pdInput) ){
      fluidRow(
        col_12(
          message_box(
            text = "No model structure defined",
            icon = "info-circle-fill",
            theme = "info"
          )
        )
      )
    }

  })

  varianceDF <- reactive({

    if ( input$platformInput == "Berkeley Madonna") {
      return(NULL)
    }

    req( input$parameterTable, input$new_menu == "Covariance" )

    # Capture cases when parameter table contains duplicate
    parameterTable <- hot_to_r(input$parameterTable) %>%
      dplyr::mutate(
        Variability = factor(
          dplyr::case_when(
            .data$Variability == 0 ~ "None",
            .data$Variability == 1 ~ "Additive",
            .data$Variability == 2 ~ "Exponential",
            .data$Variability == 3 ~ "Logit",
            TRUE ~ "NA"
          ),
          levels = c("None", "Additive", "Exponential", "Logit"),
          ordered = TRUE
        )
      )
    parms <- parameterTable$Parameter
    if ( length( unique(parms[duplicated(parms)]) ) != 0 ){
      return(NULL)
    }

    # Get parameters and variability info
    DF <- parameterTable %>%
      dplyr::select(.data$Variability, .data$Parameter)
    row.names(DF) <- DF$Parameter

    ### Check content of input$varianceTable and preserve custom inputs
    if ( length(isolate(input$varianceTable)) > 0 )
    {
      oDF <- hot_to_r(isolate(input$varianceTable))
      if ( nrow(DF) == 0 ){
        DF <- oDF
      }
      if ( !identical(DF, oDF)) {

        mDF <- merge(
          cbind(
            DF,
            data.frame("_SORT_" = 1:nrow(DF))
          ),
          oDF,
          by = "Parameter",
          all.x = TRUE
        )
        mDF <- mDF[order(mDF[, "X_SORT_"]), ]

        for (col in names(oDF)[-2] ){
          DF[, col] <- ifelse(
            is.na(mDF[,paste(col, "y", sep = ".")]),
            mDF[, paste(col, "x", sep = ".")],
            mDF[, paste(col, "y", sep = ".")]
          )
          if ( is.factor(oDF[, col]) ){
            # ifelse coerces factors to integers, must reset to factor
            DF[, col] <- factor(levels(oDF[, col])[DF[, col]], levels = levels(oDF[, col]), ordered = TRUE)
          }
        }
      }
    }

    DF

  })

  output$varianceTable <- renderRHandsontable({

    req( varianceDF )

    DF <- varianceDF()

    rhandsontable(
      data = DF,
      contextMenu = FALSE,
      manualColumnMove = FALSE,
      manualRowMove = TRUE,
      width = 150,
      height = max(200, (nrow(DF) + 1)*25 + 10)  # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(
        col = "Parameter",
        readOnly = TRUE,
        colWidths = 0.1
      ) %>%
      hot_col(
        col = "Variability",
        colWidths = 100,
        type = "dropdown",
        source = c("None", "Additive", "Exponential", "Logit")
      ) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot;
        $('a[data-value=\"Covariance\"').on('click', function( ){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

  })

  outputOptions(output, "varianceTable", suspendWhenHidden = FALSE)

  output$varianceTableUI <- renderUI( {rHandsontableOutput("varianceTable")} )

  varianceTable_content <- reactive({
    if ( is.null(input$varianceTable) | length(input$varianceTable$data) == 0) {
      return(NULL)
    } else {
      hot_to_r(input$varianceTable)
    }
  })

  #---- Covariance ----

  output$covarianceTable <- renderRHandsontable({

    if ( input$platformInput == "Berkeley Madonna") {
      return(NULL)
    }

    req( varianceTable_content(), input$new_menu == "Covariance" )
    varianceTable <- varianceTable_content()

    # Create diagonal matrix (default variance is 0.2) as linear array
    n <- nrow(varianceTable)
    if ( length(isolate(input$covarianceTable)) > 0 && nrow(hot_to_r(isolate(input$covarianceTable))) == n) {
      DF <- as.matrix(
        hot_to_r( isolate(input$covarianceTable) )
      )
    } else {
      DF <- diag(n)*0.2
    }

    # Get the list of parameters without variability
    if ( any(varianceTable$Variability == "None") ){
      novar_rows <- which(varianceTable$Variability == "None")
    } else {
      novar_rows <- NULL
    }

    # Make value adjustments
    for ( i in 1:n ) {
      for ( j in 1:n ) {
        # Set diagonal and lower elements to 0 based upon parameters without variability
        if ( i %in% novar_rows | j %in% novar_rows ){
          DF[i, j] <- 0
        }
        # Change upper triangle cells into NA
        if ( j > i ){
          DF[i, j] <- NA
        }
      }
    }

    # Convert to data.frame
    DF <- data.frame(DF)
    names(DF) <- rownames(DF) <- varianceTable$Parameter

    # Create the rhandsontable object
    tmp <- rhandsontable(
      data = DF,
      width = "100%",
      height = (nrow(DF) + 1)*25 + 10
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_validate_numeric(cols = 1:nrow(DF), min = 0)

    # Lock cells
    locked <- "tmp"
    for ( i in 1:n ){
      if ( any(varianceTable$Variability == "None") ){
        min_novar_row <- novar_rows[novar_rows < i]
        min_novar_row <- rev(min_novar_row)[1]
      } else {
        min_novar_row <- NA
      }
      for ( j in 1:n ){
        lock <- FALSE
        # Lock cells from upper triangle
        if ( j > i) lock <- TRUE
        # Lock cells based upon parameters without variability
        if ( i %in% novar_rows | j %in% novar_rows) lock <- TRUE
        # Lock off-diagonal elements based upon parameters without variability
        if ( !is.na(min_novar_row) && (i > min_novar_row & j < min_novar_row) ) lock <- TRUE
        if ( lock ){
          locked <- paste(
            locked,
            glue::glue( "%>%\n  hot_cell({i}, {j}, readOnly = TRUE)" )
          )
        }
      }
    }

    tmp <- eval(parse(text = locked))

    tmp %>%
      hot_cols(renderer = "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        if ( cellProperties.readOnly == true) {
          td.style.background = '#eee';
          td.style.color = '#aaa';
        }
        // Apply scientific notation for number x if x!=0 & |x| > 1e4 | |x| < 1e-2
        let str;
        if ( typeof value === 'number') {
          value = +value;
          if ( value !== 0 && (Math.abs(value) > 1e4 || Math.abs(value) < 1e-2)) {
            str = value.toExponential();
          } else {
            str = value;
          }
        } else {
          str = value;
        }
        td.innerHTML = str;
        return td;
      }"
      ) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot;
        $('a[data-value=\"Covariance\"').on('click', function( ){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

  })

  outputOptions(output, "covarianceTable", suspendWhenHidden = FALSE)

  output$covarianceTableUI <- renderUI( {rHandsontableOutput("covarianceTable")} )

  # Process covariance matrix and check if there are errors
  covarianceBlocks <- reactive({

    chk1 <- chk2 <- chk3 <- chk4 <- chk5 <- TRUE
    blocks <- NULL

    if ( isTruthy(input$covarianceTable) ){

      covarianceTable <- as.matrix(hot_to_r(input$covarianceTable))

      req(
        identical(
          row.names( covarianceTable ),
          row.names( varianceTable_content() )
        )
      )

      covarianceTable <- as.matrix(hot_to_r(input$covarianceTable))

      if ( all( covarianceTable == 0, na.rm = TRUE ) ){
        return(
          list(
            chk1 = chk1, chk2 = chk2, chk3 = chk3, chk4 = chk4,
            blocks = list(
              list(
                omega = covarianceTable,
                type = 'diagonal'
              )
            )
          )
        )
      }

      ## Check 1: detect  if variance is set to 0 on a parameter with variability
      n <- nrow(covarianceTable)
      diag(covarianceTable)[is.na(diag(covarianceTable))] <- 0

      chk1 <- !any(
        diag(covarianceTable) == 0 &
          isolate(varianceTable_content())$Variability != "None"
      )

      ## Check 2: detect illegal 0's between non-0 covariance terms

      # Get correlation "table" as matrix of ones and zeros, and NA's in upper triangle
      correlationTable <- get_correlation_table(
        x = covarianceTable
      )

      # Check that each line is made of ones or a series of zeros followed by ones
      check1 <- t(apply(correlationTable, 1, cumsum))
      check1 <- unlist(
        apply(check1, 1, function(x) {as.vector(table(x[x!=0]) > 1)  })
      )
      chk2 <- all(!check1)

      ## Check 3: detect rows of parameter without correlation with others and
      # check that the corresponding columns also contains just one 1
      if ( !chk2 ){
        chk3 <- FALSE
        blocks <- NULL
      } else {
        # Get correlation "table" as matrix of ones and zeros
        correlationTable <- get_correlation_table(
          x = covarianceTable,
          na_zero = TRUE
        )
        rowSums <- apply(correlationTable, 1, sum)
        colSums <- apply(correlationTable, 2, sum)

        isRowDiagonal <- sapply(
          1:n,
          function(i,rowSums, n ){
            ifelse(i<n, rowSums[i] == 1 & rowSums[i+1] <= 1, rowSums[i] == 1)
          },
          rowSums,
          n
        )
        chk3 <- all(rowSums[isRowDiagonal] == colSums[isRowDiagonal])
      }

      ## Check 4: process covariance matrix sub-blocks
      if ( !chk2 | !chk3 |
           ( nrow(covarianceTable) != nrow(hot_to_r(input$varianceTable)) )
      ){
        chk4 <- FALSE
        blocks <- NULL
      } else {
        # Process isRowDiagonal info
        if ( length(isRowDiagonal) > 0 && all(isRowDiagonal) ){
          # Covariance matrix is diagonal
          chk4 <- TRUE
          blocks =  list(
            list(
              omega = covarianceTable,
              type = "diagonal"
            )
          )
        } else {
          # Covariance matrix is NOT diagonal - detect
          # tmp contains zero's for zero off-diagonal elements and upper triangle
          #              NA's for zero diagonal elements
          #              one's otherwise
          n <- nrow(covarianceTable)
          tmp <- matrix(0, ncol = n, nrow = n)
          tmp[lower.tri(tmp, diag = TRUE)] <- 1 - correlationTable[lower.tri(tmp, diag = TRUE)]
          diag(tmp) <- ifelse(
            diag(covarianceTable) == 0,
            NA,
            diag(correlationTable)
          )

          # A block is marked by changes in blockCheck0
          blockCheck0 <- apply(tmp, 1, sum) == (1:n)
          blockCheck1 <- rep(NA, n)
          cnt <- 0
          for ( irow in 1:n ){
            if ( is.na(blockCheck0[irow]) ){
              next
            }
            if ( blockCheck0[irow] == TRUE ){
              if ( irow == 1 ){
                cnt <- cnt + 1
              } else if ( irow <= n &  !identical(blockCheck0[irow], blockCheck0[irow-1]) ){
                cnt <- cnt + 1
              } else if ( irow < n & !is.na(blockCheck0[irow+1]) &!identical(blockCheck0[irow], blockCheck0[irow+1]) ){
                cnt <- cnt + 1
              }
            }
            blockCheck1[irow] <- cnt
          }

          # Get block start and end
          blocks <- vector("list", max(blockCheck1, na.rm = TRUE))
          for ( iblock in 1:length(blocks) ){
            matches <- match(blockCheck1, iblock)
            if ( length(matches) > 0 ){
              minIndex <- min( which(!is.na(matches)) )
              maxIndex <- max( which(!is.na(matches)) )
              omega <- covarianceTable[minIndex:maxIndex, minIndex:maxIndex, drop = FALSE]
              blocks[[iblock]] <- list(
                omega = omega,
                type = is_EDB(omega)
              )
            }
          }

          chk4 <- all(sapply(blocks, function(x) x$type != "error"))

        }
      }

      ## Check 5: check variability model
      chk5 <- hot_to_r(input$parameterTable) %>%
        dplyr::left_join(
          hot_to_r(input$varianceTable),
          by = 'Parameter'
        ) %>%
        dplyr::mutate(
          invalid = case_when(
            Scale == 'Log' & Variability.y == 'Logit' ~ TRUE,
            Scale == 'Logit' & Variability.y != 'Logit' ~ TRUE,
            TRUE ~ FALSE
          )
        ) %>%
        filter( invalid == TRUE ) %>%
        pull( Parameter )


    }

    list(chk1 = chk1, chk2 = chk2, chk3 = chk3, chk4 = chk4, chk5 = chk5, blocks = blocks)

  })

  ## Dynamic UI for misspecified covariance matrix
  output$covarianceWarningUI <- renderUI({

    if ( all(c(covarianceBlocks()$chk1, covarianceBlocks()$chk2, covarianceBlocks()$chk3, covarianceBlocks()$chk4, length(covarianceBlocks()$chk5) == 0 ) ) ){
      NULL
    } else {
      tagList(
        if ( !covarianceBlocks()$chk1 ) {
          message_box(
            text = "Variance cannot be set to 0 for a parameter with estimated variance",
            icon = "cone-striped",
            theme = "warning"
          )
        },
        if ( any(!c(covarianceBlocks()$chk2, covarianceBlocks()$chk3, covarianceBlocks()$chk4)) ) {
          message_box(
            text = paste(
              "The covariance matrix must be constructed as a series of diagonal,",
              "band, or full block matrices. Correlation will be ignored in",
              "the model."
            ),
            icon = "cone-striped",
            theme = "warning"
          )
        },
        if ( length( covarianceBlocks()$chk5 ) > 0 ) {
          message_box(
            text = paste(
              "Invalid variability selection for:",
              paste(covarianceBlocks()$chk5, collapse = ", ")
            ),
            icon = "cone-striped",
            theme = "warning"
          )
        }
      )
    }

  })

  output$varianceUI <- renderUI({

    req( input$pkInput, input$pdInput)

    if ( input$platformInput == "Berkeley Madonna") {
      return(
        message_box(
          text = "Variance-covariance settings are not available for the selected software platform",
          icon = "info-circle-fill",
          theme = "info"
        )
      )
    }

    if ( notTruthy(input$parameterTable) ){
      return(
        message_box(
          text = "No parameters defined",
          icon = "info-circle-fill",
          theme = "info"
        )
      )
    }


    tagList(
      fluidRow(
        col_3(
          h4(strong("Variance")),
          uiOutput("varianceTableUI")
        ),
        col_9(
          h4(strong("Covariance matrix")),
          uiOutput("covarianceTableUI")
        )
      ),
      fluidRow(
        col_12(
          uiOutput("covarianceWarningUI")
        )
      )
    )

  })

  outputOptions(output, "varianceUI", suspendWhenHidden = FALSE)

  #---- Residual variability ----

  output$residualWarningUI <- renderUI({

    if ( input$platformInput == "Berkeley Madonna" ) {
      return(
        message_box(
          text = "Residual variability cannot be defined for the selected software platform",
          icon = "info-circle-fill",
          theme = "info"
        )
      )
    }

    if ( notTruthy(input$pkInput, input$pdInput) ){
      fluidRow(
        message_box(
          text = "No model structure defined",
          icon = "info-circle-fill",
          theme = "info"
        )
      )
    }

  })

  output$rvUI <- renderUI({

    req( input$pkInput, input$pdInput)

    if ( input$platformInput == "Berkeley Madonna" ) {
      NULL
    } else {
      fluidRow(
        if ( input$pkInput != "none" ){
          col_6(
            h4(strong("Residual variability for PK")),
            selectInput(
              inputId = "pkRVInput",
              width = "100%",
              label = NULL,
              choices = c(
                "None" = "none",
                "Additive" = "add",
                "Constant CV" = "ccv",
                "Additive + Constant CV" = "accv",
                "Logarithmic" = "log"
              ),
              selected = "ccv"
            )
          )
        },
        if ( input$pdInput != "none" ){
          col_6(
            h4(strong("Residual variability for PD")),
            selectInput(
              inputId = "pdRVInput",
              width = "100%",
              label = NULL,
              choices = c(
                "None" = "none",
                "Additive" = "add",
                "Constant CV" = "ccv",
                "Additive + Constant CV" = "accv",
                "Logarithmic" = "log"
              ),
              selected = ifelse(
                input$pdInput %in% c("logistic", "ordcat"),
                "none",
                "ccv"
              )
            )
          )
        }
      )
    }

  })

  outputOptions(output, "rvUI", suspendWhenHidden = FALSE)

  pkRVInput <- reactive({

    if ( isTruthy(input$pkInput) && input$pkInput != "none" ){
      input$pkRVInput
    } else {
      "none"
    }

  })

  pdRVInput <- reactive({

    if ( isTruthy(input$pdInput) && input$pdInput != "none" ){
      input$pdRVInput
    } else {
      "none"
    }

  })

  # Dynamic RV table UI

  rvTable_input_content <- reactive({

    req( input$pkInput, input$pdInput, pkRVInput(), pdRVInput() )

    pkRV <- ifelse(
      input$pkInput != "none",
      pkRVInput(),
      pkRV <- "none"
    )
    pdRV <- ifelse(
      input$pdInput != "none",
      pdRVInput(),
      pdRV <- "none"
    )

    req( pkRV != "none" | pdRV != "none" )

    DF <- data.frame(
      Type = c(
        rep(
          "PK",
          switch(pkRV, "none" = 0, "add" = 1, "ccv" = 1, "accv" = 2, "log" = 1)
        ),
        rep(
          "PD",
          switch(pdRV, "none" = 0, "add" = 1, "ccv" = 1, "accv" = 2, "log" = 1)
        )
      ),
      Label = c(
        switch(
          pkRV,
          "none" = NULL,
          "add" = c("Additive"),
          "ccv" = c("Constant CV"),
          "accv" = c("Constant CV", "Additive"),
          "log" = c("Additive (log)")
        ),
        switch(
          pdRV,
          "none" = NULL,
          "add" = c("Additive"),
          "ccv" = c("Constant CV"),
          "accv" = c("Constant CV", "Additive"),
          "log" = c("Additive (log)")
        )
      ),
      Variance = c(
        switch(
          pkRV,
          "none" = NULL,
          "add" = c(1),
          "ccv" = c(0.2),
          "accv" = c(0.2, 1),
          "log" = c(1)
        ),
        switch(
          pdRV,
          "none" = NULL,
          "add" = c(1),
          "ccv" = c(0.2),
          "accv" = c(0.2, 1),
          "log" = c(1)
        )
      ),
      stringsAsFactors = FALSE
    )

    ### Check content of input$rvTable and preserve custom inputs
    if ( length(isolate(input$rvTable)) > 0 ){
      oDF <- hot_to_r(isolate(input$rvTable))
      if ( nrow(DF) == 0 ){
        DF <- oDF
      }
      if ( !identical(DF, oDF)) {
        mDF <- merge(
          cbind(DF, data.frame("_SORT_" = 1:nrow(DF)) ),
          oDF,
          by = c("Type", "Label"),
          all.x = TRUE
        )
        mDF <- mDF[order(mDF[, "X_SORT_"]), ]
        DF[, 3] <- ifelse(
          is.na(mDF[, "Variance.y"]),
          mDF[, "Variance.x"],
          mDF[, "Variance.y"]
        )
      }
    }

    DF

  })

  # RV table
  output$rvTable <- renderRHandsontable({

    req( rvTable_input_content())

    DF <- rvTable_input_content()

    tmp <- rhandsontable(
      data = DF,
      rowHeaders = FALSE,
      contextMenu = FALSE,
      manualColumnMove = FALSE,
      manualRowMove = TRUE,
      width = "100%",
      height = (nrow(DF) + 1)*25 + 10
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(col = 1:2, readOnly = TRUE) %>%
      hot_validate_numeric(cols = 3, min = 0)  %>%
      hot_col(
        col = "Variance",
        renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          // Apply scientific notation for number x if x!=0 & |x| > 1e4 | |x| < 1e-2
          let str;
          if ( typeof value === 'number') {
            value = +value;
            if ( value !== 0 && (Math.abs(value) > 1e4 || Math.abs(value) < 1e-2)) {
              str = value.toExponential();
            } else {
              str = value;
            }
          } else {
            str = value;
          }
          td.innerHTML = str;
          return td;
        }"
      ) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot;
        $('a[data-value=\"RV\"').on('click', function( ){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    tmp

  })

  outputOptions(output, "rvTable", suspendWhenHidden = FALSE)

  output$rvTableUI <- renderUI({

    req( input$pkInput, input$pdInput, pkRVInput(), pdRVInput() )
    req( input$platformInput != 'Berkeley Madonna' )

    if (
      (input$pkInput != "none" & pkRVInput() != "none") |
      (input$pdInput != "none" & pdRVInput() != "none")
    ) {
      fluidRow(
        col_12(
          h4(strong("Residual variability parameters")),
          rHandsontableOutput("rvTable")
        )
      )
    }

  })

  # Process RV matrix
  rvCheck <- reactive({

    req( input$pkInput, input$pdInput, pkRVInput(), pdRVInput())

    if ( is.null(input$rvTable) | length(input$rvTable$data) == 0) {
      return(list(isOK = TRUE))
    } else {
      # Find if variance is set to 0 on a parameter with variability
      rvTable <- as.matrix(hot_to_r(input$rvTable))
      if ( any(is.na(rvTable[,3]) | rvTable[,3] == 0) ) {
        return(list(isOK = FALSE))
      } else {
        return(list(isOK = TRUE))
      }
    }

  })

  ## Dynamic UI for erroneous  RV matrix
  output$rvWarningUI <- renderUI({
    if ( rvCheck()$isOK ){
      NULL
    } else {
      message_box(
        text = "Variance cannot be set to 0 for an estimated RV variance",
        icon = "cone-striped",
        theme = "warning"
      )
    }
  })

  output$rvFlagUI <- renderUI({

    req( input$pkInput, input$pdInput, pkRVInput(), pdRVInput() )

    req( input$platformInput == "NONMEM" )

    fluidRow(
      if ( pkRVInput() %in% c("ccv", "log") | pdRVInput() %in% c("ccv", "log") ){
        col_6(
          radioButtons(
            inputId = "flagF0Input",
            width = "100%",
            label = "Include flag for cases when F = 0?",
            inline = TRUE,
            choices = c("Yes" = TRUE, "No" = FALSE),
            selected = FALSE
          )
        )
      },
      col_6(
        radioButtons(
          inputId = "blqInput",
          width = "100%",
          label = "Use Beal's M3 method for BLQ data?",
          inline = TRUE,
          choices = c("Yes" = TRUE, "No" = FALSE),
          selected = FALSE
        )
      )
    )

  })

  #---- Tasks ----

  output$estimationTable <- renderRHandsontable({

    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }

    req( input$pdInput )

    DF <- data.frame(
      Step = as.character(1:5),
      Method = c("FOCE", rep("none", 4)),
      Interaction = c("Yes", rep("", 4)),
      Likelihood = c("No", rep("", 4)),
      NoPrediction = c("No", rep("", 4)),
      Options = rep("", 5),
      NSIG = c(3, rep(NA, 4)),
      stringsAsFactors = FALSE
    )

    if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ){
      DF$Options[1] <- "LAPLACE"
    }

    if ( input$pdInput %in% c("logistic", "ordcat") ){
      DF <- data.frame(
        Step = as.character(1:5),
        Method = c("FOCE", rep("none", 4)),
        Interaction = c("No", rep("", 4)),
        Likelihood = c("Yes", rep("", 4)),
        NoPrediction = c("No", rep("", 4)),
        Options = c("LAPLACE", rep("", 4)),
        NSIG = c(3, rep(NA, 4)),
        stringsAsFactors = FALSE
      )
    }

    tmp <- rhandsontable(
      data = DF,
      rowHeaders = NULL,
      contextMenu = FALSE,
      width = "100%",
      height = 160         # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(
        col = "Method",
        type = "dropdown",
        source = c("none", "FO", "FOCE", "ITS", "IMP", "SAEM", "BAYES")
      ) %>%
      hot_col(
        col = "Interaction",
        type = "dropdown",
        source = c("", "Yes", "No")
      ) %>%
      hot_col(
        col = "Likelihood",
        type = "dropdown",
        source = c("", "Yes", "No")
      ) %>%
      hot_col(
        col = "NoPrediction",
        type = "dropdown",
        source = c("", "Yes", "No")
      ) %>%
      hot_col(
        col = "NSIG",
        type = "numeric",
        format = "1a"
      ) %>%
      hot_col(col = "Step", readOnly = TRUE)  %>%
      hot_validate_numeric(
        cols = "NSIG",
        min = 1, max = 10
      ) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot;
        $('a[data-value=\"Tasks\"').on('click', function( ){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    tmp

  })

  output$estimationTableUI <- renderUI({

    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }

    rHandsontableOutput("estimationTable")

  })

  output$estimationUI <- renderUI({
    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }
    checkboxInput(
      inputId = "estimationInput",
      label = "Perform estimation(s)",
      value = TRUE
    )
  })

  output$covarianceEstimationUI <- renderUI({
    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }
    req( input$estimationInput )
    checkboxInput(
      inputId = "covarianceInput",
      label = "Perform covariance step",
      value = TRUE
    )
  })

  output$simulationUI <- renderUI({
    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }
    checkboxInput(
      inputId = "simulationInput",
      label = "Perform simulation(s)",
      value = FALSE
    )
  })

  output$nsimUI <- renderUI({
    req(input$simulationInput)
    numericInput(
      inputId = "nsubInput",
      label = "Number of simulations",
      width = "100%",
      min = 1,
      step = 1,
      value = 1
    )
  })

  output$seedUI <- renderUI({
    req(input$simulationInput)
    numericInput(
      inputId = "simulationSeedInput",
      label = "Seed number",
      width = "100%",
      min = 1,
      step = 1,
      value = round(100000 * signif(stats::runif(1), 5),0)
    )
  })

  output$taskUI <- renderUI({

    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        fluidRow(
          message_box(
            text = "No model structure defined",
            icon = "info-circle-fill",
            theme = "info"
          )
        )
      )
    }

    tagList(
      h4(strong("Select tasks to be performed")),
      fluidRow(
        col_6(
          uiOutput("estimationUI")
        ),
        col_6(
          uiOutput("covarianceEstimationUI")
        )
      ),
      fluidRow(
        col_4(
          uiOutput("simulationUI")
        ),
        col_4(
          uiOutput("nsimUI")
        ),
        col_4(
          uiOutput("seedUI")
        )
      ),
      conditionalPanel(
        condition = "input.estimationInput",
        fluidRow(
          col_12(
            uiOutput("estimationTableUI")
          )
        )
      )
    )

  })

  #---- Scaling ----

  output$mmUI <- renderUI({

    req( input$platformInput, input$pkInput, input$doseUnitInput)

    if ( input$platformInput %in% c("NONMEM", "mrgsolve") & input$pkInput != "none" ){
      doseUnit <- input$doseUnitInput
      concentrationUnit <- unlist(strsplit(input$cpUnitInput, split = "[/]"))[1]

      if ( grepl("g", doseUnit) != grepl("g", concentrationUnit) ){
        numericInput(
          inputId = "mmInput",
          label = "Molecular Mass",
          min = 0,
          value = 100,
          step = 0.1
        )
      } else {
        NULL
      }
    }

  })

  output$scalingUI <- renderUI({

    req( input$platformInput)

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        fluidRow(
          message_box(
            text = "No model structure defined",
            icon = "info-circle-fill",
            theme = "info"
          )
        )
      )
    }

    if ( input$platformInput %in% c("NONMEM", "mrgsolve") & input$pkInput != "none" ){
      tagList(
        fluidRow(
          col_4(
            selectInput(
              inputId = "doseUnitInput",
              label = "Dose unit",
              choices = c("g", "mg", "ug", "ng", "pg", "mol", "mmol", "umol", "nmol", "pmol"),
              selected = "mg"
            ),
            uiOutput("mmUI")
          ),
          col_4(
            selectInput(
              inputId = "volumeUnitInput",
              label = "Volume unit",
              choices = c("L", "mL", "uL"),
              selected = "L"
            )
          ),
          col_4(
            selectInput(
              inputId = "cpUnitInput",
              label = "Concentration unit",
              choices = list(
                "Common" = c("ng/mL", "ng/L","mmol/L", "umol/L", "nmol/L"),
                "Other" = c("g/L", "g/mL", "g/uL", "mg/L", "mg/mL", "mg/uL",
                            "ug/L", "ug/mL", "ug/uL", "ug/mL", "ng/uL", "pg/L", "pg/mL", "pg/uL",
                            "mol/L", "pmol/L")
              ),
              selected = "ng/mL"
            )
          )
        )
      )
    }

  })

  #---- Ace toolbar ----

  output$copyBtn <- renderUI({
    bslib::tooltip(
      rclipboard::rclipButton(
        inputId = "copyButton",
        label = NULL,#"Copy to clipboard",
        clipText = input$aceNew,
        icon = icon("copy")
      ),
      "Copy",
      options = list(delay =list(show=800, hide=100))
    )
  })
  output$aceToolbarUI <- renderUI({
    fluidRow(
      col_12(
        bslib::tooltip(
          shinyBS::bsButton(
            inputId = "lockButton",
            icon = icon("lock-open"),
            label = NULL,
            block = FALSE,
            type = "toggle",
            value = FALSE
          ),
          "Lock/unlock",
          options = list(delay =list(show=800, hide=100))
        ),
        bslib::tooltip(
          actionButton(
            inputId = "refreshButton",
            label = NULL,#"(Re)generate",
            icon = icon("sync")
          ),
          "Refresh",
          options = list(delay =list(show=800, hide=100))
        ),
        uiOutput('copyBtn', style = 'display: inline-block;'),
        bslib::tooltip(
          downloadButton(
            outputId = "downloadButton",
            label = NULL,#"Download",
            icon = icon("download")
          ),
          "Download",
          options = list(delay =list(show=800, hide=100))
        ),
        bslib::tooltip(
          actionButton(
            inputId = "linkButton",
            label = NULL,#"Keyboard shortcuts",
            icon = icon("keyboard"),
            onclick ="window.open('https://github.com/ajaxorg/ace/wiki/Default-Keyboard-Shortcuts', '_blank')"
          ),
          "Keyboard showrtcuts",
          options = list(delay =list(show=800, hide=100))
        )
      )
    )
  })

  observeEvent(
    input$lockButton,
    {
      shinyBS::updateButton(
        session,
        inputId = "lockButton",
        icon = icon(
          ifelse(
            input$lockButton,
            "lock",
            "lock-open"
          )
        )
      )
    }
  )

  output$downloadButton <- downloadHandler(
    filename = function() {
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
      } else {
        model <- paste0(
          paste0("model-", Sys.Date()),
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
      }
      model
    },
    content = function(file) {
      write(input$aceNew, file, sep = '\n')
    }
  )

  #---- Model code ----
  template <- reactive({

    req( input$platformInput )

    if ( input$platformInput == "NONMEM" ){
      template_nonmem
    } else if ( input$platformInput == "mrgsolve" ){
      template_mrgsolve
    } else {
      template_bm
    }

  })

  modelCode <- reactive({

    req( "linkButton" %in% names(input) )

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        get_code(
          input = input,
          template = template,
          vars = mappedVars,
          varianceTable = NULL,
          covarianceBlock = NULL,
          rvTable = NULL
        )
      )
    } else {
      parameterTable <- hot_to_r(input$parameterTable)
      varianceTable <- hot_to_r(input$varianceTable)
      covarianceTable <- hot_to_r(input$covarianceTable)
      rvTable <- hot_to_r(input$rvTable)

      req(
        length(parameterTable) > 0,
        length(varianceTable) > 0,
        length(covarianceTable) > 0
      )

      if ( isTruthy(parameterWarnings()) ){
        return("Invalid parameter definition prevents the code generation.")
      } else if (
        nrow(parameterTable) != nrow(varianceTable) |
        nrow(varianceTable) != nrow(covarianceTable)
      ) {
        return("Inconsistent dimension of parameter and variance/covariance tables prevents the code generation.")
      } else if (
        !all(c(covarianceBlocks()$chk1, covarianceBlocks()$chk2, covarianceBlocks()$chk3, covarianceBlocks()$chk4, length(covarianceBlocks()$chk5) == 0 ) )
      ){
        return("Invalid variability selection prevents the code generation")
      } else {

        get_code(
          input = input,
          template = template,
          vars = mappedVars,
          advan = advan,
          trans = trans,
          isPRED = isPRED,
          isODE = isODE_code,
          isLINMAT = isLINMAT,
          isPREDPP = isPREDPP,
          varianceTable = varianceTable_content(),
          covarianceBlock = covarianceBlocks()$blocks,
          rvTable = rvTable,
          parm_lib = parm_lib,
          model_lib = model_lib,
          rv_lib = rv_lib,
          scaling = scaling,
          replacement = TRUE
        )
      }
    }
  })

  output$newCode <- newCode <- reactive({

    req( input$platformInput )

    dummy <- input$nmFlavorInput

    value <- modelCode()
    if ( length(value) == 0 ){
      ""
    } else {
      paste(value, collapse = "\n")
    }
  })

  outputOptions(output, "newCode", suspendWhenHidden = FALSE)

  observeEvent(
    newCode(),
    {
      if ( isFALSE(input$lockButton) ){
        shinyAce::updateAceEditor(
          session = session,
          editorId = "aceNew",
          value = newCode(),
          mode = ifelse(
            input$platformInput == "NONMEM",
            "nmtran",
            "text"
          ),
          wordWrap = TRUE
        )
      }
    }
  )

  observeEvent(
    input$refreshButton ,
    {
      shinyAce::updateAceEditor(
        session = session,
        editorId = "aceNew",
        value = newCode(),
        mode = ifelse(
          input$platformInput == "NONMEM",
          "nmtran",
          "text"
        ),
        wordWrap = TRUE
      )
    }
  )

}

