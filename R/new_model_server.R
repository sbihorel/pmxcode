#' New model module
#' @name new_model_server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param resources A list of internal resources
#'
#' @import rhandsontable
#' @import excelR

new_model_server <- function(session, input, output, resources){

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
      h4(strong("Data file")),
      fluidRow(
        col_12(
          shinyFiles::shinyFilesButton(
            id = "dataFileChoose",
            title = "Select data file",
            label = "Select data file",
            multiple = FALSE,
            style = "margin-bottom: 10px;"
          )
        )
      ),
      fluidRow(
        col_12(
          verbatimTextOutput("dataFile")
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
    req(modelDirReactive(), "path" %in% names(modelDirReactive()))
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
    req(input$dataFileChoose, "files" %in% names(input$dataFileChoose))

    normalizePath(
      shinyFiles::parseFilePaths(c(root = "/"), input$dataFileChoose)$datapath
    )
  })

  # Get top of the data file
  dataFile <- reactive({

    req(dataFileReactive())

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
      error = function(e){
        structure('error', class = 'try-error')
      },
      warning = function(e){
        structure('warning', class = 'try-error')
      },
      finally = {}
    )
  })

  # Data file content report
  output$dataFile <- renderText({

    req(dataFileReactive())

    text <- sprintf("Data file: %s", dataFileReactive())

    if ( inherits(dataFile(), "try-error") | !inherits(dataFile(), "data.frame") ){
      text <- paste(
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
            "\033",
            "\t",
            gsub(
              "[[]3m\033[[]38;5;246m|[[]39m\033[[]23m",
              "",
              capture.output(pillar::glimpse(dataFile()))
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
    if (input$platformInput != "NONMEM") {
      return(
        HTML_info("Variable mapping is not available for the selected software platform")
      )
    } else {

      if ( inherits(try(dataFileReactive(), silent = TRUE), "try-error") ){
        return(
          HTML_info("No data file was selected")
        )
      }

      if ( inherits(dataFile(), "try-error") ){
        return(
          HTML_info("Invalid data file")
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

    if (input$platformInput != "NONMEM"){
      vars <- NULL
    }

    vars

  })


  # Default content of the table
  mapTableInputContent <- reactive({

    req(dataVars())

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
  output$mapTable <- renderExcel({

    req(dataVars())

    DF <- mapTableInputContent()

    excelTable(
      data = DF,
      columns = data.frame(
        title = c("Description", "Reserved keyword", "Dataset variable"),
        width = c(200, 120, 120),
        type = c("text", "text", "dropdown"),
        source = I(
          list(
            0,
            0,
            dataVars()
          )
        ),
        readOnly = c(TRUE, TRUE, FALSE)
      ),
      autoWidth = FALSE,
      allowInsertRow = FALSE,
      allowDeleteRow = FALSE,
      rowDrag = FALSE,
      allowInsertColumn = FALSE,
      allowDeleteColumn = FALSE,
      allowRenameColumn = FALSE,
      columnSorting = FALSE,
      columnResize = FALSE,
      columnDrag = FALSE
      # update = paste(
      #   "function(instance, td, col, row, value, label, cellName) {",
      #   "  var colName = instance.jexcel.getHeader(col);",
      #   "  if (colName in ['Description', 'Reserved keyword']) {",
      #   "    td.classList.add('readonly') ;",
      #   "  }",
      #   "}",
        # sep = '\n'
      # )
    )

  })
  # output$mapTable <- renderRHandsontable({
  #
  #   req(dataVars())
  #
  #   DF <- mapTableInputContent()
  #
  #   tmp <- rhandsontable(
  #     data = DF,
  #     rowHeaders = FALSE,
  #     colHeaders = c("Description", "Reserved keyword", "Dataset variable"),
  #     contextMenu = FALSE,
  #     manualColumnMove = FALSE,
  #     manualRowMove = FALSE,
  #     #width = 330#,
  #     height = 250  # 25 px per row + 10 for potential scroll bar
  #   ) %>%
  #     hot_table(contextMenu = FALSE) %>%
  #     hot_col(col = 1, colWidths = 200, readOnly = TRUE) %>%
  #     hot_col(col = 2, colWidths = 100, readOnly = TRUE) %>%
  #     hot_col(col = 3, colWidths = 100, type = 'dropdown',
  #             source = dataVars()) %>%
  #     # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
  #     htmlwidgets::onRender("
  #     function(el, x) {
  #       var hot = this.hot
  #       $('#collapse').find('a').click(function() {
  #         setTimeout(function() {hot.render();}, 0);
  #       })
  #     }")
  #
  #   tmp
  #
  # })
  #
  # outputOptions(output, "mapTable", suspendWhenHidden = FALSE)

  output$mapTableUI <- renderUI({

    tagList(
      if ( !is.null(dataVars()) && !all(dataVars() == "") ){
        h4(strong("Dataset variable mapping"))
      },
      #rHandsontableOutput('mapTable')
      excelOutput(
        "mapTable",
        width = 0,  # Hide white frame around excelR table
        height = 0
      )
    )

  })

  # Variables to be dropped

  output$mapDropUI <- renderUI({

    req(dataVars())

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

    req(dataVars())

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

    req(dataVars())

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

    for (i in seq_along(oVars)){
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

    # Add =DROP
    if ( length(matches) > 0 ){
      tmp[matches] <- paste0(tmp[matches], "=DROP")
    }

    gsub("[^[:alnum:]]", "", tmp)

  })

  #---- PK Structure ----

  input_advan_ref <- data.frame(
    CMT = rep(1:3, each=5),
    INPUT = rep(c("bolus", "zero", "first", "sig", "transit"), times = 3),
    ADVAN = c(1, 1, 2, 2, NA, 3, 3, 4, 4, NA, 11, 11, 12, 12, NA),
    stringsAsFactors = FALSE
  )

  advan_trans_ref <- data.frame(
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

  # Is this a $PRED model?
  output$isPKpred <- isPKpred <- reactive({
    req(input$pkInput)
    input$pkInput == "pred"
  })
  output$isPDpred <- isPDpred <- reactive({
    req(input$pkInput, input$pdInput)
    input$pkInput %in% c("none", "pred") & input$pdInput %in% c("er", "pred", "logis", "ordcat")
  })
  isPRED <- reactive({
    req(isPKpred, isPDpred)
    isPKpred() | isPDpred()
  })

  # Is this a $PK model with ODE?
  isODE <- reactive({
    req(input$pkInput, input$pdInput, input$eliminationInput, input$absorptionInput)
    input$pkInput == "ode" |
      (input$pkInput == "pk" & (input$eliminationInput != "lin" | input$absorptionInput == "transit")) |
      input$pdInput %in% c("ode", "idr") |
      (input$pdInput == "link" & input$pkInput != "linmat")
  })
  isODE_mrg <- reactive({
    req(input$pkInput, input$pdInput, input$eliminationInput, input$absorptionInput)
    input$pkInput %in% c("ode", "pk", "linmat") | input$pdInput %in% c("ode", "idr", "link")
  })

  # Is this $PK model defined by first-order rates matrix
  isLINMAT <- reactive({
    req(input$pkInput, input$pdInput)
    input$pkInput == "linmat" & ! input$pdInput %in% c("idr", "ode")
  })

  # Is this simple $PK model
  output$isPREDPP <- isPREDPP <- reactive({
    req(isPRED, isODE, isLINMAT)
    !isPRED() & !isODE() & !isLINMAT()
  })

  outputOptions(output, "isPKpred", suspendWhenHidden = FALSE)
  outputOptions(output, "isPDpred", suspendWhenHidden = FALSE)
  outputOptions(output, "isPREDPP", suspendWhenHidden = FALSE)

  ## Dynamic UI for PK model
  output$absorptionUI <- renderUI({
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      choices <- c(
        "Bolus" = "bolus",
        "Infusion" = "zero",
        "First-order" = "first",
        "Sigmoid" = "sig"
      )
      if (input$pkInput != "linmat"){
        choices <- c(
          choices,
          "Transit compartments" = "transit"
        )
      }
      if (input$pkInput != "pk"){
        choices <- c(
          choices,
          "Custom" = "custom"
        )
      }
      col_4(
        selectInput(
          inputId = "absorptionInput",
          width = "100%",
          label = "Dosing",
          choices = choices,
          selected = "first"
        )
      )
    }
  })
  output$pkZeroEstUI <- renderUI({
    req(input$absorptionInput)
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      if (input$absorptionInput %in% c("zero", "sig")) {
        if (input$platformInput == "NONMEM") {
          choices <- c(
            "Fixed in dataset" = 0,
            "Rate estimated" = -1,
            "Duration estimated" = -2
          )
        } else {
          choices <- c(
            "Set rate" = -1,
            "Set duration" = -2
          )
        }
        selected <- choices[0]
        col_4(
          selectInput(
            inputId = "zeroEstInput",
            width = "100%",
            label = "Zero-order rate",
            choices = choices,
            selected = selected
          )
        )
      }
    }
  })

  output$alagUI <- renderUI({
    req(input$absorptionInput)
    if ( input$pkInput %in% c("pk", "linmat", "ode") ) {
      if (input$absorptionInput != "transit") {
        choices <- c("Yes" = TRUE, "No" = FALSE)
      } else {
        choices <- c("No" = FALSE)
      }
      if (input$pkInput %in% c("pk", "linmat", "ode")) {
        col_4(
          radioButtons(
            inputId = "alagInput",
            label = "Include dosing lag?",
            choices = choices,
            selected = FALSE,
            inline = FALSE
          )
        )
      } else {
        NULL
      }
    }
  })

  output$eliminationUI <- renderUI({
    req(input$pkInput)
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

  output$pknCMTUI <- renderUI({
    req(input$absorptionInput)
    if (!(input$pkInput %in% c("pk", "linmat", "ode"))) {
      NULL
    } else {
      numericInput(
        inputId = "pknCMTInput",
        width = "100%",
        label = "Number of compartments",
        min = ifelse(input$absorptionInput %in% c("bolus", "zero"), 1, 2),
        value = ifelse(input$absorptionInput %in% c("bolus", "zero"), 1, 2),
        step = 1
      )
    }
  })
  output$pkDefaultDoseUI <- renderUI({
    req(input$pknCMTInput)
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
    req(input$pknCMTInput, input$absorptionInput)
    numericInput(
      inputId = "pkDefaultObsInput",
      width = "100%",
      label = "Default observation compartment",
      min = 1,
      max = as.numeric(input$pknCMTInput),
      value = ifelse(input$absorptionInput %in% c("bolus", "zero"), 1, 2),
      step = 1
    )
  })

  # Define if and which ADVAN should be used?
  advan <- reactive({
    if (isPRED()){
      advan <- NA
    } else {
      if ( isODE() | isLINMAT() ){
        advan <- input$advanInput
      } else {
        advan <- input_advan_ref %>%
          dplyr::filter( CMT == as.integer(input$pkCMTInput) & INPUT == input$absorptionInput ) %>%
          dplyr::pull(ADVAN)
      }
    }
    advan
  })

  # Define if and which ADVAN should be used?
  trans <- reactive({
    req(input$transInput)
    if (isPRED()){
      if (isPKpred()){
        trans <- NA
      }
      if (isPDpred){

      }
    } else {
      if (isODE() | isLINMAT()){
        trans <- 1
      } else {
        if (length(input$transInput) == 0){
          trans <- if ( is.na(advan()) ){
            1
          } else {
            advan_trans_ref %>%
              dplyr::slice( advan() ) %>%
              dplyr::pull(DEAFULT)
          }
        } else {
          trans <- as.numeric(sub("TRANS", "", input$transInput))
        }
      }
    }
    trans
  })

  # Parameterization UI
  output$advanUI <- renderUI({

    if ( isPRED() ){
      return(NULL)
    }

    if ( input$pkInput == "ode" |
         (input$pkInput == "pk" & input$eliminationInput != "lin" | input$absorptionInput == "transit") |
         input$pdInput %in% c("ode", "idr") | (input$pdInput == "link" & input$pkInput != "linmat")
    ) {
      choices <- c("ADVAN6" = 6, "ADVAN8" = 8, "ADVAN9" = 9, "ADVAN13" = 13)
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

  output$transUI <- renderUI({

    if ( isPRED() ){
      return(NULL)
    }

    selectInput(
      inputId = "transInput",
      width = "100%",
      label = "TRANS",
      choices = paste0(
        "TRANS",
        if (is.na(advan())){
          advan_trans_ref %>%
            dplyr::filter(is.na(ADVAN)) %>%
            dplyr::pull(TRANS)
        } else {
          unlist(
            strsplit(
              advan_trans_ref %>%
                dplyr::filter(ADVAN == advan() & !is.na(ADVAN)) %>%
                dplyr::pull(TRANS),
              ","
            )
          )
        }
      ),
      selected = paste0(
        "TRANS",
        ifelse(
          is.na(advan()),
          advan_trans_ref %>%
            dplyr::filter(is.na(ADVAN)) %>%
            dplyr::pull(DEFAULT),
          advan_trans_ref %>%
            dplyr::filter( ADVAN == advan() & !is.na(ADVAN) ) %>%
            dplyr::pull(DEFAULT)
        )
      )
    )
  })

  # TMDD estimated parameters
  output$tmddUI <- renderUI({
    req(parm_lib, input$eliminationInput)

    # Get subset of parm_lib
    parms <- parm_lib %>%
      dplyr::filter(
        CMT == input$pkCMTInput &
          grepl(input$absorptionInput, ABSORPTION) &
          ELIMINATION == input$eliminationInput
      )

    # Process parameter choices
    choices <- parms$TRANS
    names(choices) <- gsub(
      "[|]+", ", ",
      gsub(
        "^[|]+|[|]+$", "",
        gsub(
          "CL|VC|Q|VP|CLD1|CLD2|VP1|VP2|KA|MTT|NN", "",
          parms$PARMS
        )
      )
    )

    # Build selector input
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

    if (input$pkInput %in% c("pk", "linmat", "ode")){
      choices <- c(
        "None" = "none",
        "Direct effect" = "direct",
        "Biophase / Link" = "link",
        "Indirect response" = "idr",
        "Defined by ODEs" = "ode"
      )
      selected <- "none"
    } else if (input$pkInput == "pred"){
      choices <- c(
        "None" = "none",
        "Exposure-Response" = "er",
        "Defined by explicit solutions" = "pred"
      )
      selected <- "none"
    } else {
      if (input$platformInput != "Berkeley Madonna"){
        choices <- c(
          "Exposure-Response" = "er",
          "Defined by ODEs" = "ode",
          "Defined by explicit solutions" = "pred",
          "Logistic regression" = "logis",
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

  # Dynamic UI for parameterization of direct effect, link, E-R, logistic
  # regression, and ordered categorical PD model
  output$endpointUI <- renderUI({
    if (input$pdInput == "ordcat") {
      textInput(
        inputId = "endpointInput",
        width = "100%",
        label = "Endpoint",
        placeholder = "Enter the endpoint name"
      )
    }
  })
  output$minCategoryUI <- renderUI({
    if (input$pdInput == "ordcat") {
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
    if (input$pdInput == "ordcat") {
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
    if (input$pdInput %in% c("logis", "ordcat")) {
      choices <- c(
        "None" = "base",
        "Linear" = "lin",
        "Power" = "power",
        "Exponential" = "exp",
        "Michaelis-Menten" = "mm",
        "Hill" = "hill",
        "Weibull" = "weibull")
    } else {
      choices <- c(
        "Linear" = "lin",
        "Power" = "power",
        "Exponential" = "exp",
        "Michaelis-Menten" = "mm",
        "Hill" = "hill",
        "Weibull" = "weibull")
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

    req(input$effectFormInput)

    type <- ifelse(
      input$pdInput %in% c("logis", "ordcat"),
      "logis_ordcat",
      "direct_er"
    )
    choices <- pdForm_lib %>%
      dplyr::filter(TYPE == type & FORM ==input$effectFormInput ) %>%
      dplyr::pull(PARAMETERIZATION)

    selectInput(
      inputId = "effectParmInput",
      width = "100%",
      label = "Parameterization",
      choices = choices,
      selected = choices[1]
    )
  })

  output$effectStimUI <- renderUI({

    req(input$pdInput, input$effectFormInput, input$effectParmInput)

    if ( isTruthy(input$effectFormInput) && input$effectFormInput == "base") {
      NULL
    } else {

      type <- ifelse(
        input$pdInput %in% c("logis", "ordcat"),
        "logis_ordcat",
        "direct_er"
      )
      choices <- pdForm_lib %>%
        dplyr::filter(
          TYPE == "logis_ordcat" &
            FORM == input$effectFormInput &
            PARAMETERIZATION == as.numeric(input$effectParmInput)
        ) %>%
        dplyr::select(INCREASE, DECREASE)
      choices <- as.vector(unlist(choices))

      if (input$pdInput == "er"){
        if (identical(choices, c(1L, 1L))) {
          choices <- c("Increasing" = TRUE, "Decreasing" = FALSE)
        } else if (identical(choices, c(0L,1L))){
          choices <- c("Decreasing" = FALSE)
        } else {
          choices <- c("Increasing" = TRUE)
        }
      } else {
        if (identical(choices, c(1L,1L))) {
          choices <- c("Stimulatory" = TRUE, "Inhibitory" = FALSE)
        } else if (identical(choices, c(0L,1L))){
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

  output$effectMathjax <- renderUI({

    req(input$effectFormInput, input$effectParmInput, input$effectStimInput)

    withMathJax(
      helpText(
        sprintf(
          "Prototypical model: \\(\\quad %s\\)",
          parm_lib %>%
            dplyr::filter(
              TYPE == "function" &
                FORM == input$effectFormInput &
                TRANS == as.numeric(input$effectParmInput) &
                INCREASE == as.integer(as.logical(input$effectStimInput))
            ) %>%
            dplyr::pull(MATHJAX)
        )
      )
    )
  })

  output$exposureVarUI <- renderUI({
    req(input$pdInput)

    if (input$pdInput != "er" | input$platformInput != "NONMEM"){
      return(NULL)
    }

    if (length(dataVars()) > 0 && any(dataVars() != "") ){
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

  output$logisDriverVarUI <- renderUI({
    req(input$pdInput)

    if (!input$pdInput %in% c("logis", "ordcat") | input$platformInput != "NONMEM"){
      return(NULL)
    }

    if (length(dataVars()) > 0 && any(dataVars() != "") ){
      selectInput(
        inputId = "logisVarInput",
        width = "100%",
        label = "Response driver",
        choices = unique( c("", dataVars()[which(dataVars() != "DV")]) ),
        selected  = ""
      )
    } else {
      textInput(
        inputId = "logisVarTextInput",
        width = "100%",
        label = "Response driver",
        placeholder = "Enter a valid variable name"
      )
    }
  })

  output$effectDriverUI <- renderUI({
    max <- value <- 1
    if (input$pkInput == "pk"){
      if (input$absorptionInput %in% c("zero", "bolus")){
        max <- as.numeric(input$pkCMTInput)
        value <- 1
      } else {
        max <- as.numeric(input$pkCMTInput) + 1
        value <- 2
      }
    } else if (input$pkInput %in% c("linmat", "ode")){
      max <- as.numeric(input$pknCMTInput)
      value <- as.numeric(input$pkDefaultObsInput)
    }
    min <- 1
    if (input$pdInput == "link"){
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

  ## Dynamic UI for IDR model parameterization
  output$idrStimUI <- renderUI({

    req(input$idrTypeInput)

    if (input$idrTypeInput %in% c("idr3", "idr4")){
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

  output$idrMathjax <- renderUI({

    req(input$idrTypeInput, input$idrParmInput, input$idrStimInput)

    fluidRow()
    withMathJax(
      helpText(
        sprintf(
          "Prototypical model: \\(\\quad \\begin{align}%s \\\\ %s\\end{align}\\)",
          parm_lib %>%
            dplyr::filter(
              TYPE == "idr" &
                FORM == input$idrTypeInput &
                TRANS == input$idrParmInput
            ) %>%
            dplyr::pull(MATHJAX),
          parm_lib %>%
            dplyr::filter(
              TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
                FORM == input$idrStimInput
            ) %>%
            dplyr::pull(MATHJAX)
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
            HTML_info("No model structure defined")
          )
        )
      )
    }

  })

  parameterTable_input_content <- reactive({

    req(input$pkInput, input$pdInput, input$nOTParmInput)

    req( input$pkInput != "none" | input$pdInput != "none")

    nparms <- 0

    ### PK parameters
    if (input$pkInput == "none")
    {

      pkDF <- data.frame(
        Type = character(),
        SourceParam = character(),
        Parameter = character(),
        Label = character(),
        Unit = character(),
        Low = character(),
        Initial = character(),
        High = character(),
        Fixed = character(),
        Variability = character(),
        stringsAsFactors = FALSE
      )

    } else if (input$pkInput == "pk")
    {
      req(input$absorptionInput, input$eliminationInput)

      index <- get_model_lib_index(
        input = input,
        advan = advan,
        trans = trans,
        parm_lib = parm_lib
      )
      parm_info <- parm_lib %>%
        dplyr::slice(index) %>%
        tidyr::separate_rows(
          PARMS, VAR, LOW, INITIAL, HI,
          sep = "[|]"
        )

      pkDF <- data.frame(
        Type = rep("PK", nrow(parm_info)),
        SourceParam = parm_info$PARMS,
        Parameter = parm_info$PARMS,
        Label = parm_info$PARMS,
        Unit = parm_info$PARMS,
        Low = parm_info$LOW,
        Initial = parm_info$INITIAL,
        High = parm_info$HI,
        Fixed = "No",
        Variability = parm_info$VAR,
        stringsAsFactors = FALSE
      )

      if (input$absorptionInput %in% c("zero", "sig")){
        rate <- as.numeric(input$zeroEstInput)
        if (length(rate) > 0 && rate < 0){
          PARM <- ifelse(rate == -1, "R1", "D1")
          pkDF <- pkDF %>%
            dplyr::bind_rows(
              data.frame(
                Type = "PK",
                SourceParam = PARM,
                Parameter = PARM,
                Label = PARM,
                Unit = PARM,
                Low = "0",
                Initial = "1",
                High = "+INF",
                Fixed = "No",
                Variability = "exp",
                stringsAsFactors = FALSE
              )
            )
        }
      }
      if (as.logical(input$alagInput)){
        pkDF <- pkDF %>%
          dplyr::bind_rows(
            data.frame(
              Type = "PK",
              SourceParam = "ALAG1",
              Parameter = "ALAG1",
              Label = "ALAG1",
              Unit = "ALAG1",
              Low = "0",
              Initial = "1",
              High = "+INF",
              Fixed = "No",
              Variability = "exp",
              stringsAsFactors = FALSE
            )
          )
      }

    } else
    {

      pkparms <- paste0("TH", 1:abs(as.numeric(input$nPKParmInput)) )
      if (!isPRED() & input$absorptionInput %in% c("first", "sig")){
        pkparms <- c(pkparms, "KA")
      }
      if (!isPRED() & input$absorptionInput %in% c("zero", "sig")){
        rate <- as.numeric(input$zeroEstInput)
        if (rate < 0){
          pkparms <- c(
            pkparms,
            ifelse(
              rate == -1,
              sprintf("R%s", input$pkDefaultDoseInput),
              sprintf("D%s", input$pkDefaultDoseInput)
            )
          )
        }
      }
      if (!isPRED() & input$absorptionInput == "transit"){
        pkparms <- c(pkparms, "MTT", "NN")
      }
      if (!isPRED() & as.logical(input$alagInput)){
        pkparms <- c(
          pkparms,
          sprintf("ALAG%s", input$pkDefaultDoseInput)
        )
      }

      pkDF <- data.frame(
        Type = "PK",
        SourceParam = pkparms,
        Parameter = pkparms,
        Label = pkparms,
        Unit = pkparms,
        Low = "0",
        Initial = "1",
        High = "+INF",
        Fixed = "No",
        Variability = "exp",
        stringsAsFactors = FALSE
      ) %>%
        dplyr::mutate(
          Low = ifelse(Parameter == "NN", "1", "0"),
          Initial = ifelse(Parameter == "NN", "2", "1")
        )

    }

    ### PD parameters
    if (input$pdInput == "none")
    {

      parm_info <- NULL

    } else if (input$pdInput %in% c("direct", "er"))
    {

      req(input$effectFormInput, input$effectParmInput, input$effectStimInput)

      parm_info <- parm_lib %>%
        dplyr::filter(
          TYPE == "function" &
            FORM == input$effectFormInput &
            TRANS == input$effectParmInput &
            INCREASE == as.integer(as.logical(input$effectStimInput))
        ) %>%
        tidyr::separate_rows(
          PARMS, VAR, LOW, INITIAL, HI,
          sep = "[|]"
        )

    } else if (input$pdInput == "link")
    {

      req(input$effectFormInput, input$effectParmInput, input$effectStimInput)

      parm_info <- parm_lib %>%
        dplyr::filter(
          TYPE == "link" |
            (
              TYPE == "function" &
                FORM == input$effectFormInput &
                TRANS == input$effectParmInput &
                INCREASE == as.integer(as.logical(input$effectStimInput))
            )
        ) %>%
        tidyr::separate_rows(
          PARMS, VAR, LOW, INITIAL, HI,
          sep = "[|]"
        )

    } else if (input$pdInput == "idr")
    {

      req(input$idrTypeInput, input$idrParmInput, input$idrStimInput)

      parm_info <- parm_lib %>%
        dplyr::filter(
          (
            TYPE == "idr" & FORM == input$idrTypeInput & TRANS == input$idrParmInput
          ) |
            (
              TYPE == ifelse(input$idrTypeInput %in% c("idr1", "idr2"), "inh", "stim") &
                FORM == input$idrStimInput
            )
        ) %>%
        tidyr::separate_rows(
          PARMS, VAR, LOW, INITIAL, HI,
          sep = "[|]"
        )

    } else if (input$pdInput %in% c("logis", "ordcat"))
    {

      req(input$effectFormInput, input$effectParmInput, input$effectStimInput)

      if (input$pdInput == "logis") {

        parm_info <- parm_lib %>%
          dplyr::filter(TYPE == "logis") %>%
          tidyr::separate_rows(
            PARMS, VAR, LOW, INITIAL, HI,
            sep = "[|]"
          )

      } else {

        req(input$minCategoryInput, input$maxCategoryInput)


        if ( areTruthy(input$maxCategoryInput, input$minCategoryInput) ){
          if (input$minCategoryInput <= input$maxCategoryInput){
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
        pdparms <- paste0("LGI", minCat:maxCat)
        if (ncats > 1) {
          parm_lib <- parm_lib %>%
            dplyr::bind_rows(
              data.frame(
                PARMS = pdparms,
                VAR = c("add", rep("none", ncats - 1)),
                LOW = c("-INF", rep("0", ncats -1)),
                INIT = rep("1", ncats),
                HI = rep("+INF", ncats),
                stringsAsFactors = FALSE
              )
            )
        } else {
          parm_lib <- parm_lib %>%
            dplyr::bind_rows(
              data.frame(
                PARM = pdparms,
                VAR = "add",
                LOW = "-INF",
                INIT = "1",
                HIGH = "+INF",
                stringsAsFactors = FALSE
              )
            )
        }

        parm_info <- parm_lib %>%
          dplyr::filter(PARMS %in% pdparms)

      }

      # Remove baseline parameter in function parameters
      stim_parm_info <- parm_lib %>%
        dplyr::filter(
          TYPE == "function" &
            FORM == input$effectFormInput &
            TRANS == input$effectParmInput &
            INCREASE == as.integer(as.logical(input$effectStimInput))
        ) %>%
        tidyr::separate_rows(
          PARMS, VAR, LOW, INITIAL, HI,
          sep = "[|]"
        ) %>%
        dplyr::slice(-1)

      parm_info <- parm_info %>%
        dplyr::bind_rows(
          stim_parm_info
        )

    } else
    {

      nPDparms <- abs(as.numeric(input$nPDParmInput))
      pdparms <- paste0("TH", nrow(pkDF) + 1:nPDparms)

      parm_lib <- parm_lib %>%
        dplyr::bind_rows(
          data.frame(
            PARMS = pdparms,
            VAR = rep("exp", nPDparms),
            LOW = rep("0", nPDparms),
            INIT = rep("1", nPDparms),
            HI = rep("+INF", nPDparms),
            stringsAsFactors = FALSE
          )
        )

      parm_info <- parm_lib %>%
        dplyr::filter(PARMS %in% pdparms)

    }

    if ( is.null(parm_info) )
    {
      pdDF <- data.frame(
        Type = character(),
        SourceParam = character(),
        Parameter = character(),
        Label = character(),
        Unit = character(),
        Low = character(),
        Initial = character(),
        High = character(),
        Fixed = character(),
        Variability = character(),
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
        Low = parm_info$LOW,
        Initial = parm_info$INITIAL,
        High = parm_info$HI,
        Fixed = "No",
        Variability = parm_info$VAR,
        stringsAsFactors = FALSE
      )
    }

    ### Other parameters
    nOTparms <- abs(as.numeric(input$nOTParmInput))
    if ( nOTparms == 0 )
    {
      otDF <- data.frame(
        Type = character(),
        SourceParam = character(),
        Parameter = character(),
        Label = character(),
        Unit = character(),
        Low = character(),
        Initial = character(),
        High = character(),
        Fixed = character(),
        Variability = character(),
        stringsAsFactors = FALSE
      )
    } else
    {
      otparms <- paste0("TH", nrow(pkDF) + nrow(pdDF) + 1:nOTparms)
      parm_lib <- parm_lib %>%
        dplyr::bind_rows(
          data.frame(
            PARMS = otparms,
            VAR = rep("exp", length(otparms)),
            LOW = rep("0", length(otparms)),
            INIT = rep("1", length(otparms)),
            HI = rep("+INF", length(otparms)),
            stringsAsFactors = FALSE
          )
        )
      parm_info <- parm_lib %>%
        dplyr::filter(PARMS %in% otparms)

      otDF <- data.frame(
        Type = rep("OT", nrow(parm_info)),
        SourceParam = parm_info$PARMS,
        Parameter = parm_info$PARMS,
        Label = parm_info$PARMS,
        Unit = parm_info$PARMS,
        Low = parm_info$LOW,
        Initial = parm_info$INITIAL,
        High = parm_info$HI,
        Fixed = "No",
        Variability = parm_info$VAR,
        stringsAsFactors = FALSE
      )

    }

    ### Create table of parameters

    DF <- dplyr::bind_rows(pkDF, pdDF, otDF) %>%
      dplyr::mutate(
        Label = get_labelunit(
          input = input,
          parms = Label,
          labelunit_lib = labelunit_lib,
          what = "label"
        ),
        Unit = get_labelunit(
          input = input,
          parms = Unit,
          labelunit_lib = labelunit_lib,
          what = "unit"
        ),
        Fixed = factor(Fixed, levels = c("Yes", "No"), ordered = TRUE),
        Variability = factor(
          dplyr::case_when(
            Variability == "none" ~ "None",
            Variability == "add" ~ "Additive",
            Variability == "exp" ~ "Exponential",
            Variability == "logit" ~ "Logit",
            TRUE ~ "NA"
          ),
          levels = c("None", "Additive", "Exponential", "Logit"),
          ordered = TRUE
        )
      )

    ### Check content of input$parameterTable and preserve custom inputs
    if ( length(isolate(input$parameterTable)) > 0 )
    {
      oDF <- hot_to_r(isolate(input$parameterTable))
      if (nrow(DF) == 0){
        DF <- oDF
      }
      if (!identical(DF, oDF)) {
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

        for (col in names(oDF)[-2]){
          DF[, col] <- ifelse(
            is.na(mDF[,paste(col, "y", sep = ".")]) |
              (col == "Label" & mDF$SourceParam %in% c("KM", "IC50", "SC50")),
            mDF[, paste(col, "x", sep = ".")],
            mDF[, paste(col, "y", sep = ".")]
          )
          if (is.factor(oDF[, col])){
            # ifelse coerces factors to integers, must reset to factor
            DF[, col] <- factor(levels(oDF[, col])[DF[, col]], levels = levels(oDF[, col]), ordered = TRUE)
          }
        }
      }
    }

    ### Adjust based upon software platform
    if (input$platformInput %in% c("mrgsolve", "Berkeley Madaonna")){
      DF$Fixed <- NULL
    }
    if (input$platformInput == "Berkeley Madaonna"){
      DF$Low <- DF$High <- DF$Variability <- NULL
    }

    DF

  })

  output$parameterTable <- renderRHandsontable({

    req(parameterTable_input_content())

    DF <- parameterTable_input_content() %>%
      dplyr::select(-Variability)

    tmp <- rhandsontable(
      data = DF,
      rowHeaders = TRUE,
      contextMenu = FALSE,
      manualColumnMove = FALSE,
      manualRowMove = TRUE,
      width = "100%",
      height = max(200, (nrow(DF) + 1)*25 + 10)  # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(col = "Type", readOnly = TRUE, colWidths = 50) %>%
      hot_col(col = "SourceParam", colWidths = 0.1) %>% # Hide the merge key SourceParm
      hot_col(col = "Parameter", colWidths = 90) %>%
      hot_col(col = "Label", colWidths = 250) %>%
      hot_col(col = "Unit", colWidths = 50) %>%
      hot_col(col = "Low", colWidths = 50) %>%
      hot_col(col = "Initial", colWidths = 50) %>%
      hot_col(col = "High", colWidths = 50) %>%
      hot_col(
        col = ifelse(
          input$platformInput != "Berkeley Madonna",
          c("Low", "Initial", "High"),
          "Initial"
        ),
        renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          // Apply scientific notation for number x if x!=0 & |x| > 1e4 | |x| < 1e-2
          let str;
          if (typeof value === 'number') {
            value = +value;
            if (value !== 0 && (Math.abs(value) > 1e4 || Math.abs(value) < 1e-2)) {
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
        var hot = this.hot
        $('#collapse').find('a').click(function() {
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    # Adjust based upon software platform
    if (input$platformInput == "NONMEM") {
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

  output$parameterTableUI <- renderUI({rHandsontableOutput("parameterTable")})

  parameterTable_content <- reactive({
    if (is.null(input$parameterTable) | length(input$parameterTable$data) == 0) {
      return(NULL)
    } else {
      hot_to_r(input$parameterTable)
    }
  })

  output$parmsUI <- renderUI({

    req(input$pkInput, input$pdInput)

    fluidRow(
      col_12(
        fluidRow(
          conditionalPanel(
            condition = "input.platformInput == 'NONMEM' && input.pdInput != 'logis' && input.pdInput != 'ordcat'",
            col_3(
              radioButtons(
                inputId = "muInput",
                label = "MU referencing",
                choices = c("Yes" = TRUE, "No" = FALSE),
                selected = FALSE,
                inline = TRUE
              )
            )
          ),
          # Number of custom PK parameters
          conditionalPanel(
            condition = "output.isPKpred | input.pkInput == 'ode' | input.pkInput == 'amat'",
            col_3(
              numericInput(
                inputId = "nPKParmInput",
                width = "100%",
                label = "Additional PK parameters",
                value = 1,
                min = 1,
                step = 1
              )
            )
          ),
          # Number of additional PD parameters
          conditionalPanel(
            condition = "input.pdInput == 'pred' | input.pdInput == 'ode'",
            col_3(
              numericInput(
                inputId = "nPDParmInput",
                width = "100%",
                label = "Additional PD parameters",
                value = 1,
                min = 1,
                step = 1
              )
            )
          ),
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
            strong("Parameters"),
            uiOutput("parameterTableUI")
          )
        )
      )
    )

  })

  outputOptions(output, "parmsUI", suspendWhenHidden = FALSE)

  output$duplicateParmWarningUI <- renderUI({
    if (is.null(input$parameterTable) | length(input$parameterTable$data) == 0){
      NULL
    } else {
      parms <- hot_to_r(input$parameterTable)$Parameter
      dupParms <- unique(parms[duplicated(parms)])
      if (length(dupParms) == 0){
        NULL
      } else {
        HTML_info(
          sprintf(
            paste(
              "The parameter table includes duplicates (%s). Edit the parameter",
              "names or modify the parameterization of the PK or PD models."
            ),
            paste(dupParms, collapse = ", ")
          )
        )

      }
    }
  })


  #---- Variance tab ----

  output$varianceWarningUI <- renderUI({

    if ( notTruthy(input$pkInput, input$pdInput) ){
      fluidRow(
        col_12(
          HTML_info("No model structure defined")
        )
      )
    }

  })

  output$varianceTable <- renderRHandsontable({

    if (input$platformInput == "Berkeley Madonna") {
      return(NULL)
    }

    req(input$parameterTable)

    # Capture cases when parameter table contains duplicate
    parms <- hot_to_r(input$parameterTable)$Parameter
    if ( length( unique(parms[duplicated(parms)]) ) != 0 ){
      return(NULL)
    }

    DF <- parameterTable_input_content() %>%
      dplyr::select(Parameter, Variability) %>%
      dplyr::rename(" " = "Parameter")

    rhandsontable(
      data = DF,
      rowHeaders = FALSE,
      contextMenu = FALSE,
      manualColumnMove = FALSE,
      manualRowMove = TRUE,
      width = 200,
      height = max(200, (nrow(DF) + 1)*25 + 10)  # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(
        col = " ",
        readOnly = TRUE,
        colWidths = 50
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
        var hot = this.hot
        $('#collapse').find('b').click(function() {
          setTimeout(function() {hot.render();}, 0);
        })
      }")

  })

  outputOptions(output, "varianceTable", suspendWhenHidden = FALSE)

  output$varianceTableUI <- renderUI({rHandsontableOutput("varianceTable")})

  varianceTable_content <- reactive({
    if (is.null(input$varianceTable) | length(input$varianceTable$data) == 0) {
      return(NULL)
    } else {
      hot_to_r(input$varianceTable)
    }
  })

  #---- Covariance ----

  output$covarianceTable <- renderRHandsontable({

    if (input$platformInput == "Berkeley Madonna") {
      return(NULL)
    }

    req(varianceTable_content())

    varianceTable <- varianceTable_content()

    # Create diagonal matrix (default variance is 0.2) as linear array
    n <- nrow(varianceTable)
    if (length(isolate(input$covarianceTable)) > 0 && nrow(hot_to_r(isolate(input$covarianceTable))) == n) {
      DF <- as.vector(
        as.matrix(
          hot_to_r(
            isolate(input$covarianceTable)
          )
        )
      )
    } else {
      DF <- as.vector(diag(n)*0.2)
    }

    # Set diagonal and lower elements to 0 based upon parameters without variability
    if ( any(varianceTable$Variability == "None") ){
      rows <- which( varianceTable$Variability == "None" )
      indices <- unlist(
        sapply(
          rows,
          function(i, n){
            sapply(
              1:i,
              function(j, i, n){ i:n + (j-1)*n }, i, n
            )
          },
          n,
          simplify = FALSE
        )
      )
      DF[indices] <- 0
    }

    # Change upper triangle cells into NA
    if (n > 1) {
      indices <- unlist(
        sapply(
          1:(n-1),
          function(i, n){
            sapply(
              (i+1):n,
              function(j, i, n){ i + (j-1)*n }, i, n
            )
          },
          n
        )
      )
      DF[indices] <- NA
    }

    # Convert to data.frame
    DF <- data.frame(
      matrix(DF, nrow = n)
    )
    names(DF) <- rownames(DF) <- varianceTable[, 1]

    # Get coordinates of upper triangle cells
    if (n > 1) {
      indices <- matrix(
        c(
          sapply(
            1:(n*(n-1)/2),
            function(k, n){
              n - 2 - floor(sqrt(-8*(k - 1) + 4*n*(n-1)-7)/2.0 - 0.5)
            },
            n
          ),
          sapply(
            1:(n*(n-1)/2),
            function(k, n){
              i <- n - 2 - floor(sqrt(-8*(k - 1) + 4*n*(n-1)-7)/2.0 - 0.5)
              k + i - n*(n-1)/2 + (n-i)*((n-i)-1)/2
            },
            n
          )
        ),
        ncol = 2
      ) + 1
    }

    # Create the rhandsontable object
    tmp <- rhandsontable(
      data = DF,
      width = "100%",
      height = (nrow(DF) + 1)*25 + 10
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_validate_numeric(col = 1:nrow(DF), min = 0)

    # Lock cells from upper triangle
    if (n > 1) {
      lock <- paste0(
        "tmp %>% ",
        paste(
          sapply(
            1:nrow(indices),
            function(x, indices){
              sprintf(
                "hot_cell(%s, %s, readOnly = TRUE)",
                indices[x, 1],
                indices[x, 2]
              )
            },
            indices
          ),
          collapse = " %>% "
        )
      )
    } else {
      lock <- "tmp"
    }

    tmp <- eval(parse(text = lock))

    # Lock cells based upon parameters without variability
    if (any(varianceTable$Variability == "None")){
      rows <- which(varianceTable$Variability == "None")

      indices <- unique(
        matrix(
          c(
            unlist(sapply(rows, function(i, n) rep(i:n, each = i), n)),
            unlist(sapply(rows, function(i, n) rep(1:i, times = n - i + 1), n, simplify = FALSE))
          ),
          ncol = 2
        )
      )
      lock.cells <- paste0(
        "tmp %>% ",
        paste(
          apply(
            indices,
            1,
            function(x){
              sprintf("hot_cell(%s, %s, readOnly = TRUE)", x[1], x[2])
            }
          ),
          collapse = " %>% "
        )
      )
      tmp <- eval(parse(text = lock.cells))
    }

    tmp %>%
      hot_cols(renderer = "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        if (cellProperties.readOnly == true) {
          td.style.background = '#eee';
          td.style.color = '#aaa';
        }
        // Apply scientific notation for number x if x!=0 & |x| > 1e4 | |x| < 1e-2
        let str;
        if (typeof value === 'number') {
          value = +value;
          if (value !== 0 && (Math.abs(value) > 1e4 || Math.abs(value) < 1e-2)) {
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
        var hot = this.hot
        $('#collapse').find('a').click(function() {
          setTimeout(function() {hot.render();}, 0);
        })
      }")

  })

  outputOptions(output, "covarianceTable", suspendWhenHidden = FALSE)

  output$covarianceTableUI <- renderUI({rHandsontableOutput("covarianceTable")})

  # Process covariance matrix and check if there are errors
  covarianceCheck1 <- reactive({

    if ( !isTruthy(input$covarianceTable) ){
      return(list(isOK = FALSE, data = NULL))
    }

    if ( is.null(input$covarianceTable) | length(input$covarianceTable$data) == 0 ) {
      return(list(isOK = TRUE))
    } else {
      # Find if variance is set to 0 on a parameter with variability
      covarianceTable <- as.matrix(hot_to_r(input$covarianceTable))
      diag(covarianceTable)[is.na(diag(covarianceTable))] <- 0

      if (nrow(covarianceTable) != nrow(varianceTable_content())) {
        return(list(isOK = TRUE))
      }

      if (any(diag(covarianceTable) == 0 & isolate(varianceTable_content())$Variability != "None")) {
        return(list(isOK = FALSE))
      } else {
        return(list(isOK = TRUE))
      }
    }
  })

  covarianceCheck2 <- reactive({

    if ( !isTruthy(input$covarianceTable) ){
      return(list(isOK = FALSE, data = NULL))
    }

    if ( is.null(input$covarianceTable) | length(input$covarianceTable$data) == 0 ) {
      return(list(isOK = TRUE, data = NULL))
    } else {
      # Look for illegal 0's between non-0 covariance terms

      # Get correlation "table" as matrix of ones and zeros
      correlationTable <- get_correlation_table(
        hot_to_r(input$covarianceTable)
      )

      # Check that each line is made of ones or a series of zeros followed by ones
      check1 <- t(apply(correlationTable, 1, cumsum))
      check1 <- unlist(
        apply(check1, 1, function(x) {as.vector(table(x[x!=0]) > 1)  })
      )
      check1_OK <- all(!check1)

      return(list(isOK = check1_OK, data = NULL))
    }
  })

  covarianceCheck3 <- reactive({

    if ( !isTruthy(input$covarianceTable) ){
      return(list(isOK = FALSE, data = NULL))
    }

    if ( !covarianceCheck2()$isOK ) {
      return(list(isOK = FALSE, data = NULL))
    } else {
      if (is.null(input$covarianceTable) | length(input$covarianceTable$data) == 0){
        return(list(isOK = TRUE, data = NULL))
      } else {

        # Get correlation "table" as maxtrix of zeros and ones
        covarianceTable <- as.matrix(hot_to_r(input$covarianceTable))
        correlationTable <- get_correlation_table(
          x = covarianceTable,
          na_zero = TRUE
        )
        n <- nrow(correlationTable)

        # Find rows of parameter without correlation with others and check that the
        # corresponding columns also contains just one 1
        rowSums <- apply(correlationTable, 1, sum)
        colSums <- apply(correlationTable, 2, sum)

        isRowDiagonal <- sapply(
          1:n,
          function(i,rowSums, n){
            ifelse(i<n, rowSums[i] == 1 & rowSums[i+1] <= 1, rowSums[i] == 1)
          },
          rowSums,
          n
        )

        check2_OK <- all(rowSums[isRowDiagonal] == colSums[isRowDiagonal])

        return(
          list(
            isOK = check2_OK,
            data = list(
              covarianceTable = covarianceTable,
              isRowDiagonal = isRowDiagonal
            )
          )
        )
      }
    }
  })
  covarianceCheck4 <- reactive({

    if ( !isTruthy(input$covarianceTable) ){
      return(list(isOK = FALSE, data = NULL))
    }

    if ( !covarianceCheck2()$isOK | !covarianceCheck3()$isOK ) {
      return(list(isOK = FALSE, data = NULL))
    } else {

      if ( nrow(hot_to_r(input$covarianceTable)) != nrow(hot_to_r(input$varianceTable)) ){
        return(list(isOK = FALSE, data = NULL))
      }

      if ( is.null(input$covarianceTable) | length(input$covarianceTable$data) == 0 ){
        return(list(isOK = TRUE, data = NULL))
      } else {
        # Extract information
        covarianceTable <- as.matrix(covarianceCheck3()$data$covarianceTable)
        isRowDiagonal <- covarianceCheck3()$data$isRowDiagonal

        # Get correlation "table" as  matrix of zeros and ones
        correlationTable <- get_correlation_table(covarianceTable)

        # Process isRowDiagonal info
        if ( length(isRowDiagonal) > 0 && all(isRowDiagonal) ){
          return(
            list(
              isOK = TRUE,
              data = list(
                list(
                  omega = covarianceTable,
                  type = "diagonal"
                )
              )
            )
          )
        } else {
          ### Find omega blocks
          # tmp contains zero's for zero off-diagonal elements and upper triangle
          #              NA's for zero diagonal elements
          #              one's otherwise
          n <- nrow(covarianceTable)
          tmp <- matrix(0, ncol = n, nrow = n)
          tmp[bottom_triangle(tmp)] <- 1 - correlationTable[bottom_triangle(tmp)]
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
          block <- vector("list", max(blockCheck1, na.rm = TRUE))
          for ( iblock in 1:length(block)){
            matches <- match(blockCheck1, iblock)
            if ( length(matches) > 0 ){
              minIndex <- min( which(!is.na(matches)) )
              maxIndex <- max( which(!is.na(matches)) )
              omega <- covarianceTable[minIndex:maxIndex, minIndex:maxIndex, drop = FALSE]
              block[[iblock]] <- list(
                omega = omega,
                type = is_EDB(omega)
              )
            }
          }

          check3_OK <- all(sapply(block, function(x) x$type != "error"))

          return(
            list(
              isOK = check3_OK,
              data = block
            )
          )
        }
      }
    }
  })

  ## Dynamic UI for misspecified covariance matrix
  output$covarianceWarningUI <- renderUI({

    if (all(c(covarianceCheck1()$isOK, covarianceCheck2()$isOK, covarianceCheck3()$isOK, covarianceCheck4()$isOK))){
      NULL
    } else {
      tagList(
        if ( !covarianceCheck1()$isOK ) {
          HTML_info(
            "Variance cannot be set to 0 for a parameter with estimated variance"
          )
        },
        if ( !covarianceCheck1()$isOK ) {
          p()
        },
        if ( any(!c(covarianceCheck2()$isOK, covarianceCheck3()$isOK, covarianceCheck4()$isOK)) ) {
          HTML_info(
            paste(
              "The covariance matrix must be constructed as a series of diagonal,",
              "band, or full block matrices. Correlation will be ignored in",
              "the model."
            )
          )
        }
      )
    }

  })

  output$varianceUI <- renderUI({

    req(input$pkInput, input$pdInput)

    if (input$platformInput == "Berkeley Madonna") {
      return(
        HTML_info("Variance-covariance settings are not available for the selected software platform")
      )
    }

    if ( notTruthy(input$parameterTable) ){
      return(
        HTML_info("No parameters defined")
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
        HTML_info("Residual variability cannot be defined for the selected software platform")
      )
    }

    if ( notTruthy(input$pkInput, input$pdInput) ){
      fluidRow(
        col_12(
          HTML_info("No model structure defined")
        )
      )
    }

  })

  output$rvUI <- renderUI({

    req(input$pkInput, input$pdInput)

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
                input$pdInput %in% c("logis", "ordcat"),
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
    if (length(isolate(input$rvTable)) > 0){
      oDF <- hot_to_r(isolate(input$rvTable))
      if (nrow(DF) == 0){
        DF <- oDF
      }
      if (!identical(DF, oDF)) {
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

    req(rvTable_input_content())

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
      hot_validate_numeric(col = 3, min = 0)  %>%
      hot_col(
        col = "Variance",
        renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          // Apply scientific notation for number x if x!=0 & |x| > 1e4 | |x| < 1e-2
          let str;
          if (typeof value === 'number') {
            value = +value;
            if (value !== 0 && (Math.abs(value) > 1e4 || Math.abs(value) < 1e-2)) {
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
        var hot = this.hot
        $('#collapse').find('a').click(function() {
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

    req(input$pkInput, input$pdInput, pkRVInput(), pdRVInput())

    if (is.null(input$rvTable) | length(input$rvTable$data) == 0) {
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
    if (rvCheck()$isOK){
      NULL
    } else {
      HTML_info("Variance cannot be set to 0 for an estimated RV variance")
    }
  })

  output$rvFlagUI <- renderUI({

    req( input$pkInput, input$pdInput, pkRVInput(), pdRVInput() )

    req( input$platformInput == "NONMEM" )

    fluidRow(
      if (pkRVInput() %in% c("ccv", "log") | pdRVInput() %in% c("ccv", "log") ){
        col_6(
          radioButtons(
            input = "flagF0Input",
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
          input = "blqInput",
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
      Method = c('FOCE', rep('none', 4)),
      Interaction = c('Yes', rep('', 4)),
      Likelihood = c('No', rep('', 4)),
      NoPrediction = c('No', rep('', 4)),
      Options = rep('', 5),
      stringsAsFactors = FALSE
    )

    if ( isTruthy(input$blqInput) && as.logical(input$blqInput) ){
      DF$Options[1] <- 'LAPLACIAN'
    }

    if ( input$pdInput %in% c('logis', 'ordcat') ){
      DF <- data.frame(
        Step = as.character(1:5),
        Method = c('FOCE', rep('none', 4)),
        Interaction = c('No', rep('', 4)),
        Likelihood = c('Yes', rep('', 4)),
        NoPrediction = c('No', rep('', 4)),
        Options = c('LAPLACIAN', rep('', 4)),
        stringsAsFactors = FALSE
      )
    }

    tmp <- rhandsontable(
      data = DF,
      rowHeaders = NULL,
      contextMenu = FALSE,
      width = '100%',
      height = 160         # 25 px per row + 10 for potential scroll bar
    ) %>%
      hot_table(contextMenu = FALSE) %>%
      hot_col(
        col = 'Method',
        type = 'dropdown',
        source = c('none', 'FO', 'FOCE', 'LAPLACIAN', 'ITS', 'IMP', 'SAEM', 'BAYES')
      ) %>%
      hot_col(
        col = 'Interaction',
        type = 'dropdown',
        source = c('', 'Yes', 'No')
      ) %>%
      hot_col(
        col = 'Likelihood',
        type = 'dropdown',
        source = c('', 'Yes', 'No')
      ) %>%
      hot_col(
        col = 'NoPrediction',
        type = 'dropdown',
        source = c('', 'Yes', 'No')
      ) %>%
      hot_col(col = 'Step', readOnly = TRUE) %>%
      # To fix display problem: https://github.com/jrowen/rhandsontable/issues/366
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('#collapse').find('a').click(function() {
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    tmp

  })

  output$estimationTableUI <- renderUI({

    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }

    rHandsontableOutput('estimationTable')

  })

  output$estimationUI <- renderUI({
    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }
    checkboxInput(
      inputId = 'estimationInput',
      label = 'Perform estimation(s)',
      value = TRUE
    )
  })

  output$simulationUI <- renderUI({
    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }
    checkboxInput(
      inputId = 'simulationInput',
      label = 'Perform simulation(s)',
      value = FALSE
    )
  })

  output$taskUI <- renderUI({

    if ( input$platformInput != "NONMEM" ){
      return(NULL)
    }

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        fluidRow(
          col_12(
            HTML_info("No model structure defined")
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
        conditionalPanel(
          condition = "input.estimationInput",
          col_6(
            checkboxInput(
              inputId = 'covarianceInput',
              label = 'Perform covariance step',
              value = TRUE
            )
          )
        )
      ),
      fluidRow(
        col_4(
          uiOutput("simulationUI")
        ),
        conditionalPanel(
          condition = "input.simulationInput",
          col_4(
            numericInput(
              inputId = 'nsubInput',
              label = 'Number of simulations',
              width = '100%',
              min = 1,
              step = 1,
              value = 1
            )
          )
        ),
        conditionalPanel(
          condition = "input.simulationInput",
          col_4(
            numericInput(
              inputId = 'simulationSeedInput',
              label = 'Seed number',
              width = '100%',
              min = 1,
              step = 1,
              value = round(100000 * signif(runif(1), 5),0)
            )
          )
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

  #---- Miscellaneous ----

  output$mmUI <- renderUI({

    req(input$platformInput, input$pkInput, input$doseUnitInput)

    if (input$platformInput == 'NONMEM' & input$pkInput != 'none'){
      doseUnit <- input$doseUnitInput
      concentrationUnit <- unlist(strsplit(input$cpUnitInput, split = '[/]'))[1]

      if (grepl('g', doseUnit) != grepl('g', concentrationUnit)){
        numericInput(
          inputId = 'mmInput',
          label = 'Molecular Mass',
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

    req(input$platformInput)

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        fluidRow(
          col_12(
            HTML_info("No model structure defined")
          )
        )
      )
    }

    if ( input$platformInput == "NONMEM" & input$pkInput != "none" ){
      tagList(
        fluidRow(
          col_12(
            h4(strong("Scaling"))
          )
        ),
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

  output$optionsUI <- renderUI({

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(NULL)
    }

    wellPanel(
      h4(strong("Optimization options")),
      fluidRow(
        conditionalPanel(
          condition = paste(
            "input.pkInput == 'ode' ||",
            "(input.pkInput == 'pk' && (input.eliminationInput != 'lin' || input.absorptionInput == 'transit')) ||",
            "input.pdInput == 'ode' || input.pdInput == 'idr' ||",
            "(input.pdInput == 'link' && input.pkInput != 'linmat')"
          ),
          col_6(
            selectInput(
              inputId = 'advanODEInput',
              width = '100%',
              label = 'ODE solver',
              choices = c(
                'ADVAN6' = 6,
                'ADVAN8' = 8,
                'ADVAN9' = 9,
                'ADVAN13' = 13,
                'ADVAN14' = 14,
                'ADVAN15' = 15
              ),
              selected = 13
            )
          )
        ),
        conditionalPanel(
          condition = "input.pkInput == 'linmat' && input.pdInput != 'idr' && input.pdInput != 'ode'",
          col_6(
            selectInput(
              inputId = 'advanLinMatInput',
              width = '100%',
              label = 'ADVAN',
              choices = c('ADVAN5' = 5, 'ADVAN7' = 7),
              selected = 5
            )
          )
        ),
        col_6(
          numericInput(
            inputId = 'nsigInput',
            width = '100%',
            label = 'Number of significant digits',
            value = 3,
            min = 1,
            step = 1
          )
        )
      )
    )

  })

  #---- Ace toolbar ----

  output$aceToolbarUI <- renderUI({
    fluidRow(
      column(
        width = 12,
        rclipboard::rclipboardSetup(),
        shinyBS::bsButton(
          inputId = "lockButton",
          icon = icon("lock-open"),
          label = NULL,
          block = FALSE,
          type = "toggle",
          value = FALSE
        ),
        actionButton(
          inputId = "refreshButton",
          label = NULL,#"(Re)generate",
          icon = icon("sync")
        ),
        rclipboard::rclipButton(
          inputId = "copyButton",
          label = NULL,#"Copy to clipboard",
          clipText = input$aceNew,
          icon = icon("copy")
        ),
        downloadButton(
          outputId = "downloadButton",
          label = NULL,#"Download",
          icon = icon("download")
        ),
        actionButton(
          inputId = "linkButton",
          label = NULL,#"Keyboard shortcuts",
          icon = icon("keyboard"),
          onclick ="window.open('https://github.com/ajaxorg/ace/wiki/Default-Keyboard-Shortcuts', '_blank')"
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

  #---- Model code ----
  modelCode <- reactive({

    req( 'lockButton' %in% names(input) )

    if ( notTruthy(input$pkInput, input$pdInput) ){
      return(
        get_nonmem_code(
          input = input,
          template = template_nonmem,
          vars = mappedVars,
          varianceTable = NULL,
          covarianceBlock = NULL,
          rvTable = NULL
        )
      )
    } else {
      parameterTable <- hot_to_r(input$parameterTable)
      rvTable <- hot_to_r(input$rvTable)

      if (length(parameterTable$Parameter) != length(unique(parameterTable$Parameter))){
        return('Duplicates in parameter tables prevents the code generation.')
      } else {
        if (input$platformInput == "NONMEM") {
          return(
            get_nonmem_code(
              input = input,
              template = template_nonmem,
              vars = mappedVars,
              advan = advan,
              trans = trans,
              isPRED = isPRED,
              isODE = isODE,
              isLINMAT = isLINMAT,
              isPREDPP = isPREDPP,
              varianceTable = varianceTable_content(),
              covarianceBlock = covarianceCheck4()$data,
              rvTable = rvTable,
              parm_lib = parm_lib,
              model_lib = model_lib,
              rv_lib = rv_lib,
              scaling = scaling,
              replacement = TRUE #as.logical(input$subInput)
            )
          )
        } else if (input$platformInput == "mrgsolve") {
          # return(
          #   make_mrg_model(input = input,
          #                  template = mrg_template,
          #                  user = username,
          #                  date = date,
          #                  vars = final.vars,
          #                  advan = advan,
          #                  trans = trans,
          #                  isPRED = isPRED,
          #                  isODE = is_mrg_ODE,
          #                  isLINMAT = isLINMAT,
          #                  covTable = covCheck3()$data,
          #                  rvTable = rvTable,
          #                  parm_lib = parm_lib,
          #                  model_replacement = model_replacement,
          #                  rv_ref = rv_ref,
          #                  replacement = as.logical(input$subInput)
          #   )
          # )
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
    }
  })

  output$newCode <- newCode <- reactive({

    req(input$platformInput)

    dummy <- input$nmFlavorInput

    value <- modelCode()
    if (length(value) == 0){
      ''
    } else {
       paste(value, collapse = '\n')
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
            input$platformInput == 'NONMEM',
            'nmtran',
            'text'
          )
        )
      }
    }
  )

}

