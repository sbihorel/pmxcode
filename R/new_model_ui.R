new_model_ui <- function(){
  tabPanel(
    title = "Library",
    icon = icon("school", verify_fa = FALSE),
    value = "new",
    fluidRow(
      col_7(
        navlistPanel(
          id = "new_menu",
          widths = c(2, 10),
          #---- Platform ----
          tabPanel(
            title = "Platform",
            wellPanel(
              fluidRow(
                col_12(
                  h4(strong("Select a modeling platform"))
                )
              ),
              fluidRow(
                col_6(
                  radioButtons(
                    inputId = "platformInput",
                    label = " ",
                    choices = c("NONMEM", "mrgsolve"), #"Berkeley Madonna", "mrgsolve"),
                    selected = "NONMEM"
                  )
                ),
                col_6(
                  conditionalPanel(
                    condition = "input.platformInput == 'NONMEM'",
                    radioButtons(
                      inputId = "nmFlavorInput",
                      label = " ",
                      choices = c("Standard style", "PsN/Xpose style"),
                      selected = "Standard style"
                    )
                  )
                )
              ),
              fluidRow(
                col_6(
                  textInput(
                    inputId = "platformVersionInput",
                    label = "Enter a version number (optional)",
                    width = "100%"
                  )
                )
              )
            )
          ),

          #---- Files ----
          tabPanel(
            title = "Files",
            wellPanel(
              uiOutput('filesUI')
            )
          ),

          #---- Mapping ----
          tabPanel(
            title = "Mapping",
            wellPanel(
              fluidRow(
                col_12(
                  uiOutput("mapNAUI"),
                  uiOutput("mapTableUI")
                )
              ),
              fluidRow(
                col_12(
                  uiOutput("mapDropUI"),
                  uiOutput("mapContVarUI"),
                  uiOutput("mapCatVarUI")
                )
              )
            )
          ),

          #---- PK model structure ----
          tabPanel(
            title = "Structure",
            wellPanel(
              h4(strong("Pharmacokinetic model")),
              fluidRow(
                col_12(
                  selectInput(
                    inputId = "pkInput",
                    width = "100%",
                    label = NULL,
                    choices = c(
                      "None" = "none",
                      "Defined by subroutines" = "pk",
                      "Defined by first-order rates" = "linmat",
                      "Defined by ODEs" = "ode",
                      "Defined by explicit solutions" = "pred"
                    ),
                    selected = "pk"
                  )
                )
              ),
              # First row for LINMAT and ODE model
              conditionalPanel(
                condition = "input.pkInput == 'linmat' | input.pkInput == 'ode'",
                fluidRow(
                  col_4( uiOutput("pknCMTUI") ),
                  col_4( uiOutput("pkDefaultDoseUI") ),
                  col_4( uiOutput("pkDefaultObsUI") )
                )
              ),
              # First row
              fluidRow(
                conditionalPanel(
                  condition = "input.pkInput == 'pk'",
                  col_4(
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
                  )
                ),
                uiOutput("ivDosingUI"),
                uiOutput("poDosingUI")
              ),
              # Second row
              fluidRow(
                conditionalPanel(
                  condition = "input.pkInput == 'pk'",
                  uiOutput("eliminationUI")
                ),
                conditionalPanel(
                  condition = "input.pkInput == 'pk' | input.pkInput == 'linmat' | input.pkInput == 'ode'",
                  uiOutput("ivRateUI"),
                  uiOutput("poRateUI"),
                  conditionalPanel(
                    condition = "input.poInput != 'sig'",
                    uiOutput("alagUI1")
                  )
                )
              ),
              # Third row
              fluidRow(
                conditionalPanel(
                  condition = "input.pkInput == 'pk' & (input.eliminationInput == 'mm' | input.eliminationInput == 'mmlin')",
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
                ),
                conditionalPanel(
                  condition = "input.pkInput == 'pk' & (input.eliminationInput == 'tmdd' | input.eliminationInput == 'tmddqe' | input.eliminationInput == 'tmddqer' | input.eliminationInput == 'tmddqss' | input.eliminationInput == 'tmddqssr')",
                  col_8(
                    uiOutput("tmddUI")
                  )
                ),
                conditionalPanel(
                  condition = "(input.pkInput == 'pk' | input.pkInput == 'linmat' | input.pkInput == 'ode') & input.poInput == 'sig'",
                  uiOutput("alagUI2")
                )
              ),
              uiOutput("warningDosingUI")
            ),

            #---- PD model structure ----
            wellPanel(
              h4(strong("Pharmacodynamic model")),
              fluidRow(
                col_12( uiOutput("pdUI") )
              ),

              # UI for functional form of direct effect, biophase, and exposure-response models
              conditionalPanel(
                condition = paste(
                  "input.pdInput == 'direct' | input.pdInput == 'biophase' |",
                  "input.pdInput == 'er'  | input.pdInput == 'logistic' | input.pdInput == 'ordcat'"),
                fluidRow(
                  conditionalPanel(
                    condition = "input.pdInput == 'ordcat'",
                    col_4( uiOutput("endpointUI") )
                  ),
                  col_4(
                    uiOutput("exposureVarUI"),
                    uiOutput("logisticDriverVarUI")
                  )
                ),
                fluidRow(
                  col_4( uiOutput("minCategoryUI") ),
                  col_4( uiOutput("maxCategoryUI") )
                ),
                fluidRow(
                  col_4( uiOutput("effectFormUI") ),
                  col_4( uiOutput("effectParmUI") ),
                  col_4( uiOutput("effectStimUI") )
                ),
                fluidRow(
                  col_12( uiOutput("effectMathjax") )
                )
              ),

              # Ui for functional form of IDR models
              conditionalPanel(
                condition = "input.pdInput == 'idr'",
                fluidRow(
                  col_6(
                    selectInput(
                      inputId = "idrTypeInput",
                      width = "100%",
                      label = "Drug model",
                      choices = c(
                        "Inhibition of production" = "idr1",
                        "Inhibition of elimination" = "idr2",
                        "Stimulation of production" = "idr3",
                        "Stimulation of elimination" = "idr4"
                      ),
                      selected = "idr1"
                    ),
                    uiOutput("idrStimUI")
                  ),
                  col_6(
                    selectInput(
                      inputId = "idrParmInput",
                      width = "100%",
                      label = "Estimated parameters",
                      choices = c(
                        "R(0), KOUT" = 1,
                        "KIN, KOUT" = 2,
                        "R(0), KIN" = 3
                      ),
                      selected = 1
                    )
                  )
                ),
                fluidRow(
                  col_12( uiOutput("idrMathjax") )
                )
              ),

              # Ui for driver of drug effect
              conditionalPanel(
                condition = "input.pdInput == 'direct' | input.pdInput == 'biophase' | input.pdInput == 'idr'",
                fluidRow(
                  col_6( uiOutput("effectDriverUI") ),
                  col_6(
                    selectInput(
                      inputId = "effectCpDriverInput",
                      width = "100%",
                      label = "Effect driver",
                      choices = c(
                        "Concentration" = TRUE,
                        "Amount" = FALSE
                      ),
                      selected = TRUE
                    )
                  )
                )
              ),

              # UI for number of PD compartments
              conditionalPanel(
                condition = "input.pdInput == 'ode'",
                fluidRow(
                  col_6(
                    numericInput(
                      inputId = "pdnCMTInput",
                      width = "100%",
                      label = "Number of compartments",
                      min = 1,
                      value = 1,
                      step = 1
                    )
                  )
                )
              )
            ),

            wellPanel(
              h4(strong('Coding options')),
              fluidRow(
                col_6(
                  uiOutput("advanUI")
                ),
                col_6(
                  uiOutput("transUI")
                )
              )
            )
          ),

          #---- Parameters ----
          tabPanel(
            title = "Parameters",
            wellPanel(
              uiOutput("parameterWarningUI"),
              uiOutput("parameterUI"),
              uiOutput("warningParameterUI"),
              uiOutput("importParameterUI")
            )
          ),

          #---- Covariance ----
          tabPanel(
            title = "Covariance",
            wellPanel(
              uiOutput("varianceWarningUI"),
              uiOutput("varianceUI")
            )
          ),

          #---- RV ----
          tabPanel(
            title = "RV",
            wellPanel(
              uiOutput('residualWarningUI'),
              uiOutput('rvUI'),
              uiOutput('rvTableUI'),
              uiOutput('rvFlagUI'),
              uiOutput('rvWarningUI')
            )
          ),

          #---- Tasks ----
          tabPanel(
            title = "Tasks",
            wellPanel(
              conditionalPanel(
                condition = "input.platformInput != 'NONMEM'",
                HTML_info("Tasks cannot be defined for the selected software platform")
              ),
              uiOutput("taskUI")
            )
          ),

          #---- Scaling ----
          tabPanel(
            title = "Scaling",
            wellPanel(
              uiOutput("scalingUI")
            )
          )
        )
      ),

      #---- ACE editor ----
      col_5(
        uiOutput('aceToolbarUI'),
        p(),
        shinyAce::aceEditor(
          outputId = "aceNew",
          mode = "plain_text",
          theme = "crimson_editor",
          height = "775px",
          fontSize = 12,
          wordWrap = TRUE
        )
      )
    )
  )
}
