new_model_ui <- function(){
  shiny::tabPanel(
    title = "New",
    icon = icon("square-plus", verify_fa = FALSE),
    value = "new",
    fluidRow(
      col_6(
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
                    choices = c("NONMEM", "Berkeley Madonna", "mrgsolve"),
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

          #---- PK model - Structure ----
          tabPanel(
            title = "Structure",
            wellPanel(
              h4(strong("Pharmacokinetic model")),
              fluidRow(
                column(
                  width = 12,
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
              conditionalPanel(
                condition = "input.pkInput == 'pk'",
                fluidRow(
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
                )
              ),
              conditionalPanel(
                condition = "input.pkInput == 'linmat' | input.pkInput == 'ode'",
                fluidRow(
                  col_4( uiOutput("pknCMTUI") ),
                  col_4( uiOutput("pkDefaultDoseUI") ),
                  col_4( uiOutput("pkDefaultObsUI") )
                )
              ),
              conditionalPanel(
                condition = "input.pkInput == 'pk'",
                fluidRow(
                  uiOutput("absorptionUI"),
                  uiOutput("pkZeroEstUI"),
                  uiOutput("alagUI")
                ),
                conditionalPanel(
                  condition = "input.pkInput == 'pk'",
                  fluidRow(
                    col_4(
                      uiOutput("eliminationUI")
                    ),
                    conditionalPanel(
                      condition = "input.eliminationInput == 'mm' | input.eliminationInput == 'mmlin'",
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
                      condition = "input.eliminationInput == 'tmdd' | input.eliminationInput == 'tmddqe' | input.eliminationInput == 'tmddqer' | input.eliminationInput == 'tmddqss' | input.eliminationInput == 'tmddqssr'",
                      col_8(
                        uiOutput("tmddUI")
                      )
                    )
                  )
                )
              )
            ),

            #---- PD model structure ----
            wellPanel(
              h4(strong("Pharmacodynamic model")),
              fluidRow(
                col_12( uiOutput("pdUI") )
              ),

              # UI for functional form of direct effect, link, and exposure-response models
              conditionalPanel(
                condition = paste(
                  "input.pdInput == 'direct' | input.pdInput == 'link' |",
                  "input.pdInput == 'er'  | input.pdInput == 'logis' | input.pdInput == 'ordcat'"),
                fluidRow(
                  conditionalPanel(
                    condition = "input.pdInput == 'ordcat'",
                    col_4( uiOutput("endpointUI") )
                  ),
                  col_4(
                    uiOutput("exposureVarUI"),
                    uiOutput("logisDriverVarUI")
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
                condition = "input.pdInput == 'direct' | input.pdInput == 'link' | input.pdInput == 'idr'",
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
              uiOutput("parmsUI"),
              uiOutput("duplicateParmWarningUI")
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

          #---- Misc. ----
          tabPanel(
            title = "Misc.",
            wellPanel(
              uiOutput("scalingUI")
            ),
            uiOutput("optionsUI")
          )
        )
      ),

      #---- ACE editor ----
      col_6(
        uiOutput('aceToolbarUI'),
        p(),
        shinyAce::aceEditor(
          outputId = "aceNew",
          mode = "plain_text",
          theme = "crimson_editor",
          height = "800px",
          fontSize = 12,
          wordWrap = TRUE
        )
      )
    )
  )
}
