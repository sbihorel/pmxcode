
resources <- function(what){

  if ( "pmxcode" %in% installed.packages() ){
    pmxcode_file <- function(x){
      system.file(x, package = "pmxcode")
    }
  } else {
    pmxcode_file <- function(x){
      file.path('./inst', x)
    }
  }

  list(
    # Model parameter library
    `parm_lib` = jsonlite::fromJSON(
      pmxcode_file("resources/parm_lib.json")
    ),
    # Library of PD model expressions
    `pdForm_lib` = jsonlite::fromJSON(
      pmxcode_file("resources/pdForm_lib.json")
    ),
    # Label and Unit library
    `labelunit_lib` = jsonlite::fromJSON(
      pmxcode_file("resources/labelunit_lib.json")
    ),
    # RV library
    `rv_lib` = jsonlite::fromJSON(
      pmxcode_file("resources/rv_lib.json")
    ),
    # Dose unit scaling library
    `scaling` = jsonlite::fromJSON(
      pmxcode_file("resources/scaling.json")
    ),
    # Platform-specific templates
    `template_nonmem` = scan(
      file = pmxcode_file("resources/template_nonmem.txt"),
      what = "character",
      sep = "\n",
      quiet = TRUE,
      blank.lines.skip = FALSE
    ),
    `template_mrgsolve` = scan(
      file = pmxcode_file("resources/template_mrgsolve.txt"),
      what = "character",
      sep = "\n",
      quiet = TRUE,
      blank.lines.skip = FALSE
    ),
    `bm_template` = scan(
      file = pmxcode_file("resources/template_bm.txt"),
      what = "character",
      sep = "\n",
      quiet = TRUE,
      blank.lines.skip = FALSE
    ),
    # Model structure tag replacement library
    `model_lib` = jsonlite::fromJSON(
      pmxcode_file("resources/model_lib.json")
    )
  )

}
