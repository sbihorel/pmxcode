
resources <- function(what){

  list(
    # Model parameter library
    `parm_lib` = jsonlite::fromJSON(
      system.file("resources/parm_lib.json", package = "pmxcode")
    ),
    # Library of PD model expressions
    `pdForm_lib` = jsonlite::fromJSON(
      system.file("resources/pdForm_lib.json", package = "pmxcode")
    ),
    # Label and Unit library
    `labelunit_lib` = jsonlite::fromJSON(
      system.file("resources/labelunit_lib.json", package = "pmxcode")
    ),
    # RV library
    `rv_lib` = jsonlite::fromJSON(
      system.file("resources/rv_lib.json", package = "pmxcode")
    ),
    # Dose unit scaling library
    `scaling` = jsonlite::fromJSON(
      system.file("resources/scaling.json", package = "pmxcode")
    ),
    # Platform-specific templates
    `template_nonmem` = scan(
      file = system.file("resources/template_nonmem.txt", package = "pmxcode"),
      what = "character",
      sep = "\n",
      quiet = TRUE,
      blank.lines.skip = FALSE
    ),
    `template_mrgsolve` = scan(
      file = system.file("resources/template_mrgsolve.txt", package = "pmxcode"),
      what = "character",
      sep = "\n",
      quiet = TRUE,
      blank.lines.skip = FALSE
    ),
    `bm_template` = scan(
      file = system.file("resources/template_bm.txt", package = "pmxcode"),
      what = "character",
      sep = "\n",
      quiet = TRUE,
      blank.lines.skip = FALSE
    ),
    # Model structure tag replacement library
    `model_lib` = jsonlite::fromJSON(
      system.file("resources/model_lib.json", package = "pmxcode")
    )
  )

}
