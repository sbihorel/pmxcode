
.onLoad <- function(libname, pkgname) {

  packageStartupMessage('Welcome to pmxcode!')

  # Make R check happy when resources are used
  utils::globalVariables(
    c("parm_lib", "pdForm_lib", "labelunit_lib", "rv_lib", "scaling",
      "template_nonmem", "template_mrgsolve", "template_bm", "model_lib"
      )
  )

}
