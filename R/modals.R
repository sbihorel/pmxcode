modal_reference_creation <- function(){
  showModal(
    modalDialog(
      title = 'Model import',
      p(
        paste(
          'A new model code is about to be imported. Any edit(s) made in the left editor panel will',
          'be lost. How do you want to proceed?'
        )
      ),
      footer = tagList(
        actionButton(
          inputId = 'importModalOKBtn', icon = icon('check'), label = 'Import the new code'
        ),
        modalButton(icon = icon('times'), label = 'Keep my current code for now')
      )
    )
  )
}

creation_mod_modal <- function(){
  showModal(
    modalDialog(
      title = 'Model preparation',
      p(
        paste(
          'A model code is about to be modified. Any edit(s) made in the right editor panel will',
          'be lost. How do you want to proceed?'
        )
      ),
      footer = tagList(
        actionButton(
          inputId = 'modifyModalOKBtn', icon = icon('check'), label = 'Modify the new code'
        ),
        modalButton(icon = icon('times'), label = 'Keep my current code for now')
      )
    )
  )
}
