
#### Instructions

This *pmxcode* module supports the creation of univariate models for
execution of stepwise covariate analysis in NONMEM. This is implemented
sequentially for each step of forward covariate selection and backward
elimination. For each step, 3 actions are required and are performed in
one of the tab panels:

- The selection of a reference model (in REFERENCE MODEL)
- The definition of parameter/covariate relationships (in COVARIATE
  EFFECTS)
- The creation of the univariate models (in UNIVARIATE MODELS)

#### Reference model

In this panel, you must upload a reference model for the intended step
of each step of forward covariate selection and backward elimination.
This is done using the SELECT REFERENCE MODEL button. Once uploaded, the
model code is shown in a text editing field that allows you to make any
modification you see fit.

Note that that this *pmxcode* module relies on the NONMEM coding and
commenting conventions implemented in its model library module. In
particular, it requires that the typical value of any parameter X is
defined by the TVX variable. Additionally, because the module assumes
independence between additive and multiplicative covariate effects, the
module requires that the intercept part of the typical value of any
parameter X is defined by the TVXI variable.

To facilitate the implementation of these requirements, at step 1 of
forward covariate selection, you can use the CONVERT button to
automatically modify the code of the uploaded reference model. This
modified version will be used as reference for creation of univariate
models instead of the uploaded code.

#### Definition of parameter/covariate relationships

Parameter/covariate relationships are defined for each step of the
forward selection or backward elimination stages using the table
provided in the interface. Since NONMEM execution can take time, the
covariate effect definitions can be saved (using the
<img src='www/download_grey.svg' width='13px'> button) in a .csv file
and later uploaded (using the
<img src='www/upload_grey.svg' width='13px'>) for further modifications.

The interface table includes multiple columns:

- Parameter and covariate must be entered as single variables in the
  eponymous columns (using alphanumeric characters only);
- The *Type* column defines whether the covariate is discrete or
  continuous variables;
- The *Functional form* column defines the mathematical form of the
  relationship: “Linear”, “Power”, or “Exponential” for continuous
  variables and “Additive”, “Proportional”, or “Direct proportional” for
  discrete variables;
- Any centering value of the covariate effect must be entered in the
  *Center value for continuous covariate*;
- The initial estimate of the covariate effect must be entered in the
  eponymous column;
- For discrete covariates with \> 2 categories, non-reference categories
  are each associated with a specific dichotomous flag and initial
  estimate. The flags must be entered in the *Dichotomous flags for
  discrete covariate* column separated by colons without spaces and
  estimates must be entered in a similar fashion (and corresponding
  order) in the *Initial estimate* column. For instance, to define the
  effects of renal function categories, one can enter
  “RFMILD:RFMOD:RFSEV” in the flag column and “0.1:0.2:0.3” in the
  estimate column.
- The *Action* column defines what action will be taken based upon the
  information entered in the row: “Create” to create a control stream,
  “Do not create” to perform no action, “Select” to indicate that the
  covariate effect was selected at a particular forward selection step,
  and “Remove” to indicate that the covariate effect to be removed at a
  particular backward elimination step.

Use the <img src='www/plus_grey.svg' width='13px'> and
<img src='www/minus_grey.svg' width='13px'> buttons to add and delete
rows in the interface table. Table content can also be copied
(<img src='www/copy_grey.svg' width='13px'>) and pasted
(<img src='www/paste_grey.svg' width='13px'>). You can also click on
<img src='www/duplicate_grey.svg' width='13px'> to create a new step by
duplicating the content of the current step (selected covariate effect
will be filtered out based upon the Action column selection). For step 1
of backward elimination, you can also click on
<img src='www/extract_grey.svg' width='13px'> to extract the list of
parameter/covariate relationships that were previously included in the
reference model.

Note that :

- The variables (parameters, covariates, and flags) must be defined in
  the NONMEM control stream in the \$INPUT, \$PRED, or \$PK block.
- If covariates and flags need to be defined in the control stream, this
  must be done prior to ;– COVARIATE EFFECT START.
- The downloaded .csv files will contain the following data column:
  *Stage*, *Step*, *Parameter*, *Covariate*, *Type*, *Function*,
  *Center*, *Flags*, *Initial*, and *Action*.

#### Creation of univariate models

Univariate models are automatically created by click on the CREATE
button after selecting the stage of analysis (“Forward selection” or
“Backward elimination”) and the step of analysis (1, 2, 3, etc.).
Optionally, you can define the folder in which the univariate model
files will be saved using the SELECT MODEL DIRECTORY. You will have to
download and save the files at this location using the EXTRACT ALL
button. This button only shows up after creation of the univariate runs.

Nota that, if the uploaded reference model uses PsN convention, the
univariate model files will be named runX.mod, with X being a 3 or
4-digit number starting at the value set in the field entitled “Run nº
start at”. If the uploaded reference model uses standard NONMEM
convention and a user-defined named, the univariate model files will use
a different naming convention:
\<prefix\>-\<parameter\>-\<covariate\>-\<function\>-\<number\>.ctl

where \<prefix\> is the value set in the FIlename prefix field,
\<parameter\> and \<covariate\> are the variables involved in the
relationship, \<function\> is either “lin”, “pow”, “exp”, “add”, “prop”
or “dirprop” based upon the selected functional form, and \<number\> is
a 2-digit number.

After creation of the univariate model code, you can see the content of
each file in LIST OF UNIVARIATE MODELS tab and look at checks performs
across all univariate models in the CHECKS tab.
