### Shiny Libraries
library("shiny")
library("DT") # Interactive datatable using jQuery's Datatables
library("shinyBS") # Collapsable panels and many other bootstrap interfaces
library("shinyalert") # Alert related modal pop-ups
library("shinyjs") # Javascript support in Shiny
### This required for shinyapps.io to use extendedShinyjs() from the "shinyjs" package
### Also see "Note" section of "extendShinyjs" function definition in "shinyjs" package documentation:
### 	https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf
library("V8") ### https://github.com/daattali/shinyjs/issues/20

### Plotting
library("ggplot2")
library("plotly")

### Utilities
library("dplyr") # Use of the '%>%' operator, and much more
### Example of '%>%' usage: http://genomicsclass.github.io/book/pages/dplyr_tutorial.html#pipe-operator-
library("glue")
library("readxl") # For reading Microsoft Excel files

### Local scripts
source("config.R")
source("dynamic.R")

print(glue("\n\nBeginning new interation..."))
print(glue("--global.R"))