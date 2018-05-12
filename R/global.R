### Dependencies:
### 	shiny, readxl, glue, shinyBS, plotly, dplyr, DT
### Install all of these using:
### 	install.packages(c("shiny", "readxl", "glue", "shinyBS", "plotly", "dplyr", "DT"))

### Shiny Libraries
library("shiny")
library("DT") # Interactive datatable using jQuery's Datatables
library("shinyBS") # Collapsable panels and many other bootstrap interfaces
library("shinyjs") # Javascript supprot in Shiny
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

### Local scripts
source("config.R")
source("dynamic.R")

print(glue("\n\nBeginning new interation..."))
print(glue("--global.R"))