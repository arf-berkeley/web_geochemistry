library(shiny)
library(ggplot2)
#library(ggiraph)
library(shinyBS)

source("function_library.R")

fluidPage(

  titlePanel("Geochemical Visualization – South America"),

  sidebarPanel(

    selectInput('region', 'Region', unique(source_data()$Site_Country), "Peru"),
    bsCollapse(id = "sources",# open = "Panel 2",
               bsCollapsePanel("Choose Obsidian Sources", #"temp", style = "warning")),
                               "MURR Neutron Activation Analysis",
                               #htmlOutput("MURR Neutron Activation Analysis"),
                               uiOutput("source_checkbox_group"),
                               style = "warning")),
    #),
    #selectInput('type', 'Type', c("Source")),
    checkboxInput('show_source_data', "Show source datapoints"),
    # Disabling this because tooltip shows the same info.
    #checkboxInput('label_source_points', "Show source labels"),

    selectInput('element1', 'Horizontal element (X)', elements, "Rb"),
    selectInput('element2', 'Vertical element (Y)', elements, "Sr"),
#    actionButton("plot", label = "Plot"),
    tags$hr(),

    fileInput('file1', 'Upload artifact data', multiple=F,
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),

    checkboxInput('plot_artifact_points', "Plot artifact points", T),
    checkboxInput('plot_artifact_labels', "Label artifact points"),
    checkboxInput('plot_artifact_ellipses', "Plot artifact ellipses")
  ),

  mainPanel(
    #plotOutput('plot'),
    # this is an extra div used ONLY to create positioned ancestor for tooltip
    # we don't change its position
    #div(
      #style = "position:relative",
      #ggiraph::ggiraphOutput("plot")
      plotly::plotlyOutput("plot")
      #verbatimTextOutput("x")

      #plotOutput("plot")#,
                 #hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
      #uiOutput("hover_info")
    #)
  )
)
