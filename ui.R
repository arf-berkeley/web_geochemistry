print("Running 'ui.R'.")

library("shiny")
# library("DT")

source("function_library.R")

fixedPage(
	titlePanel("Geochemical Visualization – South America"),

	fixedRow(
		column(3,
			fixedRow(style="margin:0px 0px 10px;border-radius:20px;padding:0px 10px 0px;background-color:#F0F8FF;border:1px solid #A9A9A9;",
				tags$h4(tags$ul(tags$u("Source Information"))),
				# tags$hr(),

				selectInput('region', 'Region', unique(source_data()$Site_Country), "Peru"),

				shinyBS::bsCollapse(id="sources",
					shinyBS::bsCollapsePanel(title="Choose Obsidian Sources",
						"MURR Neutron Activation Analysis",
						uiOutput("source_checkbox_group"),
						style="primary")
					),

				checkboxInput(inputId='show_source_data',
					label="Show source datapoints",
					value=FALSE),

				### Selection for dependent variables
				selectInput(inputId='element1',
					label='Horizontal element (X)',
					choices=elements,
					selected="Rb"),
				selectInput(inputId='element2',
					label='Vertical element (Y)',
					choices=elements,
					selected="Sr")
			),

			# fixedRow(style="background-color:green;"),

			fixedRow(style="margin:10px 0px 0px;border-radius:20px;padding:0px 10px 0px;background-color:#F0F8FF;border:1px solid #A9A9A9;",
				### Just a horizontal line across the UI (part of shiny::tags - HTML Tags)
				### https://shiny.rstudio.com/articles/html-tags.html
				# tags$hr(),
				tags$h4(tags$ul(tags$u("Artifact Information"))),

				radioButtons(inputId='selection',
					label="Upload Data",
					choices=list("Source", "Artifact"),
					inline=TRUE
				),
				fileInput('file1',
					label=NULL,
					multiple=FALSE,
					accept=c('text/csv',
						'text/comma-separated-values,text/plain',
						'.csv')
					),

				checkboxInput(inputId='plot_artifact_points',
					label="Plot artifact points",
					value=TRUE),
				checkboxInput(inputId='plot_artifact_labels',
					label="Label artifact points",
					value=FALSE),
				checkboxInput(inputId='plot_artifact_ellipses',
					label="Plot artifact ellipses",
					value=FALSE)
			)
		),

		column(9, #style="background-color:#F0F8FF;border:1px solid #A9A9A9;",
			fixedRow(
				column(1),
				column(10,
					plotly::plotlyOutput("plot")
				),
				column(1)
			),
			fixedRow(
				# style="background-color:#F0F8FF;border:1px solid #A9A9A9;"
				DT::dataTableOutput('table')
			)
		)
	)
)