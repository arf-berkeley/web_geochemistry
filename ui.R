# Dependencies:
# 	shiny, readxl, glue, shinyBS, plotly, dplyr, DT
# Install all of these using:
# 	install.packages(c("shiny", "readxl", "glue", "shinyBS", "plotly", "dplyr", "DT"))

print("")
print("")
print("Beginning new interation...")
print("Running 'ui.R'.")

library("shiny")
# library("DT")

source("config.R")

fixedPage(
	includeCSS("styles.css"),
	titlePanel("Geochemical Visualization – South America"),

	column(3,
		shinyBS::bsCollapse(id="sidepanel", open=c("Source Information", "File Management"), multiple=TRUE,
			shinyBS::bsCollapsePanel(title="Source Information", style="info",
				selectInput(inputId='country',
					label='Country (# Sources)',
					choices=country_labels,
					# selected=country_labels[0]
					selected="Peru (324)"
				),

				shinyBS::bsCollapse(id="sources",
					shinyBS::bsCollapsePanel(title="Choose Obsidian Sources", style="default",
							uiOutput("source_selection")
					)
				),
				# selectInput(inputId='sources',
				# 	label='Sources',
				# 	choices=output$sources,
				# 	selected=output$source_selection
				# ),

				### Selection for dependent variables
				selectInput(inputId='element1',
					label='Horizontal element (X)',
					choices=elements,
					selected="Rb"
				),

				selectInput(inputId='element2',
					label='Vertical element (Y)',
					choices=elements,
					selected="Sr"
				)
			),

			shinyBS::bsCollapsePanel(title="File Management", style="info",
				tags$b("Upload source or artifact data"),
				tags$div(class="my-center",
					radioButtons(inputId='input_selection',
						label=NA,
						choices=list("Source", "Artifact"),
						inline=TRUE
					),
					fileInput('file1',
						label=NULL,
						multiple=FALSE,
						accept=c('text/csv',
							'text/comma-separated-values,text/plain',
							'.csv'
						)
					)
				),

				tags$b("Download updated table"),
				tags$div(class="my-center",
					radioButtons(inputId='output_selection',
						label=NA,
						choices=list("CSV", "XLSX", "ODS"),
						inline=TRUE
					),
					downloadButton(outputId='x3',
						label="Download"
					)
				)
			),

			shinyBS::bsCollapsePanel(title="Options", style="info",
				checkboxInput(inputId='show_source_data',
					label="Show source datapoints",
					value=FALSE),

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
		)
	),

	column(9, #style="background-color:#F0F8FF;border:1px solid #A9A9A9;",
		fixedRow(plotly::plotlyOutput("plot", height="350px", width="auto")),
		
		### Controlling the style using 'div.dataTables_wrapper' in 'styles.css'
		fixedRow(DT::DTOutput("table"))
	)
)