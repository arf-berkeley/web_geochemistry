### Dependencies:
### 	shiny, readxl, glue, shinyBS, plotly, dplyr, DT
### Install all of these using:
### 	install.packages(c("shiny", "readxl", "glue", "shinyBS", "plotly", "dplyr", "DT"))

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
		tags$div(style=my_sidepanel_style,
			tags$div(
				"Weclome to the online geochemical visualization tool by UC Berkeley's Archaeological Research Facility!"
			),
			tags$div(style="margin-top:5px;margin-bottom:3px;",
				"Learn about the tool", a("here", href='help.html', target='blank')
			)
		),
		shinyBS::bsCollapse(id="sidepanel", open=c("Source Information", "File Management"), multiple=TRUE,
			shinyBS::bsCollapsePanel(title="Source Information", style="info",
				selectInput(inputId='country',
					label='Country (# Sources)',
					choices=country_labels,
					selected=country_labels[6] ### Peru
				),

				shinyBS::bsCollapse(id="sources",
					shinyBS::bsCollapsePanel(title="Select Sources", style="default",
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
					choices=sort(names(elements)),
					selected="Rubidium"
				),

				selectInput(inputId='element2',
					label='Vertical element (Y)',
					choices=sort(names(elements)),
					selected="Strontium"
				)
			),

			shinyBS::bsCollapsePanel(title="File Management", style="info",
				tags$b("Upload source or artifact data"),
				tags$div(class="my-center",
					radioButtons(inputId='upload_format_type',
						label=NA,
						choices=list("Source", "Artifact"),
						inline=TRUE
					),
					fileInput(inputId='upload_files',
						label=NULL,
						multiple=TRUE,
						accept=c('text/csv',
							'text/comma-separated-values,text/plain',
							'.csv'
						)
					)
				),

				tags$div(style="margin-bottom:16px;",
					conditionalPanel(
						# condition="typeof input.upload_files == 'object'",
						condition='typeof input["upload_files"] == "object"',
						# condition="input.upload_format_type == 'Source'",
						# condition='output["test"]',
						# 'true',
						shinyBS::bsCollapsePanel(title="View uploaded files", style="default",
							"Uploaded files go here!"
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
				checkboxInput(inputId='show_source_points',
					label="Show source datapoints",
					value=FALSE),
				checkboxInput(inputId='show_source_info',
					label="Show source information",
					value=FALSE)

				# checkboxInput(inputId='plot_artifact_points',
				# 	label="Plot artifact points",
				# 	value=TRUE),
				# checkboxInput(inputId='plot_artifact_labels',
				# 	label="Label artifact points",
				# 	value=FALSE),
				# checkboxInput(inputId='plot_artifact_ellipses',
				# 	label="Plot artifact ellipses",
				# 	value=FALSE)
			)
		)
	),

	column(9, #style="background-color:#F0F8FF;border:1px solid #A9A9A9;",
		fixedRow(plotly::plotlyOutput("plot", height="350px", width="auto")),
		# renderText("Hello World.")
		### Controlling the style using 'div.dataTables_wrapper' in 'styles.css'
		# shinyBS::bsCollapse(id="table_panel",
		# 	shinyBS::bsCollapsePanel(title="View table", style="info",
		# 		fixedRow(DT::DTOutput("table"))
		# 	)
		# )
		fixedRow(DT::DTOutput("table"))
	)
)