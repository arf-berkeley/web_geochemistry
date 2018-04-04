print("")
print("")
print("Beginning new interation...")
print("Running 'ui.R'.")

library("shiny")
# library("DT")

source("config.R")

fixedPage(
	titlePanel("Geochemical Visualization – South America"),

	fixedRow(
		column(3,
			tags$h4(tags$ul(tags$u("Source Information"))),
			fixedRow(style=my_sidepanel_style,
				selectInput(inputId='country',
					label='Country',
					choices=country_labels,
					# selected=country_labels[0]
					selected="Peru (324)"
				),

				shinyBS::bsCollapse(id="sources",
					shinyBS::bsCollapsePanel(title="Choose Obsidian Sources",
							# label="MURR Neutron Activation Analysis",
							style="default",
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
					selected="Rb"),
				selectInput(inputId='element2',
					label='Vertical element (Y)',
					choices=elements,
					selected="Sr")
			),

			tags$h4(tags$ul(tags$u("Artifact Information"))),
			fixedRow(style=my_sidepanel_style,
				### Just a horizontal line across the UI (part of shiny::tags - HTML Tags)
				### https://shiny.rstudio.com/articles/html-tags.html

				# p(class='text-center',
				radioButtons(inputId='input_selection',
					label="Upload Data",
					choices=list("Source", "Artifact"),
					inline=TRUE
				),
				# ),

				fileInput('file1',
					label=NULL,
					multiple=FALSE,
					accept=c('text/csv',
						'text/comma-separated-values,text/plain',
						'.csv'
					)
				),

				# tags$hr(),

				radioButtons(inputId='output_selection',
					label="Download Data",
					choices=list("CSV", "XLSX", "ODS"),
					inline=TRUE
				),
				p(class='text-center',
					downloadButton(outputId='x3',
						label="Download Updated Table"
					)
				)
			),

			tags$h4(tags$ul(tags$u("Plot Options"))),
			fixedRow(style=my_sidepanel_style,
				### Just a horizontal line across the UI (part of shiny::tags - HTML Tags)
				### https://shiny.rstudio.com/articles/html-tags.html
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
				DT::DTOutput("table")
			)
		)
	)
)