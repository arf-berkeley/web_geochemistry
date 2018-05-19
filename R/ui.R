print(glue("--ui.R"))

fixedPage(
	includeCSS("styles.css"),
	shinyjs::useShinyjs(),
	shinyjs::extendShinyjs(script="interactive.js"),
	shinyalert::useShinyalert(),
	# shinyjs::extendShinyjs(text="jsCode"),
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
		shinyBS::bsCollapse(id="sidepanel", open=c("Source Information", "Data Management"), multiple=TRUE,
			shinyBS::bsCollapsePanel(title="Source Information", style="info",
				selectInput(inputId='country',
					label='Country (# Sources)',
					choices=country_labels,
					selected=country_labels[0] ### Peru
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

			shinyBS::bsCollapsePanel(title="Data Management", style="info",
				tags$div(class="my-center",
					actionButton("view_uploaded_files", "View uploaded files", class="my-btn"),
					shinyBS::bsModal(id="upload_modal",
						title="Change name",
						trigger="view_uploaded_files",
						size="large",
						uiOutput("upload_interface"),
						tags$div(style="margin-top:10px;margin-bottom:10px;margin-left:0px;margin-right:0px;padding:0px 5px 5px;border:1px solid black;border-radius:5px;",
							DT::DTOutput("upload_preview")
						),
						uiOutput("upload_type_interface"),
						uiOutput("upload_sample_id_column_interface"),
						uiOutput("upload_group_column_interface"),
						uiOutput("upload_element_column_interface"),
						uiOutput("upload_note_column_interface"),
						uiOutput("upload_show_interface")
					)
				),

				tags$b("Upload data"),
				fileInput(inputId='upload_files',
					label=NULL,
					multiple=TRUE,
					accept=c('text/csv',
						'text/comma-separated-values,text/plain',
						'.csv'
					)
				),

				# tags$b("Download updated data"),
				tags$div(class="my-center",
					actionButton("download_table", "Download table", class="my-btn")
				)
			),

			shinyBS::bsCollapsePanel(title="Options", style="info",
				checkboxInput(inputId='show_source_points',
					label="Show source datapoints",
					value=FALSE),
				checkboxInput(inputId='show_source_info',
					label="Show source information",
					value=FALSE),
				checkboxInput(inputId='include_source_data',
					label="Include source data in table",
					value=FALSE)

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
		### Controlling the style using 'div.dataTables_wrapper' in 'styles.css'
		# shinyBS::bsCollapse(id="table_panel",
		# 	shinyBS::bsCollapsePanel(title="View table", style="info",
		# 		fixedRow(DT::DTOutput("table"))
		# 	)
		# )
		# fixedRow(DT::DTOutput("table")),
		# tags$div(#id="mine", style="border: 1px solid black;width:200px;",
		actionButton("clear_selected", "Clear selected")
		# ),
	)
)