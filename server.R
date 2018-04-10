print("Running 'server.R'.")

library("dplyr")
library("ggplot2")

### No need because both are already in 'ui.R', which is loaded first
# library("shiny") 
source("config.R")
print("")
print("")

server = function(input, output) {

	info = observe({
		cat("\n", Sys.time(), "\n")
		cat("Current Country:\n")
		print(current_country())
		available_source_counts()
		cat("Current Sources:\n")
		print(current_sources())
		print(glue::glue("Current Elements: {element_x()} {element_y()}"))
		cat("\n")
	})

	current_df = reactive({
		temp = data %>% filter(Site.Country == current_country()) %>% filter(Source.Name %in% current_sources())
		temp = temp %>% filter(element_x() > 0 & element_y() > 0)
		temp = temp %>% group_by(Source.Name)
		return(temp)
	})

	current_country = reactive({
		# cat("\tEvaluating current_country()\n")
		return(extract_count_label(input$country))
	})

	available_source_counts = reactive({
		# Selects the 'sources' from a given 'country'
		# Used for the shinyBS::bsCollapsePanel() display
		#
		# Reacts to:
		#	input$country
		# Outputs:
		#	output$source_selection
		# cat("\tEvaluating available_source_counts()\n")
		# print(glue::glue("Observing updated country: '{current_country()}'")) # Status message for observe() event

		### Filters data by the 'country', then selects 'Source.Name', counts all the unique entires using 'table()', then converts to a Data.Frame
		sources_df = data %>% filter(Site.Country == current_country()) %>% select(Source.Name) %>% table %>% data.frame
		colnames(sources_df) = c("Source.Name", "Count")

		### Filters the 'Source.Name' that appear less than three times or are empty
		present_sources = sources_df %>% filter(Count >= 3 & Source.Name != "")
		# print(present_sources)
		return(present_sources)
	})

	initial_sources = reactive({
		# cat("\tEvaluating initial_sources()\n")
		initial_sources = available_source_counts() %>% arrange(desc(Count)) %>% head(5)
		return(initial_sources)
	})

	current_sources = reactive({
		# cat("\tEvaluating current_sources()\n")
		if (is.null(input$selected_source_labels)) {
			current_sources = vectorof_count_labels(initial_sources())
		} else {
			current_sources = input$selected_source_labels
		}
		return(extract_count_label(current_sources))
	})

	element_x = reactive({
		return(input$element1)
	})

	element_y = reactive({
		return(input$element2)
	})

	output$source_selection = renderUI({
		# cat("\tEvaluating output$source_selection renderUI()\n")
		checkboxGroupInput("selected_source_labels",
			label = "Available sources",
			choices = vectorof_count_labels(available_source_counts()),
			selected = vectorof_count_labels(initial_sources())
		)
	})

	selected_points = reactive({
		### This makes the table reactive to points being selected
		selection = input$table_rows_selected
		data = current_df() %>% ungroup()

		selected_data = data %>% filter(row_number() %in% selection)
		print("")
		print(select(selected_data, "ANID", element_x(), element_y(), "Source.Name"))
		print(typeof(selected_data))
		print(selection)
		# if (length(selection) > 0) {
		# 	print(data[selection,])
		# 	print(" ")
		# }
		return(selected_data)
	})

	add_selected_points = reactive({
		geom_point(
			data=selected_points(),
			aes_string(x=element_x(), y=element_y(), fill="Source.Name"), 
			size=2,
			label="Selected",
			colour="black",
			fill="red",
			shape=21 # To get the outside border
		)
		# scale_fill_manual(name = "", values="black", label="Your Selection")
	})

	#### Create the table
	### https://shiny.rstudio.com/articles/datatables.html
	### https://rstudio.github.io/DT/shiny.html
	## DT::renderDataTable() takes an expression that returns a rectangular data object with column names, such as a data frame or a matrix.
	output$table <- DT::renderDT({
		# print("Running 'renderDataTable()'.")
		# data = get_data()
		# print(typeof(iris))
		# print(typeof(data))
		# data
		if (!is.null(input$file1)) {
			# print("     Uploaded file!")
			# print(input$file1)
			# tab = read.csv("inbound/obsidian-NAA-database.csv")
			tab = current_df()
			# tab
		} else {
			# print("     No uploaded file...")
			# tab = data.frame(Sample=character(),
				# stringsAsFactors=FALSE)
			tab = current_df()
		}

		tab = tab %>% select(ANID, Site.Country, Source.Name, Long.Date, input$element1, input$element2, NKT_edits)

		# cat("Selected data:")
		# print(input$table_cell_clicked)
		# print(output)

		DT::datatable(tab,
			class="compact hover cell-border",
			rownames=FALSE,
			# filter="top",
			extensions="FixedHeader", #https://rstudio.github.io/DT/extensions.html#fixedheader
			options=list(
				dom='fti',
				columnDefs=list(list(
					className="dt-center", targets="_all"
				)),
				pageLength=-1,
				### 'fixedHeader' is not working and is likely a bug
				### https://github.com/rstudio/DT/issues/389
				fixedHeader=TRUE
			),
			editable=TRUE
		)
	})

	element_plot = reactive({
		data = current_df()

		### Example of '%>%' usage: http://genomicsclass.github.io/book/pages/dplyr_tutorial.html#pipe-operator-
		# ellipse = data %>% group_by(Source.Name) %>% filter(n() >= 3)
		# ellipse = na.omit(ellipse) %>% group_by(Source.Name)
		print(element_x())
		print(typeof(element_x()))
		p = ggplot(data, mapping=aes_string(x=element_x(), y=element_y(), group="Source.Name", color="Source.Name")) +
			stat_ellipse() +
			geom_point(size=2) +
			# xlab(names(elements)[elements == input$element1]) +
			# ylab(names(elements)[elements == input$element2]) +
			theme_minimal() #+
			# guides(color=guide_legend(nrow=2)) +
			theme(#plot.background = element_rect(color = "#e3e3e3", fill="#f7f7f7"),
				panel.background = element_rect(fill="white"),
				legend.position = "bottom",
				legend.title = element_blank(),
				panel.border = element_rect(fill=NA, colour="#e3e3e3")
			)

		# # Re-enable this once ellipses are working.
		if (input$show_source_data) {
			p = p + geom_point(mapping=aes(tooltip = tooltip, onclick = "",
											data_id = 1:nrow(ellipse)))#, data_id = ANID))
		}

		# Handle uploaded data if there is any.
		if (!is.null(input$file1)) {
			upload = read.csv(input$file1$datapath)
				if (!"SiteID" %in% names(upload)) {
					upload$SiteID = ""
				}

			upload = upload[, c(input$element1, input$element2, "SampleID", "SiteID")]
			colnames(upload) = c("X", "Y", colnames(upload)[3:4])
			#upload$tooltip = paste0("Sample ID: ", upload$SampleID,
			#        ifelse(upload$SiteID == "", "", paste0("<br />Site ID: ", upload$SiteID)))
			#p = p + geom_point_interactive(aes(x=x, y=y, data=upload))
			#p = p + geom_point_interactive(data=upload,

			#######
			if (input$plot_artifact_points) {
				p = p +
					geom_point(data=upload,
						mapping=aes(x=X, y=Y, group=SiteID, #label=SampleID,
							color=factor(SiteID, labels=paste("Site", unique(SiteID))),
							alpha=0.8, label=NULL)
					) +
					guides(alpha=FALSE) +
					scale_color_discrete()

				if (input$plot_artifact_ellipses) {
					p = p +
						stat_ellipse(data=upload,
							aes(x=X, y=Y,
								group=SiteID,
								color=factor(SiteID, labels=paste("Site", unique(SiteID))
								)
							)
						)
				}

				if (input$plot_artifact_labels) {
					p = p + geom_text(data = upload,
					mapping = aes(x = X, y = Y, group = SiteID,
					color = factor(SiteID),
					label = SampleID),
					check_overlap = T, size = 2.5, show.legend = F)
				}
			}
		}

		cat("\tLength: ", !is.null(input$table_rows_selected), "\n")
		if (!is.null(input$table_rows_selected)) {
			p = p + add_selected_points()
				
		}

		p = plotly::ggplotly(p,
				tooltip=c(
					element_x(),
					element_y(),
					"ANID"
				)
			) %>%
			plotly::layout(
				margin=list(
					t = 50
				),
				legend=list(
					x=1.0, y=0.5,
					font=list(
						family="Helvetica",
						size=11
					)
					#bordercolor=my_border_color,
					#borderwidth=1
					#bgcolor=my_background_color
				)
			) #%>%
			# plotly::config(displayModeBar = FALSE)

		# ### Fixes 'Warning in origRenderFunc(): Ignoring explicitly provided widget ID'
		# ### https://github.com/ropensci/plotly/issues/985
		p$elementId = NULL
		# #####

		return(p)
	})

	output$plot <- plotly::renderPlotly({
		element_plot()

		# data = current_df()
		# print(element_x())
		# print(select(data, element_x()))

		# ggplot(mtcars, aes(x=wt, y=mpg)) +
		# 	geom_point(size=2, shape=23)
	})

}

server
