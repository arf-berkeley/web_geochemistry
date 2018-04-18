cat("\n\n--server.R\n")

# library("dplyr")
library("ggplot2")

### No need because both are already in 'ui.R', which is loaded first
# library("shiny") 
# source("config.R") ### Requires: 'config.R', but its already loaded by 'ui.R'
source("dynamic.R")

server = function(input, output) {

	internal = DynamicData()
	tracker = DynamicPlot()

	# state = observe({
	# 	cat("\n\n---state\n")
	# 	cat(paste("Country:", selected_country(), "\n"))
	# 	cat(paste("Sources:", "\n"))
	# 	cat(paste("\t", selected_sources(), "\n"))
	# 	cat(paste("x:", selected_x(), "/", "y:", selected_y(), "\n"))
	# })

	# #' React to updating the 'country'
	# #'
	# #' @return String of selected 'country'
	# #' @details
	# #' Note: 'input$country' will be of the format 'Country (# Sources)'
	# #' Therefore, 'extract_count_label()' from 'config.R' is used to extract only the 'country'
	# #' @examples
	# #' count_label = 'Country (# Sources)'
	# #' label = extract_count_label(count_label) # Holds 'Country'
	selected_country = reactive({
		cat("--selected_country\n")
		return(extract_count_label(input$country))
	})

	### Observe the updated 'country' event using 'selected_country()'
	# 1) Sets the internal 'country'
	# 2) Determines the default 'sources' using 'default_sources()' in 'config.R'
	check_country = observeEvent(selected_country(), {
		internal = setCountry(internal, selected_country())
		internal = setSources(internal, default_sources(internal@country))
		internal <<- internal
		initialize_plot()
	})

	### React to updating the 'sources'
	# 1) Checks if the input from 'input$selected_source_labels' is NULL
	# 2) If RETURNS - default 'sources' for 'selected_country()'
	# 3) Else RETURNS - list of the selected 'sources' using 'vectorof_labels()' from 'config.R'
	# Note: like 'input$country', 'input$selected_source_labels' is of the format 'Source (# Samples)'
	# 	'vectorof_labels()' is an apply() implementation of 'extract_count_labels()'
	selected_sources = reactive({
		country = selected_country() # Force update when 'country' is changed
		if (is.null(input$selected_source_labels)) {
			return(default_sources(country))
		} else {
			return(vectorof_labels(input$selected_source_labels))
		}
	})

	### Observe the updated 'sources' using 'selected_sources()'
	# 1) Stores the previously selected sources from 'internal' using 'getSources()'
	# 2) Reads the currently selected sources from 'selected_sources()'
	# 3) Compares these two lists to determine if a source was added or removed
	# 4) Applies the appropriate method ('addSource' or 'removeSource') to 'internal'
	check_sources = observeEvent(selected_sources(), {
		print(glue::glue("[observeEvent] Check Selected Sources"))
		before = getSources(internal)
		after = selected_sources()

		add = after[!(after %in% before)]
		remove = before[!(before %in% after)]

		if (length(add) > 0) {
			internal <<- addSource(internal, add)

			df = internal@df %>%
				filter(Source.Name == add) %>%
				select(ANID, internal@x, internal@y, Source.Name)
			tracker <<- addPath(tracker, add)
			index = getPathIndex(tracker, add)

			print(glue::glue("\tAdding {add} with {nrow(df)} artifacts to index {index}"))

			fig = fig +
				stat_ellipse(df,
					mapping=aes_string(x=internal@x, y=internal@y, color="Source.Name"),
					inherit.aes=FALSE)

			if (input$show_source_points) {
				print(glue::glue("\tShow source points: True"))
				add_source_point(add)
			}

			fig <<- fig


		} else if (length(remove) > 0) {
			internal <<- removeSource(internal, remove)

			index = getPathIndex(tracker, remove)
			tracker <<- removePath(tracker, remove)

			print(glue::glue("\tRemoving {remove} from index {index}"))

			fig = fig %>% ggedit::remove_geom('path', index)

			if (!input$show_source_points) {
				print(glue::glue("\tShow source points: False"))
				remove_source_point(remove)
			}

			fig <<- fig
		} else {
			print(glue::glue("\tNo changes to source selection"))
		}
	})

	### RenderUI for 'output$source_selection'
	# These are the checkboxes for available 'sources' in the collapsible panel 'source_selection' in 'ui.R'
	# Displays 'source_count_labels' with the format 'Source (# Samples)'
	# Utilizes the reactive 'selected_country()' to update everytime the 'country' is set
	output$source_selection = renderUI({
		checkboxGroupInput("selected_source_labels",
			label="Source (# Artifacts)",
			choices=available_source_count_labels(selected_country()),
			selected=default_source_count_labels(selected_country())
		)
	})

	selected_x = reactive({
		return(input$element1)
	})

	selected_y = reactive({
		return(input$element2)
	})

	check_x = observeEvent(selected_x(), {
		# print("New selection: x")
		internal <<- setx(internal, selected_x())
	})

	check_y = observeEvent(selected_y(), {
		# print("New selection: y")
		internal <<- sety(internal, selected_y())
	})

	add_selected_points = reactive({
		print("Adding")
		fig = fig + 
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
		fig <<- fig
	})

	# check_input = observe({
	# 	input$upload_files
	# 	print(input$upload_files)
	# 	print(length(input$upload_files))
	# })

	# add_uploaded_points = reactive({
	# 	# scale_shape_identity(2)
	# 	# guides(alpha=FALSE) +
	# 	# scale_color_discrete()

	# 	# if (input$plot_artifact_ellipses) {
	# 	# 	p = p +
	# 	# 		stat_ellipse(data=upload,
	# 	# 			aes(x=X, y=Y,
	# 	# 				group=SiteID,
	# 	# 				color=factor(SiteID, labels=paste("Site", unique(SiteID))
	# 	# 				)
	# 	# 			)
	# 	# 		)
	# 	# }

	# 	# if (input$plot_artifact_labels) {
	# 	# 	p = p + geom_text(data = upload,
	# 	# 	mapping = aes(x = X, y = Y, group = SiteID,
	# 	# 	color = factor(SiteID),
	# 	# 	label = SampleID),
	# 	# 	check_overlap = T, size = 2.5, show.legend = F)
	# 	# }
	# })

	observeEvent(plotly::event_data("plotly_click"), {
		# "plotly_click" events provide a list of "curveNumber", "pointNumber", "x", and "y"
		click = plotly::event_data("plotly_click")
		layer_index = as.numeric(click[1])
		x = as.numeric(click[3])
		y = as.numeric(click[4])
		print(glue::glue("\n[observeEvent] plotly_click: layer_index={layer_index} x={x} y={y}"))
		source = getLayer(tracker, layer_index)
		# print(tracker@layer) ### Keep this here for debugging purposes
		print(glue::glue("\tSelected source: {source}"))
		index = grab_plot_point(internal@df, x, y)
		fig = fig + 
			geom_point(
				data=internal@df[index,],
				aes_string(x=selected_x(), y=selected_y(), fill="Source.Name"), 
				size=2,
				# label="Selected",
				colour="black",
				fill="red",
				shape=21 # To get the outside border
			)
		fig <<- fig
		# internal <<- addSelection(internal, internal@df[index,])
	})

	grab_plot_point = function(data, x, y) {
		# print(paste("Looking for", selected_x(), "=", x, "and", selected_y(), "=", y))
		index = which((data[selected_x()] == x) & (data[selected_y()] == y))
		# print(index)
		return(index)
	}

	### Initialize the plot
	# 1) 
	initialize_plot = reactive({
		country = selected_country()
		x = selected_x()
		y = selected_y()
		print(glue::glue("\n[reactive] Initializing Plot: {country}"))

		fig = ggplot(data, mapping=aes_string(x=x, y=y, color="Source.Name")) +
			xlab(names(elements)[elements == x]) +
			ylab(names(elements)[elements == y]) +
			# theme_minimal() +
			# guides(color=guide_legend(nrow=2)) +
			theme(#plot.background = element_rect(color = "#e3e3e3", fill="#f7f7f7"),
				# panel.background = element_rect(fill="white"),
				# legend.position = "bottom",
				# legend.title = element_blank(),
				# panel.border = element_rect(fill=NA, colour="#e3e3e3"),
				axis.line = element_line(colour="black"),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				panel.border = element_blank(),
				panel.background = element_blank()
			)

		for (i in default_sources(country)) {
			tracker <<- addPath(tracker, i)
			df = internal@df %>% 
				filter(Source.Name == i) %>% 
				select(ANID, x, y, Source.Name)

			# cat(i, "with", nrow(df), "artifacts\n")

			fig = fig +
				stat_ellipse(df,
					mapping=aes_string(x=x, y=y, color="Source.Name"),
					inherit.aes=FALSE)
		}
		fig <<- fig
	})

	toogle_source_points = observeEvent(input$show_source_points, {
		# print(paste("Toogle source points", input$show_source_points))
		print(glue::glue("[observeEvent] Toogle Source Points: {input$show_source_points}"))
		if (input$show_source_points) {
			for (source in tracker@path) {
				add_source_point(source)
			}
		} else if (length(tracker@point) > 0) { # Is this check necessary if the list is empty???
			for (source in tracker@point) {
				remove_source_point(source)
			}
		}
		fig <<- fig
	})

	add_source_point = function(source) {
		print(glue::glue("Adding source points for {source}"))
		df = internal@df %>% filter(Source.Name == source) %>% select(ANID, internal@x, internal@y, Source.Name)
		tracker <<- addPoint(tracker, source)
		fig = fig +
			geom_point(df, 
				mapping=aes_string(x=internal@x, y=internal@y, color="Source.Name"),
				size=1.25,
				inherit.aes=FALSE
			)
		fig <<- fig
	}

	# add_source_point_reactive = reactive({
	# 	print(names(input))
	# 	add_source_point("test")
	# })

	remove_source_point = function(source) {
		index = getPointIndex(tracker, source)

		print(glue::glue("Removing source points for {source} at index {index}"))
		# print(tracker@point)

		tracker <<- removePoint(tracker, source)
		fig = fig %>% ggedit::remove_geom('point', index)
		fig <<- fig
	}

	# observeEvent(initialize_plot(), {
	# 	print("Plot was initialized!")
	# })

	updatePlot = observeEvent(c(
			initialize_plot(),
			selected_sources(),
			input$show_source_points,
			plotly::event_data("plotly_click"),
			input$table_rows_selected,
			add_source_point_reactive()
		), {
			print(glue::glue("[observeEvent] Update Plot"))
			output$plot = plotly::renderPlotly({
				# cat("---renderPlotly\n")
				# print(fig$data)
				# # initialize_plot()
				pfig = plotly::ggplotly(fig,
						# tooltip=c(element_x(), element_y(), "label")
						# tooltip=c(internal@x, internal@y)
						tooltip=c("x", "y")
					) %>%
					plotly::layout(
						margin=list(t=50),
						legend=list(
							x=1.0, y=0.5,
							font=list(family="Helvetica", size=11)
							# bordercolor=my_border_color,
							# borderwidth=1
							#bgcolor=my_background_color
						)
					) #%>%
					# plotly::config(displayModeBar = FALSE)

				# print(length(selected_sources()))
				# print(fig$hoverinfo)
				
				### Loops through the currently selected sources and removes their 'tooltip'
				### '$x$data[[i]' is split into lists of the assigned 'groups'
				# for (i in 1:length(internal@sources)) {
				# 	fig$x$data[[i]]$hoverinfo = "none"
				# }
				# fig$x$data[[2]]$hoverinfo = "none"

				### Fixes 'Warning in origRenderFunc(): Ignoring explicitly provided widget ID'
				### https://github.com/ropensci/plotly/issues/985
				pfig$elementId = NULL

				### The 'ggplotly' object must be returned!
				return(pfig)
				# fig
			})
	})

	#### Create the table
	### https://shiny.rstudio.com/articles/datatables.html
	### https://rstudio.github.io/DT/shiny.html
	## DT::renderDataTable() takes an expression that returns a rectangular data object with column names, such as a data frame or a matrix.
	# updatePlot = observeEvent(c(
	# 	plotly::event_data("plotly_click")
	# ), {
	output$table <- DT::renderDT({
		# print("Running 'renderDataTable()'.")
		if (!is.null(input$file1)) {
			# print("     Uploaded file!")
			# print(input$file1)
			tab = internal@df
		} else {
			# print("     No uploaded file...")
			tab = internal@df
		}

		tab = tab %>% select(ANID, Source.Name, input$element1, input$element2, NKT_edits)

		DT::datatable(tab,
			class="compact hover cell-border",
			rownames=FALSE,
			# filter="top",
			extensions=c("FixedHeader", "Scroller"), # https://rstudio.github.io/DT/extensions.html#fixedheader
			options=list(
				dom='fti',
				columnDefs=list(list(
					className="dt-center", targets="_all"
				)),
				scrollX=TRUE,
				scrollY=360,
				paging=FALSE,
				pageLength=-1,
				### 'fixedHeader' is not working and is likely a bug
				### https://github.com/rstudio/DT/issues/389
				fixedHeader=TRUE
			),
			editable=TRUE
		)
	})
	# })

	observeEvent(input$table_rows_selected, {
		### This makes the table reactive to points being selected
		selection = input$table_rows_selected
		data = internal@df %>% ungroup()

		selected_data = data %>% filter(row_number() %in% selection) %>% select("ANID", internal@x, internal@y, "Source.Name")
		print("")
		# print(select(selected_data, "ANID", internal@x, internal@y, "Source.Name"))
		# print(typeof(selected_data))
		print(selected_data)
		# if (length(selection) > 0) {
		# 	print(data[selection,])
		# 	print(" ")
		# }
		fig = fig + 
			geom_point(
				data=selected_data,
				aes_string(x=selected_x(), y=selected_y(), fill="Source.Name"), 
				size=2,
				label="Selected",
				colour="black",
				fill="red",
				shape=21 # To get the outside border
			)
		fig <<- fig
	})

}

# server
