cat("\n\n--server.R\n")

# library("dplyr")
library("ggplot2")

### No need because both are already in 'ui.R', which is loaded first
# library("shiny") 
# source("config@plot.R") ### Requires: 'config@plot.R', but its already loaded by 'ui.R'
source("dynamic.R")

#' New Title
#' 
#' New Description
#' 
#' New Details
#' 
#' @name Edit
#' @param source 
NULL

#' @describeIn SomethingNew Add the source ellipse
add_source_ellipse = function(source, data, plot, plot_points=FALSE) {
	print(glue::glue("\t[add_source_ellipse()] Add source ellipse {source}"))
	df = data@df %>% 
		filter(Source.Name == source) %>% 
		select(ANID, data@x, data@y, Source.Name)

	layer = stat_ellipse(df,
			mapping=aes_string(x=data@x, y=data@y, color="Source.Name"),
			inherit.aes=FALSE)
	plot = addPath(plot, layer, source)

	if (plot_points) {
		print(glue::glue("\t[add_source_ellipse()] Calling add_source_point() for {source}"))
		plot = add_source_point(source, data, plot)
	}

	return(plot)
}

#' @describeIn SomethingNew Remove the source ellipse
remove_source_ellipse = function(source, plot) {
	print(glue::glue("\t[remove_source_ellipse()] Remove source ellipse {source}"))
	plot = removePath(plot, source)

	print(glue::glue("\t[remove_source_ellipse()] Calling remove_source_point() for {source}"))
	plot = remove_source_point(source, plot)
	return(plot)
}

#' @describeIn SomethingNew Add the source points
add_source_point = function(source, data, plot) {
	print(glue::glue("\t[add_source_point()] Add source points {source}"))

	df = data@df %>%
		filter(Source.Name == source) %>%
		select(ANID, data@x, data@y, Source.Name)

	layer = geom_point(df, 
		mapping=aes_string(x=data@x, y=data@y, color="Source.Name"),
		size=1.25,
		inherit.aes=FALSE
	)

	plot = addPoint(plot, layer, source)
	return(plot)
}

#' @describeIn SomethingNew Remove the source points
remove_source_point = function(source, plot) {
	print(glue::glue("\t[remove_source_point()] Remove source points {source}"))
	plot = removePoint(plot, source)
	return(plot)
}

server = function(input, output) {

	internal = DynamicData()
	fig = DynamicPlot()

	### Initialize the plot
	# 1) 
	initialize_plot = observeEvent(c(
		input$country
	), {
		country = isolate(selected_country())
		x = isolate(selected_x())
		y = isolate(selected_y())
		print(glue::glue("\n[observeEvent] initialize_plot() - {country} ({x},{y})"))

		layer = ggplot(data, mapping=aes_string(x=x, y=y, color="Source.Name")) +
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
		fig <<- initializePlot(fig, layer)
		
		for (source in default_sources(country)) {
			print(glue::glue("\tAdding from initialize_plot()"))
			fig <<- add_source_ellipse(source, internal, fig, plot_points=input$show_source_points)
		}
	})

	update_axes = observeEvent(c(
		input$element1,
		input$element2
	), {
		country = isolate(selected_country())
		x = isolate(selected_x())
		y = isolate(selected_y())
		print(glue::glue("\n[observeEvent] update_axes() - {country} ({x},{y})"))

		layer = ggplot(data, mapping=aes_string(x=x, y=y, color="Source.Name")) +
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

		previous_sources = fig@path
		fig <<- initializePlot(fig, layer)

		for (source in previous_sources) {
			print(glue::glue("\tAdding from update_axes()"))
			fig <<- add_source_ellipse(source, internal, fig, plot_points=input$show_source_points)
		}

	})

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
	# #' Therefore, 'extract_count_label()' from 'config@plot.R' is used to extract only the 'country'
	# #' @examples
	# #' count_label = 'Country (# Sources)'
	# #' label = extract_count_label(count_label) # Holds 'Country'
	selected_country = reactive({
		internal <<- setCountry(internal, extract_count_label(input$country))
		print(glue::glue("[reactive] selected_country() - Parsing country input <{internal@country}>"))
		return(internal@country)
	})

	### Observe the updated 'country' event using 'selected_country()'
	# 1) Sets the internal 'country'
	# 2) Determines the default 'sources' using 'default_sources()' in 'config@plot.R'
	check_country = observeEvent(selected_country(), {
		print(glue::glue("[observeEvent] check_country() - Updating deault sources"))
		internal <<- setSources(internal, default_sources(internal@country))
		# initialize_plot()
	})

	### React to updating the 'sources'
	# 1) Checks if the input from 'input$selected_source_labels' is NULL
	# 2) If RETURNS - default 'sources' for 'selected_country()'
	# 3) Else RETURNS - list of the selected 'sources' using 'vectorof_labels()' from 'config@plot.R'
	# Note: like 'input$country', 'input$selected_source_labels' is of the format 'Source (# Samples)'
	# 	'vectorof_labels()' is an apply() implementation of 'extract_count_labels()'
	selected_sources = reactive({
		print(glue::glue("[reactive] selected_sources() - Parsing source input"))
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
		print(glue::glue("[observeEvent] check_sources() - Updating sources"))
		before = internal@sources
		after = selected_sources()

		add = after[!(after %in% before)]
		remove = before[!(before %in% after)]

		### These print() statements are great for debugging the four case if/else below
		# print(paste("Before:", paste(before, collapse=", ")))
		# print(paste("After:", paste(after, collapse=", ")))
		# print(paste("Equality test:", identical(default_sources(internal@country), after)))
		##########

		### Four cases occur when selecting from the available sources
		if ((length(before) == 1) & (identical(default_sources(internal@country), after))) {
			### 1) The final source is remove (and selected_sources() resets the sources to the default)
			# print(glue::glue("\tRemoving final source"))
			internal <<- removeSource(internal, before)
			fig <<- remove_source_ellipse(before, fig)
		} else if ((length(add) > 0)) {
			### 2) A source is added
			for (source in add) {
				# print(glue::glue("\tAdding {source}"))
				internal <<- addSource(internal, source)
				fig <<- add_source_ellipse(source, internal, fig, plot_points=input$show_source_points)
			}
		} else if (length(remove) > 0) {
			### 3) A source is removed
			for (source in remove) {
				# print(glue::glue("\tRemoving {source}"))
				internal <<- removeSource(internal, source)
				fig <<- remove_source_ellipse(source, fig)
			}
		} else {
			### 4) No change to the sources is made (meaning selected_sources() reacted to something extra...)
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
		internal <<- setx(internal, input$element1)
		print(glue::glue("[reactive] Updating x: {internal@x}"))
		return(internal@x)
	})

	# check_x = observeEvent(selected_x(), { ### This could be redundant now?
	# 	# print("New selection: x")
	# 	internal <<- setx(internal, selected_x())
	# })

	selected_y = reactive({
		internal <<- sety(internal, input$element2)
		print(glue::glue("[reactive] Updating y: {internal@y}"))
		return(internal@y)
	})

	# check_y = observeEvent(selected_y(), { ### This could be redundant now?
	# 	# print("New selection: y")
	# 	internal <<- sety(internal, selected_y())
	# })

	# add_selected_points = reactive({
	# 	print("Adding")
	# 	fig@plot = fig@plot + 
	# 		geom_point(
	# 			data=selected_points(),
	# 			aes_string(x=element_x(), y=element_y(), fill="Source.Name"), 
	# 			size=2,
	# 			label="Selected",
	# 			colour="black",
	# 			fill="red",
	# 			shape=21 # To get the outside border
	# 		)
	# 	# scale_fill_manual(name = "", values="black", label="Your Selection")
	# 	fig@plot <<- fig@plot
	# })

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

	# observeEvent(plotly::event_data("plotly_click"), {
	# 	# "plotly_click" events provide a list of "curveNumber", "pointNumber", "x", and "y"
	# 	click = plotly::event_data("plotly_click")
	# 	layer_index = as.numeric(click[1])
	# 	x = as.numeric(click[3])
	# 	y = as.numeric(click[4])
	# 	print(glue::glue("\n[observeEvent] plotly_click: layer_index={layer_index} x={x} y={y}"))
	# 	source = getLayer(fig, layer_index) ### Stores 'source' with "point" or "path" appended to it
	# 	print(fig@layer) ### Keep this here for debugging purposes
	# 	print(glue::glue("\tSelected source: {source}"))
	# 	index = grab_plot_point(internal@df, x, y)
	# 	fig@plot = fig@plot + 
	# 		geom_point(
	# 			data=internal@df[index,],
	# 			aes_string(x=selected_x(), y=selected_y(), fill="Source.Name"), 
	# 			size=2,
	# 			# label="Selected",
	# 			colour="black",
	# 			fill="red",
	# 			shape=21 # To get the outside border
	# 		)
	# 	fig@plot <<- fig@plot
	# 	# internal <<- addSelection(internal, internal@df[index,])
	# })

	# grab_plot_point = function(data, x, y) {
	# 	# print(paste("Looking for", selected_x(), "=", x, "and", selected_y(), "=", y))
	# 	index = which((data[selected_x()] == x) & (data[selected_y()] == y))
	# 	# print(index)
	# 	return(index)
	# }

	# toogle_source_points = observeEvent(input$show_source_points, {
	# 	print(glue::glue("[observeEvent] Toogle Source Points: {input$show_source_points}"))
	# 	if (input$show_source_points) {
	# 		for (source in fig@path) {
	# 			print(getPointIndex(fig, source))
	# 			if (is.na(getPointIndex(fig, source))) {
	# 				print(glue::glue("\t[toggle] Adding {source}"))
	# 				fig <<- add_source_point(source, internal, fig)
	# 			}
	# 		}
	# 	} else { # Is this check necessary if the list is empty???
	# 		for (source in fig@point) {
	# 			print(getPointIndex(fig, source))
	# 			if (!is.na(getPointIndex(fig, source))) {
	# 				print(glue::glue("\t[toggle] Removing {source}"))
	# 				fig <<- remove_source_point(source, fig)
	# 			}
	# 		}
	# 	}
	# })

	updatePlot = observeEvent(c(
			selected_country(),
			selected_sources(),
			selected_x(),
			selected_y()
			# input$show_source_points,
			# plotly::event_data("plotly_click"),
			# input$table_rows_selected
		), {
			print(glue::glue("[observeEvent] Update Plot"))
			output$plot = plotly::renderPlotly({
				# cat("---renderPlotly\n")
				# print(fig@plot$data)
				# # initialize_plot()
				# print(typeof(fig@plot))
				# print(as(fig@plot, "gg"))
				# print(fig@plot)
				# test = ggplot(data, mapping=aes_string(x="Rb", y="Mn", colour="Source.Name"))
				# print(typeof(test))
				# print(str(fig@plot))
				# print(str(test))
				result(fig)
				fig@plot$labels$colour = NULL ### Removes the legend title!
				pfig = plotly::ggplotly(fig@plot,
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
					# plotly::config@plot(displayModeBar = FALSE)

				# print(str(pfig@plot$x$data))

				# print(length(selected_sources()))
				# print(fig@plot$hoverinfo)
				
				### Loops through the currently selected sources and removes their 'tooltip'
				### '$x$data[[i]' is split into lists of the assigned 'groups'
				# for (i in 1:length(internal@sources)) {
				# 	fig@plot$x$data[[i]]$hoverinfo = "none"
				# }
				# fig@plot$x$data[[2]]$hoverinfo = "none"

				### Fixes 'Warning in origRenderFunc(): Ignoring explicitly provided widget ID'
				### https://github.com/ropensci/plotly/issues/985
				pfig$elementId = NULL

				### The 'ggplotly' object must be returned!
				return(pfig)
				# fig@plot
			})
	})

	# #### Create the table
	# ### https://shiny.rstudio.com/articles/datatables.html
	# ### https://rstudio.github.io/DT/shiny.html
	# ## DT::renderDataTable() takes an expression that returns a rectangular data object with column names, such as a data frame or a matrix.
	# # updatePlot = observeEvent(c(
	# # 	plotly::event_data("plotly_click")
	# # ), {
	# output$table <- DT::renderDT({
	# 	# print("Running 'renderDataTable()'.")
	# 	if (!is.null(input$file1)) {
	# 		# print("     Uploaded file!")
	# 		# print(input$file1)
	# 		tab = internal@df
	# 	} else {
	# 		# print("     No uploaded file...")
	# 		tab = internal@df
	# 	}

	# 	tab = tab %>% select(ANID, Source.Name, input$element1, input$element2, NKT_edits)

	# 	DT::datatable(tab,
	# 		class="compact hover cell-border",
	# 		rownames=FALSE,
	# 		# filter="top",
	# 		extensions=c("FixedHeader", "Scroller"), # https://rstudio.github.io/DT/extensions.html#fixedheader
	# 		options=list(
	# 			dom='fti',
	# 			columnDefs=list(list(
	# 				className="dt-center", targets="_all"
	# 			)),
	# 			scrollX=TRUE,
	# 			scrollY=360,
	# 			paging=FALSE,
	# 			pageLength=-1,
	# 			### 'fixedHeader' is not working and is likely a bug
	# 			### https://github.com/rstudio/DT/issues/389
	# 			fixedHeader=TRUE
	# 		),
	# 		editable=TRUE
	# 	)
	# })
	# # })

	# observeEvent(input$table_rows_selected, {
	# 	### This makes the table reactive to points being selected
	# 	selection = input$table_rows_selected
	# 	data = internal@df %>% ungroup()

	# 	selected_data = data %>% filter(row_number() %in% selection) %>% select("ANID", internal@x, internal@y, "Source.Name")
	# 	print("")
	# 	# print(select(selected_data, "ANID", internal@x, internal@y, "Source.Name"))
	# 	# print(typeof(selected_data))
	# 	print(selected_data)
	# 	# if (length(selection) > 0) {
	# 	# 	print(data[selection,])
	# 	# 	print(" ")
	# 	# }
	# 	fig@plot = fig@plot + 
	# 		geom_point(
	# 			data=selected_data,
	# 			aes_string(x=selected_x(), y=selected_y(), fill="Source.Name"), 
	# 			size=2,
	# 			label="Selected",
	# 			colour="black",
	# 			fill="red",
	# 			shape=21 # To get the outside border
	# 		)
	# 	fig@plot <<- fig@plot
	# })

}