cat("\n\n--server.R\n")

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
	# print(glue("\t[add_source_ellipse()] Add source ellipse {source}"))
	df = data@df %>% 
		filter(Source.Name == source) %>% 
		select(ANID, data@x, data@y, Source.Name)

	layer = stat_ellipse(df,
			mapping=aes_string(x=data@x, y=data@y, color="Source.Name"),
			inherit.aes=FALSE)
	plot = addPath(plot, layer, source)

	if (plot_points) {
		# print(glue("\t[add_source_ellipse()] Calling add_source_point() for {source}"))
		plot = add_source_point(source, data, plot)
	}

	return(plot)
}

#' @describeIn SomethingNew Remove the source ellipse
remove_source_ellipse = function(source, plot) {
	# print(glue("\t[remove_source_ellipse()] Remove source ellipse {source}"))
	plot = removePath(plot, source)

	# print(glue("\t[remove_source_ellipse()] Calling remove_source_point() for {source}"))
	plot = remove_source_point(source, plot)
	return(plot)
}

#' @describeIn SomethingNew Add the source points
add_source_point = function(source, data, plot) {
	# print(glue("\t[add_source_point()] Add source points {source}"))

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
	# print(glue("\t[remove_source_point()] Remove source points {source}"))
	plot = removePoint(plot, source)
	return(plot)
}

add_selected_points = function(data, plot) {
	layer = geom_point(data@selection,
			mapping=aes_string(x=data@x, y=data@y, fill="Source.Name"), 
			size=2,
			# label="Selected",
			colour="black",
			fill="red",
			shape=21 # To get the outside border
		)
	plot = addPoint(plot, layer, "selection")
	return(plot)
}

#' @describeIn SomethingNew Add artifact points
add_artifact_point = function(filename, data, plot) {
	df = getDatafileData(data, filename) %>% select(SampleID, SiteID, data@x, data@y, note)

	layer = geom_point(df, 
		mapping=aes_string(x=data@x, y=data@y, color="SiteID"),
		inherit.aes=FALSE,
		shape=17 # Solid triangle
	)

	plot = addPoint(plot, layer, filename)
	return(plot)
}

# #' @describeIn SomethingNew Remove the source points
# remove_artifact_point = function(source, plot) {
# 	# print(glue("\t[remove_source_point()] Remove source points {source}"))
# 	plot = removePoint(plot, source)
# 	return(plot)
# }


server = function(input, output, session) {
	shinyjs::showLog()

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
		print(glue("\n[observeEvent] initialize_plot() - {country} ({x},{y})"))

		layer = ggplot(data, mapping=aes_string(x=x, y=y, color="Source.Name")) +
			xlab(paste(names(elements)[elements == x], "(ppm)")) +
			ylab(paste(names(elements)[elements == y], "(ppm)")) +
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
			# print(glue("\tAdding from initialize_plot()"))
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
		print(glue("\n[observeEvent] update_axes() - {country} ({x},{y})"))

		layer = ggplot(data, mapping=aes_string(x=x, y=y, color="Source.Name")) +
			xlab(paste(names(elements)[elements == x], "(ppm)")) +
			ylab(paste(names(elements)[elements == y], "(ppm)")) +
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
			# print(glue("\tAdding from update_axes()"))
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
		print(glue("[reactive] selected_country() - Parsing country input <{internal@country}>"))
		return(internal@country)
	})

	### Observe the updated 'country' event using 'selected_country()'
	# 1) Sets the internal 'country'
	# 2) Determines the default 'sources' using 'default_sources()' in 'config@plot.R'
	check_country = observeEvent(selected_country(), {
		print(glue("[observeEvent] check_country() - Updating deault sources"))
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
		print(glue("[reactive] selected_sources() - Parsing source input"))
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
		print(glue("[observeEvent] check_sources() - Updating sources"))
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
			# print(glue("\tRemoving final source"))
			internal <<- removeSource(internal, before)
			fig <<- remove_source_ellipse(before, fig)
		} else if ((length(add) > 0)) {
			### 2) A source is added
			for (source in add) {
				# print(glue("\tAdding {source}"))
				internal <<- addSource(internal, source)
				fig <<- add_source_ellipse(source, internal, fig, plot_points=input$show_source_points)
			}
		} else if (length(remove) > 0) {
			### 3) A source is removed
			for (source in remove) {
				# print(glue("\tRemoving {source}"))
				internal <<- removeSource(internal, source)
				fig <<- remove_source_ellipse(source, fig)
			}
		} else {
			### 4) No change to the sources is made (meaning selected_sources() reacted to something extra...)
			# print(glue("\tNo changes to source selection"))
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
		# print(paste("CHECK HERE:", input$element1, elements[input$element1]))
		internal <<- setx(internal, as.character(elements[input$element1]))
		print(glue("[reactive] selected_x() - Updating x <{internal@x}>"))
		return(internal@x)
	})

	selected_y = reactive({
		# print(paste("CHECK HERE:", input$element2, elements[input$element2]))
		internal <<- sety(internal, as.character(elements[input$element2]))
		print(glue("[reactive] selected_y() - Updating y <{internal@y}>"))
		return(internal@y)
	})

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




	# 'function() {
	# 		# $(this.api().table().row(20).node()).addClass("selected");
	# 		# this.api().table().row(20).scrollTo();
	# 		alert("scrolled");}'

	# observeEvent(c(
	# 	input$table_rows_selected,
	# 	plotly::event_data("plotly_click")
	# ), {
	# 	observe({
	# 		print("Table row selected...")
	# 		input$table_rows_selected
	# 	})

	# 	observe({
	# 		print("Plot point selected...")
	# 		plotly::event_data("plotly_click")
	# 	})
	# 	# "plotly_click" events provide a list of "curveNumber", "pointNumber", "x", and "y"
	# 	click = plotly::event_data("plotly_click")
	# 	layer_index = as.numeric(click[1])
	# 	x = as.numeric(click[3])
	# 	y = as.numeric(click[4])
	# 	print(glue("\n[observeEvent] plotly_click: layer_index={layer_index} x={x} y={y}"))
	# 	source = getLayer(fig, layer_index) ### Stores 'source' with "point" or "path" appended to it
	# 	# print(fig@layer) ### Keep this here for debugging purposes
	# 	# print(glue("\tSelected source: {source}"))
	# 	index = grab_plot_point(internal@df, x, y)
	# 	# fig@plot = fig@plot + 
	# 	# 	geom_point(
	# 	# 		data=internal@df[index,],
	# 	# 		aes_string(x=selected_x(), y=selected_y(), fill="Source.Name"), 
	# 	# 		size=2,
	# 	# 		# label="Selected",
	# 	# 		colour="black",
	# 	# 		fill="red",
	# 	# 		shape=21 # To get the outside border
	# 	# 	)
	# 	# fig@plot <<- fig@plot

	# 	source = gsub("(.*)\\s\\w+", "\\1", source)
	# 	# selection = internal@df %>% filter(Source.Name == source) %>% filter(internal@x == x)
	# 	# print(selection)
	# 	# print(internal@df[index,])
	# 	selection = internal@df[index,]
	# 	internal_index = getSelectionIndex(internal, selection)
	# })

	observeEvent(input$table_rows_selected, {
		# shinyjs::js$selected(1)
		shinyjs::js$selected()
		print(glue("\n[observeEvent] table_rows_selected"))
		### This makes the table reactive to points being selected
		# previous_selection = internal@selection
		# current_indicies = input$table_rows_selected
		# current_selection = internal@df %>% filter(row_number() %in% current_indicies)
		# data = internal@df %>% ungroup()
		# previous_indicies = getSelectionIndex(internal, input$table_rows_selected)
		# print(previous_selection %>% select("ANID", internal@x, internal@y))
		# print("")
		# print(current_selection %>% select("ANID", internal@x, internal@y))
		# print(previous_indicies)

		# new_selection = previous_selection@selection[-index,]

		# selected_data = data %>% filter(row_number() %in% selection)
		# internal <<- addSelection(internal, selected_data)
	})

	observeEvent(input$table_selection, {
		row = input$table_selection
		# print(glue("\t<{typeof(row)}> {row}"))
		selected = data %>% filter(ANID == row[1])
		internal <<- addSelection(internal, selected)
	})

	observeEvent(input$clear_selected, {
		internal <<- clearSelection(internal)
		proxy %>% DT::selectRows(which(internal@df$ANID %in% internal@selection$ANID))
	})

	# table_selected = reactive({
	# 	test = input$table_selection
	# 	return(test)
	# })

	### "plotly_click" events provide a list of "curveNumber", "pointNumber", "x", and "y"
	observeEvent(plotly::event_data("plotly_click"), {
		click = plotly::event_data("plotly_click")
		layer_index = as.numeric(click[1])
		x = as.numeric(click[3])
		y = as.numeric(click[4])
		print(glue("\n[observeEvent] plotly_click: layer_index={layer_index} x={x} y={y}"))

		index = grab_plot_point(internal@df, x, y)
		selection = internal@df[index,]
		# internal <<- adjust_selection(internal, selection)
		# print(selection)

		internal_index = getSelectionIndex(internal, selection)
		if (is.na(internal_index)) { ### Point hasn't been selected yet, so add it
			print(glue("\tAdding new point to selection"))
			internal <<- addSelection(internal, selection)
			print(index)
			### Jump to the new selection using JavaScript in 'interactive.js'
			proxy %>% DT::selectRows(which(internal@df$ANID %in% internal@selection$ANID))
			if (index > 3) {
				shinyjs::js$scroll(index-3)
			} else {
				shinyjs::js$scroll(index)
			}
		} else { ### Point is already present, so remove it
			print(glue("\tRemoving point from selection"))
			internal <<- removeSelection(internal, selection)
		}
	})

	# adjust_selection(data, selection) {
	# 	index = getSelectionIndex(data, selection)
	# 	if (is.na(index)) { ### Point hasn't been selected yet, so add it
	# 		print(glue("\tAdding new point to selection"))
	# 		data <<- addSelection(data, selection)

	# 		### Jump to the new selection using JavaScript in 'interactive.js'
	# 		proxy %>% DT::selectRows(which(data@df$ANID %in% data@selection$ANID))
	# 		if (index > 3) {
	# 			shinyjs::js$scroll(index-3)
	# 		} else {
	# 			shinyjs::js$scroll(index)
	# 		}
	# 	} else { ### Point is already present, so remove it
	# 		print(glue("\tRemoving point from selection"))
	# 		data <<- removeSelection(data, selection)
	# 	}
	# 	return(data)
	# }

	grab_plot_point = function(data, x, y) {
		# print(paste("Looking for", selected_x(), "=", x, "and", selected_y(), "=", y))
		index = which((data[selected_x()] == x) & (data[selected_y()] == y))
		# print(index)
		return(index)
	}





	toggle_source_points = observeEvent(input$show_source_points, {
		print(glue("[observeEvent] toggle_source_points() - Toggle source points <{input$show_source_points}>"))
		if (input$show_source_points) {
			for (source in internal@sources) {
				# print(glue("\t[toggle] Adding {source}"))
				fig <<- add_source_point(source, internal, fig)
			}
		} else {
			for (source in internal@sources) {
				# print(glue("\t[toggle] Removing {source}"))
				fig <<- remove_source_point(source, fig)
			}
		}
	})




	#### Create the table
	### https://shiny.rstudio.com/articles/datatables.html
	### https://rstudio.github.io/DT/shiny.html
	## DT::renderDataTable() takes an expression that returns a rectangular data object with column names, such as a data frame or a matrix.
	updatePlot = observeEvent(c(
		selected_country(),
		selected_x(),
		selected_y(),
		selected_sources()
	), {
		output$table <- DT::renderDT({
			available_elements = elements[elements %in% colnames(tab)]
			tab = tab %>% select(ANID, Source.Name, as.character(available_elements), NKT_edits)

			DT::datatable(tab,
				class="compact hover cell-border",
				colnames=c("ID", "Source Name", as.character(available_elements), "Notes"),
				rownames=FALSE,
				# filter="top",
				### https://rstudio.github.io/DT/extensions.html#scroller
				extensions="Scroller",
				# escape=FALSE,
				options=list(
					dom='fti',
					columnDefs=list(
						list(width='1px', targets=c(0)),
						list(width='150px', targets=c(1)),
						list(width='200px', targets=c(-1)),
						# list(width='50px', targets=c(1,2)),
						# list(width='200px', targets=c(3))
						list(className="dt-center", targets="_all")
					),
					# autoWidth=TRUE,
					sScrollX="100%",
					scrollX=TRUE,
					scrollY=360,
					deferRender=TRUE,
					# scrollCollapse=TRUE,
					scroller=TRUE,
					# animate=FALSE, ### Scroller animation
					# paging=FALSE,
					# pageLength=-1,
					### 'fixedHeader' is not working and is likely a bug
					### https://github.com/rstudio/DT/issues/389
					# fixedHeader=TRUE,
					select=TRUE
				),
				editable=TRUE
			) %>% DT::formatStyle(
				c(internal@x, internal@y),
				# target='column',
				backgroundColor="#F6F6F6"
			)
		})
	})

	### https://rstudio.github.io/DT/shiny.html#manipulate-an-existing-datatables-instance
	proxy = DT::dataTableProxy('table')

	grab_index = function(x) {
		index = which((internal@df[selected_x()] == x[selected_x()]) & (internal@df[selected_y()] == x[selected_y()]))
		# print(index)
		return(index)
	}

	# observeEvent(input$table_rows_selected, {
	# 	### This makes the table reactive to points being selected
	# 	previous_selection = internal@selection
	# 	current_indicies = input$table_rows_selected
	# 	current_selection = internal@df %>% filter(row_number() %in% current_indicies)
	# 	# data = internal@df %>% ungroup()
	# 	# previous_indicies = getSelectionIndex(internal, input$table_rows_selected)
	# 	print(previous_selection %>% select("ANID", internal@x, internal@y))
	# 	print("")
	# 	print(current_selection %>% select("ANID", internal@x, internal@y))
	# 	# print(previous_indicies)

	# 	# new_selection = previous_selection@selection[-index,]

	# 	# selected_data = data %>% filter(row_number() %in% selection)
	# 	# internal <<- addSelection(internal, selected_data)
	# })





	# observeEvent(input$upload_files, {
	# # check_files = reactive({
	# 	print(paste("[observeEvent] check_input() - Parsing uploaded files"))
	# 	nfiles = length(input$upload_files$name)
	# 	temp = data.frame(name=character(), path=character())
	# 	for (i in 1:nfiles) {
	# 		newfile = data.frame(
	# 			name=input$upload_files$name[i],
	# 			path=input$upload_files$datapath[i],
	# 			type="artifact",
	# 			show=FALSE)
	# 		temp = rbind(temp, newfile)
	# 	}
	# 	internal <<- addFiles(internal, temp)
	# 	print(internal@files$name)
	

	# })

	### Handle adding the new files to the internal data
	observeEvent(input$upload_files, {
		df = input$upload_files %>% select(name, datapath)
		colnames(df) = c("name", "path")
		df["id"] = "None"
		df["source"] = "None"
		df["element"] = NA
		df["note"] = "None"
		df["type"] = "Artifact"
		df["show"] = FALSE
		df["pos"] = NA ### This may not be necessary
		internal <<- addDatafile(internal, df, source="upload")

		shinyBS::toggleModal(session,
			modalId='upload_modal',
			toggle="open")
	})

	observeEvent(input$view_uploaded_files, {
		if (nrow(internal@datafiles) < 1) {
			shinyalert::shinyalert(title="No Files Uploaded",
				text="Cannot open uploaded files interface",
				type="error",
				closeOnClickOutside=TRUE
			)

			shinyBS::toggleModal(session,
				modalId='upload_modal',
				toggle="close")
		}
	})

	# ### Open the modal dialog box to manipulate the new data
	# observeEvent(c(
	# 	input$view_uploaded_files,
	# 	input$upload_files
	# ), {
	# 	if ((input$view_uploaded_files) | !is.null(input$upload_files)) { ### This if-statement ensures the Modal is only shown when uploaded files are available
	# 		if (nrow(internal@datafiles) > 0) {
	# 			available_files = internal@datafiles %>% select(name) %>% unlist(use.names=FALSE)
	# 		} else {
	# 			available_files = list()
	# 		}
	# 		# print(available_files)
	# 		showModal(
	# 			modalDialog(
	# 				title="View uploaded files",
	# 				selectInput(inputId='test',
	# 					label="Select the file",
	# 					choices=available_files
	# 				),
	# 				print(input$test),
	# 				easyClose=TRUE
	# 			)
	# 		)
	# 	} else {
	# 		removeModal()
	# 	}
	# })

	# ### Open the modal dialog box to manipulate the new data
	# observeEvent(input$upload_files, {
	# 	# print("")
	# 	# print("Uploaded Files")
	# 	# print(input$upload_files)
	# 	if (nrow(internal@datafiles) > 0) {
	# 		available_files = internal@datafiles %>% select(name) %>% unlist(use.names=FALSE)
	# 	} else {
	# 		available_files = list()
	# 	}

	# 	print(available_files)
	# 	shinyBS::toggleModal(session, 'test', toggle="open")
	# })	


	output$upload_interface <- renderUI({
		if (nrow(internal@datafiles) > 0) {
			available_files = internal@datafiles %>% select(name) %>% unlist(use.names=FALSE)
		} else {
			available_files = list()
		}
		selectInput(inputId='upload_name',
			label="Select the file",
			choices=available_files,
			selected=available_files[0]
		)
		# print("Working")
		# HTML(input$upload_name)
	})

	observeEvent(input$upload_name, {
		uploadpath = internal@datafiles %>% filter(name == input$upload_name) %>% select(path) %>% unlist
		tab = read.csv(uploadpath, header=FALSE, nrows=10)
		nrows = nrow(tab)
		ncolumns = ncol(tab)

		# top = tab %>% filter(row_number() <= 10)
		# middle = rep("...", ncolumns)
		# bottom = tab %>% filter(row_number() >= nrows-10)
		# tab = rbind(top, middle)
		# tab = rbind(tab, bottom)

		colnames(tab) = 1:ncolumns
		index = getDatafileIndex(internal, input$upload_name, "type")

		output$upload_type_interface <- renderUI({
			selectInput(inputId='upload_type',
				label='Type',
				choices=c("Artifact", "Source"),
				selected=internal@datafiles[index,"type"]
			)
		})

		output$upload_sample_id_column_interface <- renderUI({
			selectInput(inputId='upload_sample_id_column',
				label='Sample ID Column',
				choices=c("None", colnames(tab)),
				selected=internal@datafiles[index,"id"],
				multiple=FALSE
			)
		})

		output$upload_source_column_interface <- renderUI({
			selectInput(inputId='upload_source_column',
				label='Source Column',
				choices=c("None", colnames(tab)),
				selected=internal@datafiles[index,"source"],
				multiple=FALSE
			)
		})

		output$upload_element_column_interface <- renderUI({
			selectInput(inputId='upload_element_column',
				label='Element Columns',
				choices=colnames(tab),
				selected=c(unlist(internal@datafiles[index,"element"], use.names=FALSE)),
				multiple=TRUE
			)
		})

		output$upload_note_column_interface <- renderUI({
			selectInput(inputId='upload_note_column',
				label='Note Column',
				choices=c("None", colnames(tab)),
				selected=internal@datafiles[index,"note"],
				multiple=FALSE
			)
		})

		output$upload_show_interface <- renderUI({
			if (internal@datafiles[index,"show"]) {
				show = "Yes"
			} else {
				show = "No"
			}
			radioButtons(inputId='upload_show',
				label="Show in Plot",
				choices=list("Yes", "No"),
				selected=show,
				inline=TRUE
			)
		})

		output$upload_preview <- DT::renderDT({
			upload_preview_table = DT::datatable(tab,
				class="compact hover cell-border",
				rownames=TRUE,
				extensions="Scroller",
				options=list(
					dom='t',
					ordering=FALSE,
					columnDefs=list(
						list(className="dt-center", targets="_all")
					),
					scroller=TRUE,
					scrollX=TRUE,
					scrollY=150,
					sScrollX="100%",
					select=FALSE
				)
			)


		})
	})

	# upload_preview_proxy = DT::dataTableProxy('upload_preview')

	observeEvent(input$upload_type, {
		if (!is.null(input$upload_name)) {
			internal <<- setDatafileValue(internal, input$upload_name, "type", input$upload_type)
		}
	})

	observeEvent(input$upload_sample_id_column, {
		if (!is.null(input$upload_name)) {
			internal <<- setDatafileValue(internal, input$upload_name, "id", input$upload_sample_id_column)
		}
	})

	observeEvent(input$upload_source_column, {
		if (!is.null(input$upload_name)) {
			internal <<- setDatafileValue(internal, input$upload_name, "source", input$upload_source_column)
		}
	})

	observeEvent(input$upload_element_column, {
		if (!is.null(input$upload_name)) {
			internal <<- setDatafileValue(internal, input$upload_name, "element", list(input$upload_element_column))
		}
	})

	observeEvent(input$upload_note_column, {
		if (!is.null(input$upload_name)) {
			internal <<- setDatafileValue(internal, input$upload_name, "note", input$upload_note_column)
		}
	})

	observeEvent(input$upload_show, {
		if (input$upload_show == "Yes") {
			index = getDatafileIndex(internal, input$upload_name, "name")
			upload = internal@datafiles %>% filter(row_number() == index)

			if (is.na(upload["element"])) {
				# input$upload_show = "No"
				# showModal(modalDialog(
				# 	title="Cannot Add to Plot",
				# 	size="s",
				# 	easyClose=TRUE,
				# 	"No element columns selected."
				# ))
				shinyalert::shinyalert(title="Cannot Add to Plot",
					text="No element columns selected",
					type="error",
					closeOnClickOutside=TRUE
				)

				updateRadioButtons(session, inputId='upload_show',
					label="Show in Plot",
					choices=list("Yes", "No"),
					selected="No",
					inline=TRUE
				)
			} else {
				showNotification(ui=glue("Adding {input$upload_name} to plot."),
					type="message",
					duration=3
				)

				internal <<- addDatafileData(internal, input$upload_name)
				fig <<- add_artifact_point(input$upload_name, internal, fig)
				print("***")
				print(fig@point)
				print(fig@path)
				print(fig@layer)
				print("")
				print(internal@sources)
				print("***")
			}

			# tab = read.csv(upload$path, header=TRUE)
			# # print(tab)
			# # print("")
			# # # print(upload)
			# # # print(upload$element[[1]])
			# # print(colnames(tab))
			# # print("")
			# tab %>% select(as.numeric(upload$element[[1]]))

			# print("***Adding uploaded data")
			# # print(internal@x)
			# # print(internal@y)
			# # print(colnames(tab))
			# # print(data@x, data@y)
			# layer = geom_point(tab,
			# 		mapping=aes_string(x=internal@x, y=internal@y), 
			# 		size=2,
			# 		label=upload$name,
			# 		color="blue",
			# 		shape=17 # Solid triangle
			# 	)
			# fig = addPoint(fig, layer, "upload")
			# fig <<- fig

		}
	})


	# observeEvent(c(
	# 	input$upload_sample_type,
	# 	input$upload_sample_id_column,
	# 	input$upload_source_column,
	# 	input$upload_element_column,
	# 	input$upload_note_column
	# ), {
	# 	# print("**Updating column background color")
	# 	# # print(glue(as.numeric(input$upload_note_column), " ", is.numeric(as.numeric(input$upload_note_column))))
	# 	# # print(c(as.numeric(input$upload_note_column)))
	# 	# if (!is.na(as.numeric(input$upload_note_column))) {
	# 	# 	# print(c(as.numeric(input$upload_note_column)))
	# 	# 	print("***Adding the new background")
	# 	# 	reloadData(upload_preview_proxy) %>% DT::formatStyle(
	# 	# 						c(1),
	# 	# 						c("#4D4D4D")
	# 	# 						# target='column',
	# 	# 						# backgroundColor="#4D4D4D"
	# 	# 					)
	# 	# }




	# })

	# output$upload_sample_id_column_interface <- renderUI({
	# 	print(list(1:10))
	# 	selectInput(inputId='upload_sample_id_column',
	# 		label='Sample ID Column',
	# 		choices=list(1:10),
	# 		selected=1
	# 	)
	# })

	observeEvent(c(
		input$upload_name,
		input$upload_type,
		input$upload_sample_id_column,
		input$upload_source_column,
		input$upload_element_column,
		input$upload_note_column
	), {
		# print(glue("Upload Modal Selection: ", input$upload_name))
		# print(glue("\tType: ", input$upload_format_type))
		# print(glue("\tSample ID: ", input$upload_sample_id_column))
		# print(glue("\tSource: ", input$upload_source_column))
		# if (length(input$upload_element_column) > 0) {
		# 	print(glue("\tElements: ", paste(sort(input$upload_element_column), collapse=", ")))
		# } else {
		# 	print(glue("\tElements: None"))
		# }
		# print(glue("\tNote: ", input$upload_note_column))
		# temp = internal@datafiles %>% select(name, id, source, element, note, type, show)
		# print(temp)
	})





	updatePlot = observeEvent(c(
			selected_country(),
			selected_sources(),
			selected_x(),
			selected_y(),
			input$show_source_points,
			plotly::event_data("plotly_click"),
			input$table_selection,
			input$clear_selected,
			input$upload_show
		), {
			print(glue("[observeEvent] Update Plot"))
			output$plot = plotly::renderPlotly({
				### Debug the various layers of the ggplot2 object (calling result() from 'dynamic.R')
				# result(fig)
				##########

				if (length(internal@selection) > 0) {
					# print(internal@selection %>% select("ANID", internal@x, internal@y))
					fig = add_selected_points(internal, fig)
				}

				fig@plot$labels$colour = NULL ### Removes the legend title!

				# print(str(fig@plot))

				pfig = plotly::ggplotly(fig@plot,
						tooltip=c("ANID", "x", "y")
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

				# print(str(pfig))

				### Remove the tooltips of the source ellipses from the plotly object 'pfig'
				for (source in internal@sources) {
					index = getLayerIndex(fig, paste(source, "path"))
					pfig$x$data[[index]]$hoverinfo = "none"
				}
				if (!input$show_source_info) {
					for (source in internal@sources) {
						index = getLayerIndex(fig, paste(source, "point"))
						if (!is.na(index)) {
							pfig$x$data[[index]]$hoverinfo = "none"
						}
					}
				}
				
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

}