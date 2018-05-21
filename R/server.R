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
		select(ID, data@x, data@y, Source.Name)

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
		select(ID, data@x, data@y, Source.Name)

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
	df = data@upload[[filename]] %>% select(id, group, data@x, data@y, note)

	layer = geom_point(df, 
		mapping=aes_string(x=data@x, y=data@y, color="group"),
		inherit.aes=FALSE,
		shape=17 # Solid triangle
	)

	plot = addPoint(plot, layer, filename)
	return(plot)
}

#' @describeIn SomethingNew Remove the artifact points
remove_artifact_point = function(filename, plot) {
	# print(glue("\t[remove_source_point()] Remove source points {source}"))
	plot = removePoint(plot, filename)
	return(plot)
}

##### My Wrappers for quick notifications (to make the code cleaner)
	showInfo = function(str, duration=3) {
		showNotification(ui=str, type="message", duration=duration)
	}
	showWarn = function(str, duration=3) {
		showNotification(ui=str, type="warning", duration=duration)
	}
	showAlert = function(title, text) {
		shinyalert::shinyalert(title, text,	type="error", closeOnClickOutside=TRUE)
	}
##########

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


############################################################
#' Store and manipulate data from external files
#'
#' The Datafile family of functions is used to add external files,
#' set the options for these files (see \strong{Note} below), store
#' the data from the external files, and remove the file and data when
#' necessary.
#' 
#' @note These functions are meant to store data from files uploaded by a user
#'		and files pulled from external databases.
#'
#' @note Options include the filename (\emph{name}), the local file path (\emph{path}), the
#'		column numbers for the data ids, grouping, elements, and notes
#'		(\emph{id}, \emph{group}, \emph{element}, and \emph{note}, respectively), the type of data
#'		(\emph{type}, either `Artifact' or `Source'), and whether or not to include
#'		the data in the plot (\emph{show}, either `Yes' or `No').
#'
#' @name  DynamicData-Datafile
#' @param self DynamicData object
#' @param directory directory of characteristics/options for the external files
#' @param src source of the external file (either `user' or `database')
#' @param filename name of the external file used for looking it up in
#'		the directory
#' @param option option contained within the directory
#' @param value specified value for the option
NULL
############################################################
	### "plotly_click" events provide a list of "curveNumber", "pointNumber", "x", and "y"
	observeEvent(plotly::event_data("plotly_click"), {
		### Parse the "plotly_click" event
		event = plotly::event_data("plotly_click")
		curve = as.numeric(event[1])
		point = as.numeric(event[2])
		x = as.numeric(event[3])
		y = as.numeric(event[4])

		### Determine the index of the selected point
		index = which((internal@external[selected_x()] == x) & (internal@external[selected_y()] == y))
		### Extract the row corresponding to the selected point by its index
		selected = internal@external %>% filter(row_number() == index)

		# print(glue("\n[observeEvent] plotly_click: curveNumber={curve} pointNumber={point} x={x} y={y}"))
		print(glue("\n[observeEvent] plotly_click: id={selected$id}"))
		if (is.na(getSelectionIndex(internal, selected))) { ### Point hasn't been selected yet, so add it
			print(glue("\tAdding {selected$id} to selection"))
			### Add the selected row internally and select the row within the table
			internal <<- addSelection(internal, selected)
			### Jump to the new selection using JavaScript in 'interactive.js'
			if (index > 3) {
				shinyjs::js$scroll(index-3)
			} else {
				shinyjs::js$scroll(index)
			}
		} else { ### Point is already present, so remove it
			print(glue("\tRemoving {selected$id} from selection"))
			internal <<- removeSelection(internal, selected)
		}

		proxy %>% DT::selectRows(which(unlist(internal@external["id"]) %in% unlist(internal@selection["id"])))
	})

	observeEvent(input$table_rows_selected, {
		if (!(is.null(input$table_row_last_clicked))) { ### Prevent running if a row hasn't been selected yet
			index = input$table_row_last_clicked
			selected = internal@external %>% filter(row_number() == index)
			print(glue("\n[observeEvent] table_rows_selected: id={selected$id}"))
			if (is.na(getSelectionIndex(internal, selected))) { ### Point hasn't been selected yet, so add it
				print(glue("\tAdding {selected$id} to selection"))
				### Add the selected row internally and select the row within the table
				internal <<- addSelection(internal, selected)
			} else { ### Point is already present, so remove it
				print(glue("\tRemoving {selected$id} from selection"))
				internal <<- removeSelection(internal, selected)
			}
		}
	### input$table_row_last_clicked is NULL when a row is deselected
	### ignoreNULL=FALSE allows deselected rows to be determined
	}, ignoreNULL=FALSE)

	observeEvent(input$clear_selected, {
		print(glue("\n[observeEvent] clear_selected"))
		internal <<- clearSelection(internal)
		proxy %>% DT::selectRows(which(internal@external["id"] %in% internal@selection["id"]))
	})
############################################################

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

	grab_index = function(x) {
		index = which((internal@df[selected_x()] == x[selected_x()]) & (internal@df[selected_y()] == x[selected_y()]))
		# print(index)
		return(index)
	}

	### Handle adding the new files to the internal data
	observeEvent(input$upload_files, {
		for (index in 1:nrow(input$upload_files)) {
			df = input$upload_files %>% filter(row_number() == index) %>% select(name, datapath)
			print(glue("Adding {df$name}..."))
			colnames(df) = c("name", "path")
			df["id"] = "None"
			df["group"] = "None"
			df["element"] = "None"
			df["note"] = "None"
			df["type"] = "Artifact"
			df["show"] = "No"
			internal <<- addDatafile(internal, df, src="user")
		}

		output$upload_interface <- renderUI({
			selectInput(inputId='upload_name',
				label="Select the file",
				choices=names(internal@datafiles)
			)
		})

		shinyBS::toggleModal(session,
			modalId='upload_modal',
			toggle="open")
	})

	observeEvent(input$view_uploaded_files, {
		if (length(internal@datafiles) == 0) {
			showAlert(title="No Files Uploaded", text="Cannot open uploaded files interface")
			shinyBS::toggleModal(session, modalId='upload_modal', toggle="close")
		}
	})

	observeEvent(input$upload_name, {
		# filepath = internal@datafiles %>% filter(name == input$upload_name) %>% select(path) %>% unlist
		filepath = internal@datafiles[[input$upload_name]]["path"] %>% unlist(use.names=FALSE)
		tab = read.csv(filepath, header=FALSE, nrows=10)
		nrows = nrow(tab)
		ncolumns = ncol(tab)

		# top = tab %>% filter(row_number() <= 10)
		# middle = rep("...", ncolumns)
		# bottom = tab %>% filter(row_number() >= nrows-10)
		# tab = rbind(top, middle)
		# tab = rbind(tab, bottom)

		colnames(tab) = 1:ncolumns
		# index = getDatafileIndex(internal, input$upload_name, "type")

		output$upload_type_interface <- renderUI({
			selectInput(inputId='upload_type',
				label='Type',
				choices=c("Artifact", "Source"),
				selected=unlist(internal@datafiles[[input$upload_name]]["type"], use.names=FALSE)
			)
		})

		output$upload_sample_id_column_interface <- renderUI({
			selectInput(inputId='upload_sample_id_column',
				label='Sample ID Column',
				choices=c("None", colnames(tab)),
				# selected=unlist(internal@datafiles[[input$upload_name]]["id"], use.names=FALSE),
				selected=c(1),
				multiple=FALSE
			)
		})

		output$upload_group_column_interface <- renderUI({
			selectInput(inputId='upload_group_column',
				label='Source Column',
				choices=c("None", colnames(tab)),
				# selected=unlist(internal@datafiles[[input$upload_name]]["group"], use.names=FALSE),
				selected=c(2),
				multiple=FALSE
			)
		})

		output$upload_element_column_interface <- renderUI({
			selectInput(inputId='upload_element_column',
				label='Element Columns',
				choices=colnames(tab),
				# selected=c(unlist(internal@datafiles[[input$upload_name]]["element"], use.names=FALSE)),
				selected=c(3:12),
				multiple=TRUE
			)
		})

		output$upload_note_column_interface <- renderUI({
			selectInput(inputId='upload_note_column',
				label='Note Column',
				choices=c("None", colnames(tab)),
				selected=unlist(internal@datafiles[[input$upload_name]]["note"], use.names=FALSE),
				multiple=FALSE
			)
		})

		output$upload_show_interface <- renderUI({
			radioButtons(inputId='upload_show',
				label="Show in Plot",
				choices=list("Yes", "No"),
				selected=unlist(internal@datafiles[[input$upload_name]]["show"], use.names=FALSE),
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

	observeEvent(input$upload_group_column, {
		if (!is.null(input$upload_name)) {
			internal <<- setDatafileValue(internal, input$upload_name, "group", input$upload_group_column)
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
		if (input$upload_show == "Yes") { # Add the uploaded data to the internal data
			internal <<- setDatafileValue(internal, input$upload_name, "show", "Yes")
			index = getPointIndex(fig, input$upload_name)
			if (is.na(index)) { # Ensure the data is not present on the plot
				element_columns = internal@datafiles[[input$upload_name]]["element"] %>% unlist(use.names=FALSE)
				if (element_columns[1] == "None") { # Throw error notification and reset the option
					shinyalert::shinyalert(title="Cannot Add to Plot", text="No element columns selected")
					updateRadioButtons(session, inputId='upload_show',
						label="Show in Plot",
						choices=list("Yes", "No"),
						selected="No",
						inline=TRUE
					)
				} else {
					showInfo(glue("Adding {input$upload_name} to plot."))
					internal <<- addDatafileData(internal, input$upload_name)
					internal <<- addData(internal, internal@upload[[input$upload_name]])
					fig <<- add_artifact_point(input$upload_name, internal, fig)
				}

			}
		} else { # Remove the uploaded data from the internal data
			internal <<- setDatafileValue(internal, input$upload_name, "show", "No")
			if (input$upload_name %in% names(internal@upload)) { # Ensure the data is present on the plot
				showWarn(glue("Removing {input$upload_name} from plot."))
				internal <<- removeDatafileData(internal, input$upload_name)
				fig <<- remove_artifact_point(input$upload_name, fig)
			}
		}
	})

	observeEvent(c(
		input$upload_name,
		input$upload_type,
		input$upload_sample_id_column,
		input$upload_group_column,
		input$upload_element_column,
		input$upload_note_column
	), {
		# print(glue("Upload Modal Selection: ", input$upload_name))
		# print(glue("\tType: ", input$upload_format_type))
		# print(glue("\tSample ID: ", input$upload_sample_id_column))
		# print(glue("\tSource: ", input$upload_group_column))
		# if (length(input$upload_element_column) > 0) {
		# 	print(glue("\tElements: ", paste(sort(input$upload_element_column), collapse=", ")))
		# } else {
		# 	print(glue("\tElements: None"))
		# }
		# print(glue("\tNote: ", input$upload_note_column))
		# temp = internal@datafiles %>% select(name, id, source, element, note, type, show)
		# print(temp)
	})



	#### Create the table
	### https://shiny.rstudio.com/articles/datatables.html
	### https://rstudio.github.io/DT/shiny.html
	## DT::renderDataTable() takes an expression that returns a rectangular data object with column names, such as a data frame or a matrix.
	observeEvent(c(
		selected_country(),
		selected_x(),
		selected_y(),
		selected_sources(),
		input$upload_show,
		input$show_source_points
	), {
		available_elements = unlist(elements, use.names=FALSE)

		tab = data.frame()
		tab["ID"] = character()
		tab["Group"] = character()
		tab[internal@x] = character()
		tab[internal@y] = character()
		tab["Notes"] = character()

		if (nrow(internal@external) > 0) {
			tab = internal@external
			n = ncol(tab)
			element_names = colnames(tab)[3:(n-1)]
			# print(element_names)
			colnames(tab) = c("ID", "Group", as.character(element_names), "Notes")
		}

		# if (input$include_source_data) {
		# 	print(colnames(internal@df))
		# 	# itab = internal@df %>% select(ID, Source.Name, as.character(available_elements), Notes)
		# 	colnames(itab) = c("ID", "Group", as.character(available_elements), "Notes")
		# 	tab = row_bind(tab, itab)
		# }
		# print(tab)

		# print(available_elements)
		# print(internal@external)
		# test = data.frame(ID="test", Group="test", as.character(available_elements), Notes="Testing row_bind()")
		# internal = addData(internal, test)

		output$table <- DT::renderDT({
			DT::datatable(tab,
				class="compact hover cell-border",
				# colnames=c("ID", "Group", as.character(available_elements), "Notes"),
				# rownames=FALSE,
				# filter="top",
				extensions="Scroller", ### https://rstudio.github.io/DT/extensions.html#scroller
				# escape=FALSE,
				options=list(
					dom='tf',
					columnDefs=list(
						list(className="dt-center", targets="_all"),
						list(targets=c(0:1,(-1)), searchable=FALSE),
						list(width='1px', targets=c(0)),
						list(width='150px', targets=c(1)),
						list(width='200px', targets=c(-1))
					),
					# autoWidth=TRUE,
					sScrollX="100%",
					scrollX=TRUE,
					scrollY=360,
					scrollCollapse=TRUE, # Allows the table to be <= 'scrollY' height
					deferRender=TRUE,
					# scrollCollapse=TRUE,
					scroller=TRUE,
					# animate=FALSE, ### Scroller animation
					# paging=FALSE,
					# pageLength=-1,
					### 'fixedHeader' is not working and is likely a bug
					### https://github.com/rstudio/DT/issues/389
					# fixedHeader=TRUE,
					select=TRUE,
					language=list(
						zeroRecords="No data to display - Add data through Data Management"
					)
				),
				editable=TRUE,
				### This callback function fixes the "scrollCollapse" option of "Scroller" from not loading data
				### Fix found here: https://github.com/rstudio/DT/issues/371#issuecomment-368324884
				callback = JS("setTimeout(function() { table.draw(true); }, 500);")
			) %>% DT::formatStyle(
				c(internal@x, internal@y),
				# target='column',
				backgroundColor="#F6F6F6"
			)
		})
	})

	### https://rstudio.github.io/DT/shiny.html#manipulate-an-existing-datatables-instance
	proxy = DT::dataTableProxy('table')


	observeEvent(c(
			selected_country(),
			selected_sources(),
			selected_x(),
			selected_y(),
			input$show_source_points,
			plotly::event_data("plotly_click"),
			input$table_rows_selected,
			input$clear_selected,
			input$upload_show
		), {
			print(glue("[observeEvent] Update Plot"))
			### Debug the various layers of the ggplot2 object (calling result() from 'dynamic.R')
			print("*****")
			result(fig)
			print("*****")
			##########

			if (length(internal@selection) > 0) {
				# print(internal@selection %>% select("ANID", internal@x, internal@y))
				fig = add_selected_points(internal, fig)
			}

			fig@plot$labels$colour = NULL ### Removes the legend title!

			output$plot = plotly::renderPlotly({
				pfig = plotly::ggplotly(fig@plot,
						tooltip=c("ID", "x", "y")
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