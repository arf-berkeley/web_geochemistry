cat("\n\n--server.R\n")

# library("dplyr")
library("ggplot2")

### No need because both are already in 'ui.R', which is loaded first
# library("shiny") 
# source("config.R") ### Requires: 'config.R', but its already loaded by 'ui.R'
source("class.R")
source("DynamicPlot.R")

server = function(input, output) {

	internal = Internal()
	tracker = DynamicPlot()

	# state = observe({
	# 	cat("\n\n---state\n")
	# 	cat(paste("Country:", selected_country(), "\n"))
	# 	cat(paste("Sources:", "\n"))
	# 	cat(paste("\t", selected_sources(), "\n"))
	# 	cat(paste("x:", selected_x(), "/", "y:", selected_y(), "\n"))
	# })

	### React to updating the 'country'
	# RETURNS - string of selected 'country'
	# Note: 'input$country' will be of the format 'Country (# Sources)'
	# Therefore, 'extract_count_label()' from 'config.R' is used to extract only the 'country'
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
		before = getSources(internal)
		after = selected_sources()

		add = after[!(after %in% before)]
		remove = before[!(before %in% after)]

		if (length(add) > 0) {
			internal <<- addSource(internal, add)

			df = internal@df %>%
				filter(Source.Name == add) %>%
				select(ANID, internal@x, internal@y, Source.Name)
			tracker <<- addPathLayer(tracker, add)
			index = getPathIndex(tracker, add)

			print(glue::glue("Adding {add} with {nrow(df)} artifacts to index {index}"))

			fig = fig +
				stat_ellipse(df,
					mapping=aes_string(x=internal@x, y=internal@y, color="Source.Name"),
					inherit.aes=FALSE)
			fig <<- fig


		} else if (length(remove) > 0) {
			internal <<- removeSource(internal, remove)

			index = getPathIndex(tracker, remove)
			tracker <<- removePathLayer(tracker, remove)

			print(glue::glue("Removing {remove} from index {index}"))

			fig = fig %>% ggedit::remove_geom('path', index)
			fig <<- fig
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
		print("New selection: x")
		internal <<- setx(internal, selected_x())
	})

	check_y = observeEvent(selected_y(), {
		print("New selection: y")
		internal <<- sety(internal, selected_y())
	})

	# internal_df = reactive({
	# 	temp = data %>% filter(Site.Country == selected_country()) %>% filter(Source.Name %in% isolate(selected_sources()))
	# 	# temp = temp %>% filter(element_x() > 0 & element_y() > 0)
	# 	temp = temp %>% group_by(Source.Name)
	# 	return(temp)
	# })

	# element_x = reactive({
	# 	return(input$element1)
	# })

	# element_y = reactive({
	# 	return(input$element2)
	# })

	# selected_points = reactive({
	# 	### This makes the table reactive to points being selected
	# 	selection = input$table_rows_selected
	# 	data = internal_df() %>% ungroup()

	# 	selected_data = data %>% filter(row_number() %in% selection)
	# 	print("")
	# 	print(select(selected_data, "ANID", element_x(), element_y(), "Source.Name"))
	# 	print(typeof(selected_data))
	# 	print(selection)
	# 	# if (length(selection) > 0) {
	# 	# 	print(data[selection,])
	# 	# 	print(" ")
	# 	# }
	# 	return(selected_data)
	# })

	# add_selected_points = reactive({
	# 	geom_point(
	# 		data=selected_points(),
	# 		aes_string(x=element_x(), y=element_y(), fill="Source.Name"), 
	# 		size=2,
	# 		label="Selected",
	# 		colour="black",
	# 		fill="red",
	# 		shape=21 # To get the outside border
	# 	)
	# 	# scale_fill_manual(name = "", values="black", label="Your Selection")
	# })

	# check_input = observe({
	# 	input$upload_files
	# 	print(input$upload_files)
	# 	print(length(input$upload_files))
	# })

	# add_uploaded_points = reactive({
 # #+
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
	# 	print("Selection!")
	# 	test = plotly::event_data("plotly_click")
	# 	# print(typeof(test))
	# 	# print(test)
	# 	x = as.numeric(test[3])
	# 	y = as.numeric(test[4])
	# 	index = grab_plot_point(internal_df(), x, y)
	# 	# print(nrow(internal_df()))
	# 	print("Highlighting")
	# 	geom_point(
	# 		data=internal_df()[index,],
	# 		aes_string(x=element_x(), y=element_y(), fill="Source.Name"), 
	# 		size=2,
	# 		# label="Selected",
	# 		colour="black",
	# 		fill="red",
	# 		shape=21 # To get the outside border
	# 	)
	# })

	# grab_plot_point = function(data, x, y) {
	# 	print(paste("Looking for", element_x(), "=", x, "and", element_y(), "=", y))
	# 	# print(names(data))
	# 	# temp = data %>% filter(element_x() == x)
	# 	# print(nrow(filter(data, element_x() > x)))
	# 	return(which((data[element_x()] == x) & (data[element_y()] == y)))
	# }

	# add_selected_plot_point = reactive({

	# })

	# initialize_plot = {
		
	# 	fig = 

	# 	return(fig)
	# }

	### Initialize the plot
	# 1) 
	initialize_plot = reactive({
		country = selected_country()
		x = selected_x()
		y = selected_y()
		print(paste("Initializing the plot for", country))

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
			tracker <<- addPathLayer(tracker, i)
			df = internal@df %>% 
				filter(Source.Name == i) %>% 
				select(ANID, x, y, Source.Name)

			# cat(i, "with", nrow(df), "artifacts\n")

			fig = fig +
				stat_ellipse(df,
					mapping=aes_string(x=x, y=y, color="Source.Name"),
					inherit.aes=FALSE)
		}

		# print(tracker@order)

		# print(fig$layers)
		# index = getLayerIndex(tracker, "Tocomar")
		# tracker <<- removeLayer(tracker, "Tocomar")
		# fig = fig %>% ggedit::remove_geom('point', index)
		# print(fig$layers)

		fig <<- fig
	})

	toogle_source_points = observeEvent(input$show_source_points, {
		# print(paste("Toogle source points", input$show_source_points))
		if (input$show_source_points) {
			for (source in tracker@path) {
				# print(glue::glue("Adding source points for {source}"))
				df = internal@df %>% filter(Source.Name == source) %>% select(ANID, internal@x, internal@y, Source.Name)
				tracker <<- addPointLayer(tracker, source)
				fig = fig +
					geom_point(df, 
						mapping=aes_string(x=internal@x, y=internal@y, color="Source.Name"),
						size=1.25,
						inherit.aes=FALSE)
			}
		} else if (length(tracker@point) > 0) { # Is this check necessary if the list is empty???
			for (source in tracker@point) {
				index = getPointIndex(tracker, source)

				# print(glue::glue("Removing source points for {source} at index {index}"))
				# print(tracker@point)

				tracker <<- removePointLayer(tracker, source)
				fig = fig %>% ggedit::remove_geom('point', index)
			}
		}
		fig <<- fig
	})

	# remove_source_from_plot = reactive({
	# 	print(glue::glue("Removing {internal@last_source} from plot"))
	# 	print(nrow(internal@df))
	# 	df = internal@df %>% filter(Source.Name != internal@last_source) %>% select(internal@x, internal@y, Source.Name)
	# 	print(nrow(df))
	# 	# fig <<- fig +
	# 	# 	stat_ellipse(data=df,
	# 	# 		mapping=aes_string(x=internal@x, y=internal@y, group="Source.Name", color="Source.Name")
	# 	# 	)
	# })

	#  = reactive({

	# })

	# fig = reactive({
	# 	initialize_plot()
	# 	redraw_plot()
	# 	return(fig)
	# })

	# element_plot = reactive({
	# 	p = fig

	# 	# # print(test)

	# 	# # print(paste("Check here:", typeof(fig)))
	# 	# if (input$show_source_data) {
	# 	# # 	# p = p + geom_point(mapping=aes(tooltip = tooltip, onclick = "",
	# 	# # 	# 								data_id = 1:nrow(ellipse)))#, data_id = ANID))
	# 	# 	p = p + geom_point(size=1)
	# 	# }

	# 	# # Handle uploaded data if there is any.
	# 	# print(input$upload_files)
	# 	# if (length(input$upload_files) > 0) {
	# 	# 	print("Adding input points")
	# 	# 	# add_input_points()
	# 	# 	upload = read.csv(input$upload_files$datapath)
	# 	# 	upload = select(upload, element_x(), element_y(), "SampleID", "SiteID")
	# 	# 	# print(names(upload))
	# 	# 	# cat(element_x(), element_y(), "\n")

	# 	# 	p = p + geom_point(
	# 	# 		data=upload,
	# 	# 		mapping=aes_string(x=element_x(), y=element_y(), group="SiteID", color="SiteID"),
	# 	# 		size=1.1,
	# 	# 		shape=17,
	# 	# 		alpha=0.75
	# 	# 	)
	# 	# }

	# 	# # cat("\tLength: ", !is.null(input$table_rows_selected), "\n")
	# 	# if (!is.null(input$table_rows_selected)) {
	# 	# 	p = p + add_selected_points()	
	# 	# }
	# })

	observeEvent(initialize_plot(), {
		print("Plot was initialized!")
	})

	updatePlot = observeEvent(c(
			initialize_plot(),
			selected_sources(),
			input$show_source_points
			# remove_source_from_plot()
		), {
			# for (i in names(fig)) {
			# 	fp = glue::glue("ggplot/{i}.txt")
			# 	print(fp)
			# 	# print(typeof(fig[i]))
			# 	lapply(fig[i], write, file=fp, append=TRUE)
			# }
			# print(names(fig))
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

	 # = pfig

	# #### Create the table
	# ### https://shiny.rstudio.com/articles/datatables.html
	# ### https://rstudio.github.io/DT/shiny.html
	# ## DT::renderDataTable() takes an expression that returns a rectangular data object with column names, such as a data frame or a matrix.
	# output$table <- DT::renderDT({
	# 	# print("Running 'renderDataTable()'.")
	# 	# data = get_data()
	# 	# print(typeof(iris))
	# 	# print(typeof(data))
	# 	# data
	# 	if (!is.null(input$file1)) {
	# 		# print("     Uploaded file!")
	# 		# print(input$file1)
	# 		# tab = read.csv("data/obsidian-NAA-database.csv")
	# 		tab = internal_df()
	# 		# tab
	# 	} else {
	# 		# print("     No uploaded file...")
	# 		# tab = data.frame(Sample=character(),
	# 			# stringsAsFactors=FALSE)
	# 		tab = internal_df()
	# 	}

	# 	tab = tab %>% select(ANID, Site.Country, Source.Name, Long.Date, input$element1, input$element2, NKT_edits)

	# 	# cat("Selected data:")
	# 	# print(input$table_cell_clicked)
	# 	# print(output)

	# 	DT::datatable(tab,
	# 		class="compact hover cell-border",
	# 		rownames=FALSE,
	# 		# filter="top",
	# 		extensions="FixedHeader", # https://rstudio.github.io/DT/extensions.html#fixedheader
	# 		options=list(
	# 			dom='fti',
	# 			columnDefs=list(list(
	# 				className="dt-center", targets="_all"
	# 			)),
	# 			pageLength=-1,
	# 			### 'fixedHeader' is not working and is likely a bug
	# 			### https://github.com/rstudio/DT/issues/389
	# 			fixedHeader=TRUE
	# 		),
	# 		editable=TRUE
	# 	)
	# })


}

# server
