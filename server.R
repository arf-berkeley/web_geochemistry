print("Running 'server.R'.")

# library("dplyr")
library("ggplot2")

### No need because both are already in 'ui.R', which is loaded first
# library("shiny") 
source("config.R")
source("class.R")
print("")
print("")

server = function(input, output) {

	internal = Internal()
	# internal = setCountry(internal, "Peru")

	current_country = reactive({
		return(extract_count_label(input$country))
	})

	check_country = observeEvent(current_country(), {
		internal = setCountry(internal, current_country())
		internal = setSources(internal, default_sources(internal@country))
		internal <<- internal
	})

	current_sources = reactive({
		if (is.null(input$selected_source_labels)) {
			current = default_sources(current_country())
		} else {
			current = vectorof_labels(input$selected_source_labels)
		}
		return(current)
	})


	check_sources = observeEvent(current_sources(), {
		### Example of '%>%' usage: http://genomicsclass.github.io/book/pages/dplyr_tutorial.html#pipe-operator-
		before = internal@sources
		after = current_sources()

		add = after[!(after %in% before)]
		remove = before[!(before %in% after)]
		if (length(add) > 0) {
			# print(paste("Add:", add))
			internal = addSource(internal, add)
			internal <<- internal
		} else if (length(remove) > 0) {
			# print(paste("Remove:", remove))
			internal = removeSource(internal, remove)
			internal <<- internal
		} #else {
			# print("No new source")
		# }
	})

	# info = observe({
	# 	# cat("\n", Sys.time(), "\n")
	# 	# cat("Current Country:\n")
	# 	# print(current_country())
	# 	# available_source_counts()
	# 	# cat("Current Sources:\n")
	# 	# print(current_sources())
	# 	# print(glue::glue("Current Elements: {element_x()} {element_y()}"))
	# 	# cat("\n")
	# 	print(paste("internal_df():", typeof(internal_df())))
	# })

	internal_df = reactive({
		temp = data %>% filter(Site.Country == current_country()) %>% filter(Source.Name %in% isolate(current_sources()))
		# temp = temp %>% filter(element_x() > 0 & element_y() > 0)
		temp = temp %>% group_by(Source.Name)
		return(temp)
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
			choices = available_source_count_labels(internal@country),
			selected = default_source_count_labels(internal@country)
		)
	})

	selected_points = reactive({
		### This makes the table reactive to points being selected
		selection = input$table_rows_selected
		data = internal_df() %>% ungroup()

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

	grab_plot_point = function(data, x, y) {
		print(paste("Looking for", element_x(), "=", x, "and", element_y(), "=", y))
		# print(names(data))
		# temp = data %>% filter(element_x() == x)
		# print(nrow(filter(data, element_x() > x)))
		return(which((data[element_x()] == x) & (data[element_y()] == y)))
	}

	# add_selected_plot_point = reactive({

	# })

	# initialize_plot = {
		
	# 	fig = 

	# 	return(fig)
	# }

	fig = reactive({
		print("Initializing the plot")

		ggplot(isolate(internal_df()),
				# mapping=aes_string(x=element_x(), y=element_y(), group="Source.Name", color="Source.Name")
				# mapping=aes_string(x=element_x(), y=element_y(), label="ANID", group="Source.Name", color="Source.Name",
				mapping=aes_string(x=isolate(element_x()), y=isolate(element_y()), group="Source.Name", color="Source.Name",
					hoverinfo=FALSE)
			) +
			stat_ellipse() +
			xlab(names(elements)[elements == isolate(element_x())]) +
			ylab(names(elements)[elements == isolate(element_y())]) +
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
	})

	element_plot = reactive({
		p = fig()

		# # print(test)

		# # print(paste("Check here:", typeof(fig)))
		# if (input$show_source_data) {
		# # 	# p = p + geom_point(mapping=aes(tooltip = tooltip, onclick = "",
		# # 	# 								data_id = 1:nrow(ellipse)))#, data_id = ANID))
		# 	p = p + geom_point(size=1)
		# }

		# # Handle uploaded data if there is any.
		# print(input$upload_files)
		# if (length(input$upload_files) > 0) {
		# 	print("Adding input points")
		# 	# add_input_points()
		# 	upload = read.csv(input$upload_files$datapath)
		# 	upload = select(upload, element_x(), element_y(), "SampleID", "SiteID")
		# 	# print(names(upload))
		# 	# cat(element_x(), element_y(), "\n")

		# 	p = p + geom_point(
		# 		data=upload,
		# 		mapping=aes_string(x=element_x(), y=element_y(), group="SiteID", color="SiteID"),
		# 		size=1.1,
		# 		shape=17,
		# 		alpha=0.75
		# 	)
		# }

		# # cat("\tLength: ", !is.null(input$table_rows_selected), "\n")
		# if (!is.null(input$table_rows_selected)) {
		# 	p = p + add_selected_points()	
		# }

		# print("Converting...")
		p = plotly::ggplotly(p,
				# tooltip=c(element_x(), element_y(), "label")
				tooltip=c(element_x(), element_y())
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

		# print(length(current_sources()))
		# print(p$hoverinfo)
		
		### Loops through the currently selected sources and removes their 'tooltip'
		### '$x$data[[i]' is split into lists of the assigned 'groups'
		for (i in 1:length(current_sources())) {
			p$x$data[[i]]$hoverinfo = "none"
		}
		# p$x$data[[2]]$hoverinfo = "none"

		# ### Fixes 'Warning in origRenderFunc(): Ignoring explicitly provided widget ID'
		# ### https://github.com/ropensci/plotly/issues/985
		p$elementId = NULL
		# #####

		return(p)
	})

	output$plot <- plotly::renderPlotly({
		element_plot()
		# fig()

		# data = internal_df()
		# print(element_x())
		# print(select(data, element_x()))

		# ggplot(mtcars, aes(x=wt, y=mpg)) +
		# 	geom_point(size=2, shape=23)
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
			tab = internal_df()
			# tab
		} else {
			# print("     No uploaded file...")
			# tab = data.frame(Sample=character(),
				# stringsAsFactors=FALSE)
			tab = internal_df()
		}

		tab = tab %>% select(ANID, Site.Country, Source.Name, Long.Date, input$element1, input$element2, NKT_edits)

		# cat("Selected data:")
		# print(input$table_cell_clicked)
		# print(output)

		DT::datatable(tab,
			class="compact hover cell-border",
			rownames=FALSE,
			# filter="top",
			extensions="FixedHeader", # https://rstudio.github.io/DT/extensions.html#fixedheader
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


}

server
