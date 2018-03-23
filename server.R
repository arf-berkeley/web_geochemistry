print("Running 'server.R'.")

library("dplyr")
library("ggplot2")

### No need because both are already in 'ui.R', which is loaded first
# library("shiny") 
# source("function_library.R")

server = function(input, output) {

	get_data <- reactive({
		# Sorts through the NAA database and extracts necessary columns for plotting.
		# Returns:
		#	List of x, y, anid, source, and tooltip.
		print("Running 'get_data()'.")
		data = read.csv("inbound/obsidian-NAA-database.csv")
		# data = source_data()

		### Bias based on region...
		data = data[data$Site.Country == input$region, ]

		if ("source_group" %in% names(input) && (length(input$source_group) == 0 || sum(input$source_group %in% data$Source.Name) > 0)) {
			source_group = input$source.group
		} else {
			# Restrict to the top 4 sources.
			source_counts = table(data$Source.Name)
			df = data.frame(source=names(source_counts), count=as.numeric(source_counts))
			df = df[order(df$count, decreasing=T), ]
			# Up to 4 - if df has fewer than 4 rows then just show everything (e.g. small region).
			source_group = df[1:min(4, nrow(df)), "source"]
		}

		# Restrict to the selected sources.
		data = data[data$Source.Name %in% source_group, ]

		if (nrow(data) == 0) return(data.frame()) ### Some arbitrary empty catch?

		data$tooltip = with(data, paste0(
			"ANID: ", ANID, "<br />",
			"Source: ", Source.Name, "<br />",
			"Investigator: ", Investigator.Name)
		)

		data = data[, c(input$element1, input$element2, "ANID", "Source.Name", "tooltip")]
		cat(input$element1, input$element2)
		colnames(data) = c("x", "y", "anid", "source", "tooltip")
		remove_count = sum(data$x == 0 | data$y == 0)
		if (remove_count > 0) {
			cat("Removing rows where x or y is 0:", remove_count, "\n")
		}

		# Remove rows where x or y is 0.
		data = data[data$x != 0 & data$y != 0, ]

		print(names(data))
		data
	})

	init = reactive({
		print("Running 'init()'.")
		data = read.csv("inbound/obsidian-NAA-database.csv")
	})

	#### Create the table
	output$table <- renderDataTable({
		print("Running 'renderDataTable()'.")
		# data = get_data()
		# # print(typeof(iris))
		# # print(typeof(data))
		# data
		if (!is.null(input$file1)) {
			print("     Uploaded file!")
			print(input$file1)
			tab = read.csv("inbound/obsidian-NAA-database.csv")
			# tab
		} else {
			print("     No uploaded file...")
			tab = data.frame(Sample=character(),
				stringsAsFactors=FALSE)
		}
	})

	output$plot <- plotly::renderPlotly({
		print("Running 'renderPlotly()'.")

		data = get_data()
		# data = init()

		### Example of '%>%' usage: https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
		ellipse = data %>% group_by(source) %>% filter(n() >= 3)
		ellipse = na.omit(ellipse) %>% group_by(source)

		p = ggplot(ellipse,
				mapping=aes(x=x, y=y, group=source, color=source)
			) +
			stat_ellipse() +
			xlab(names(elements)[elements == input$element1]) +
			ylab(names(elements)[elements == input$element2]) +
			theme_minimal() +
			guides(color=guide_legend(nrow=2)) +
			theme(#plot.background = element_rect(color = "#e3e3e3", fill="#f7f7f7"),
				panel.background = element_rect(fill="white"),
				legend.position = "bottom",
				legend.title = element_blank(),
				panel.border = element_rect(fill=NA, colour="#e3e3e3")
			)

		# Re-enable this once ellipses are working.
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
						mapping=aes(x=X, y=Y, group=SiteID, SampleID=SampleID,
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

		margin = list(l = 0, r = 300, b = 0, t = 50, pad = 0)
		p = plotly::ggplotly(p, tooltip = c("X", "Y", "SiteID", "SampleID")) %>%
			plotly::layout(margin = list(t = 50)) %>%
			plotly::config(displayModeBar = F)

		### Fixes 'Warning in origRenderFunc(): Ignoring explicitly provided widget ID'
		### https://github.com/ropensci/plotly/issues/985
		p$elementId = NULL
		#####

		p
	})

	output$source_checkbox_group <- renderUI({
		print("Running 'renderUi()'.")
		data = source_data()
		data = data[data$Site.Country == input$region, ]

		source_counts = table(data$Source.Name)
		df = data.frame(source=names(source_counts), count=as.numeric(source_counts))

		# Must have at least 3 observations.
		df = df[df$count >= 3, ]

		choices = as.list(df$source)

		names(choices) = paste0(df$source, " (", df$count, ")")

		if ("source_group" %in% names(input) && (length(input$source_group) == 0 || sum(input$source_group %in% data$Source.Name) > 0)) {
			selected = input$source_group
		} else {
			# Select the top 5 sources by default.
			df = df[order(df$count, decreasing=TRUE), ]
			# Select top 5, or every row if the df has fewer than 5 rows.
			selected = df[1:min(5, nrow(df)), "source"]
		}

		ui = checkboxGroupInput("source_group",
			label = "",
			choices = choices,
			selected = selected
		)
		ui
	})

}

server
