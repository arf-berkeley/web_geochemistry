library(shiny)
library(plotly)
library(ggplot2)
library(readxl)
library(dplyr)
#library(ggiraph)
library(shinyBS)

source("function_library.R")

server = function(input, output) {

  get_data <- reactive({
  #get_data <- function() {
    data = source_data()

    data = data[data$Site_Country == input$region, ]
    #print(names(input))
    #print(names(input$sources))

    if ("source_group" %in% names(input) &&
        (length(input$source_group) == 0 ||
         sum(input$source_group %in% data$Source_Name) > 0)) {
      source_group = input$source_group
      #cat("Source group is now:", paste(source_group), "\n")
    } else {
      # Restrict to the top 5 sources.
      source_counts = table(data$Source_Name)
      df = data.frame(source=names(source_counts), count=as.numeric(source_counts))
      df = df[order(df$count, decreasing=T), ]
      # Up to 5 - if df has fewer than 5 rows then just show everything (e.g. small region).
      source_group = df[1:min(5, nrow(df)), "source"]
      #cat("Using top 5 sources.\n")
    }

    # Restrict to the selected sources.
    data = data[data$Source_Name %in% source_group, ]

    if (nrow(data) == 0) return(data.frame())

    data$tooltip = with(data, paste0(
      "ANID: ", ANID, "<br />",
      "Source: ", Source_Name, "<br />",
      "Investigator: ", Investigator_Name)
    )

    data = data[, c(input$element1, input$element2, "ANID", "Source_Name", "tooltip")]
    # Set rownames after subsetting because ANID has duplication in the full df.
    #rownames(data) = data$ANID

    #data = subset(data, select = -ANID)
    # De-duplicate by anid.

    colnames(data) = c("x", "y", "anid", "source", "tooltip")

    remove_count = sum(data$x == 0 | data$y == 0)
    if (remove_count > 0) {
      cat("Removing rows where x or y is 0:", remove_count, "\n")
    }

    # Remove rows where x or y is 0.
    data = data[data$x != 0 & data$y != 0, ]

    data
  })
  #}

  # Normal Shiny:
  #output$plot <- renderPlot({
  # ggiraph:
  #output$plot <- renderggiraph({
  # Plotly:
  output$plot <- plotly::renderPlotly({
    # Make sure this is updated.
    input$source_group

    data = get_data()

    if (nrow(data) == 0) return(NULL)

    #ellipse = data %>% filter(count(source) > 3)
    ellipse = data %>% group_by(source) %>% filter(n() >= 3)
    ellipse = na.omit(ellipse) %>% group_by(source)
    ellipse$source = as.factor(ellipse$source)
    #cat("Ellipse rows:", nrow(ellipse), "\n")
    #print(names(ellipse))
    #print(names(input))

    # Must have 3 or more observations to plot an ellipse
    #p <- ggplot(data, aes(x = x, y = y)) +
    p <- ggplot(ellipse, mapping=aes(x = x, y = y, group = source, color = source, label=anid)) +
      ggtitle("Archeological Geochemistry") +
      stat_ellipse(aes(group=source)) +
      xlab(names(elements)[elements == input$element1]) +
      ylab(names(elements)[elements == input$element2]) +
      #theme_minimal() + theme(legend.position="none")
      theme_minimal() +
      guides(color = guide_legend(nrow = 2)) +
      theme(#plot.background = element_rect(color = "#e3e3e3", fill="#f7f7f7"),
            panel.background = element_rect(fill = "white"),
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.border = element_rect(fill = NA, colour = "#e3e3e3")#,
     #       legend.position = "none"
      )


    if (F) {
      p2 <- ggplot(data, aes(x = x, y = y, colour=source)) +
        stat_ellipse(aes(x = x, y = y, group=source), data=ellipse)
      p2

      ellipse2 = ellipse[ellipse$y != 0, ]

      p3 <- ggplot(ellipse2, aes(x = x, y = y, group=source)) +
        stat_ellipse(mapping=aes(color=source)) + theme(legend.position="none")
      p3
    }

    if (input$show_source_data) {
      #p = p + geom_point()
      #p = p + geom_point_interactive(mapping=aes(tooltip = tooltip, onclick = "", data_id = 1:nrow(ellipse)))#, data_id = ANID))
      p = p + geom_point(mapping=aes(tooltip = tooltip, onclick = "", data_id = 1:nrow(ellipse)))#, data_id = ANID))
    }

    if (input$label_source_points) {
      p = p + geom_text(data=ellipse, check_overlap=T, size=2.5)
    }


    # Handle uploaded data if there is any.
    if (!is.null(input$file1)) {
      upload = read.csv(input$file1$datapath)
      if (!"SiteID" %in% names(upload)) {
        upload$SiteID = ""
      }
      upload = upload[, c(input$element1, input$element2, "SampleID", "SiteID")]
      colnames(upload) = c("x", "y", colnames(upload)[3:4])
      upload$tooltip = paste0("Sample ID: ", upload$SampleID,
              ifelse(upload$SiteID == "", "", paste0("<br />Site ID: ", upload$SiteID)))
      #p = p + geom_point_interactive(aes(x=x, y=y, data=upload))
      #p = p + geom_point_interactive(data=upload,
      p = p + geom_point(data=upload,
                #aes(x=x, y=y, group=SiteID, color=SiteID, alpha = 0.8,
                mapping=aes(x=x, y=y, group=SiteID,
                    color=factor(SiteID, labels=paste("Site", unique(SiteID))),
                    alpha = 0.8, label = NULL,
                    #shape=I(15),
                    tooltip = tooltip, data_id = 1:nrow(upload))) +
        guides(alpha=F) + scale_color_discrete()
        #scale_size(guide = F)
        #scale_shape_identity()
    }

    #print(p)
    #ggiraph(code = print(p), width = 0.8, hover_css = "fill-opacity:.3;cursor:pointer;"#,
           # selection_type = "multiple")
            #zoom_max = 5
    #        )

    margin = list(l = 0, r = 300, b = 0, t = 50, pad = 0)
    #margin = list()
    p = plotly::ggplotly(p) %>% plotly::layout(orientation = "h")  %>%
      config(displayModeBar = F)
    p

  })#, height=700)

  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(source_data(), hover, xvar = "x", yvar = "y", threshold = 5,
                        maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0)  return(NULL)
    else print("Found point!")

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", rownames(point), "<br/>",
                    "<b> x: </b>", point$x, "<br/>",
                    "<b> y: </b>", point$y, "<br/>",
                    "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
    )
  })

  output$source_checkbox_group <- renderUI({
    data = source_data()
    data = data[data$Site_Country == input$region, ]

    source_counts = table(data$Source_Name)
    df = data.frame(source=names(source_counts), count=as.numeric(source_counts))

    # Must have at least 3 observations.
    df = df[df$count >= 3, ]

    choices = as.list(df$source)

    names(choices) = paste0(df$source, " (", df$count, ")")

    if ("source_group" %in% names(input) &&
        (length(input$source_group) == 0 ||
          sum(input$source_group %in% data$Source_Name) > 0)) {
      #print(input$source_group)
      selected = input$source_group
    } else {
      # Select the top 5 sources by default.
      df = df[order(df$count, decreasing=T), ]
      # Select top 5, or every row if the df has fewer than 5 rows.
      selected = df[1:min(5, nrow(df)), "source"]
    }

    ui = checkboxGroupInput("source_group",
                            label = "",
                            #label = h3("Checkbox group"),
                            choices = choices,
                            selected = selected
    )
    ui
  })

}

server
