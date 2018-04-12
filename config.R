print("Running 'config.R'.")

library("readxl")
library("dplyr")

# Ba	La	Lu	Nd	Sm	U	Yb	Ce	Co	Cs	Eu	Fe	Hf	Rb	Sb	Sc	Sr	Ta	Tb	Th	Zn	Zr	Br	Al	Cl	Dy	K	Mn	Na
elements = list("Manganese (ppm)" = "Mn",
                "Iron (ppm)" = "Fe",
                "Niobium (ppm)" = "Nb",
                "Rubidium (ppm)" = "Rb",
                "Strontium (ppm)" = "Sr",
                "Yttrium (ppm)" = "Y",
                #"Zinc (zn)"="Zn")
                "Zirconium (ppm)" = "Zr")

source_data = function() {
  data = readxl::read_excel("inbound/Obsidian_NAA_S-America_MURR2015.xlsx", 3)

  data = as.data.frame(data)

  # Replace spaces with underscores in column names.
  colnames(data) = sub(" ", "_", colnames(data))

  data
}

my_border_color="#E2E2E2"
my_background_color="#F0F8FF"
# my_background_color="#F5F5F5"

my_sidepanel_style="margin:10px 0px 15px;border-radius:20px;padding:3px 10px 0px;background-color:{my_background_color};border:1px solid {my_border_color};"
my_sidepanel_style=glue::glue(my_sidepanel_style)

### Source the data when the session begins (occurs for both ui.R and server.R)
data = read.csv("inbound/obsidian-NAA-database.csv")

vectorof_count_labels = function(df) {
	# Used for string formating of labels for both source country and source displayed in 'ui.R'
	# Calls apply() over the ROWS of a data.frame
	#
	# Returns a formatted string:
	#	"name (count)"
	count_label = function(row) {
		label = row[1]
		count = gsub("[[:space:]]", "", row[2])
		glue::glue("{label} ({count})")
	}
	temp = apply(df, 1, count_label)
	return(as.vector(temp))
}

extract_count_label = function(label) {
	# REGEX to capture the 'label' and extract it from the associated 'count'
	# Example:
	#	"Laguna del Diamante (21)" -> "Laguna del Diamante"
	# Test REGEX here: https://regex101.com/
	# Note: R requires an extra escape character '/' to escape the escape character '/' (meta-af)
	return(gsub("(.*)\\s\\(\\d+\\)", "\\1", label))
}

vectorof_labels = function(count_labels) {
	temp = sapply(count_labels, extract_count_label)
	return(as.vector(temp))
}

source_country_table = table(data$Site.Country)
source_countries = names(source_country_table)
source_country_df = data.frame(source_country_table)
# print(head(source_country_df))

### Error handling for no countries in the file
if (length(source_countries) == 0) {
	stop("No source countries")
}

country_labels = vectorof_count_labels(source_country_df)

available_source_table = function(country) {
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
	df = data %>% filter(Site.Country == country) %>% select(Source.Name) %>% table %>% data.frame
	colnames(df) = c("Source.Name", "Count")

	### Filters the 'Source.Name' that appear less than three times or are empty
	present = df %>% filter(Count >= 3 & Source.Name != "")
	return(present)
}

available_source_count_labels = function(country) {
	return(vectorof_count_labels(available_source_table(country)))
}

default_source_count_labels = function(country) {
	available = available_source_table(country)
	default = available %>% arrange(desc(Count)) %>% head(5)
	return(vectorof_count_labels(default))
}

default_sources = function(country) {
	return(vectorof_labels(default_source_count_labels(country)))
}