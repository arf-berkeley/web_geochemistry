print("Running 'config.R'.")

# library("readxl")
library("dplyr")

xrf_elements = list("Iron"="Fe", "Manganese"="Mn", "Neodynium"="Nd", "Rubidium"="Rb",
	"Strontium"="Sr", "Yttrium"="Y", "Zirconium"="Zr")

naa_elements = list("Barium"="Ba", "Lanthanum"="La", "Lutetium"="Lu",
	"Samarium"="Sm", "Uranium"="U", "Ytterbium"="Yb", "Cerium"="Ce",
	"Cobalt"="Co", "Caesium"="Cs", "Europium"="Eu",
	"Hafnium"="Hf", "Antimony"="Sb", "Scandium"="Sc", 
	"Tantalum"="Ta", "Terbium"="Tb", "Thorium"="Th",
	"Zinc"="Zn", "Aluminium"="Al", "Chlorine"="Cl",
	"Dysprosium"="Dy", "Potassium"="K", "Sodium"="Na")

elements = xrf_elements

# my_border_color="#E2E2E2"
my_border_color="#BCE8F1" # From shinyBS 'primary'
# my_background_color="#F0F8FF"
my_background_color="#D9EDF7" # From shinyBS 'primary'
# my_background_color="#F5F5F5"

my_sidepanel_style="margin:0px 0px 5px;border-radius:5px;padding:2px 10px 4px;background-color:{my_background_color};border:1px solid {my_border_color};"
my_sidepanel_style=glue::glue(my_sidepanel_style)

### Source the data when the session begins (occurs for both ui.R and server.R)
data = read.csv("data/MURR-NAA.csv")

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

source_country_table = data %>% select(Site.Country, Site.Name) %>% distinct(Site.Country, Site.Name) %>% select(Site.Country) %>% table
source_countries = names(source_country_table) # Used for input validation in 'config.R'
source_country_df = data.frame(source_country_table)

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