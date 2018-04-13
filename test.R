# library(readxl)
# library(dplyr) # Includes: filter()

# source_data = function() {
# 	data = readxl::read_excel("data/Obsidian_NAA_S-America_MURR2015.xlsx", 3)

# 	data = as.data.frame(data)

# 	# Replace spaces with underscores in column names.
# 	colnames(data) = sub(" ", "_", colnames(data))

# 	data
# }

data = read.csv("data/obsidian-NAA-database.csv")

typeof(data)
colnames(data) = gsub(" ", "_", colnames(data))
colnames(data) = sapply(colnames(data), tolower)

# names(data)
# unique(data$site_country)
new = dplyr::filter(data, source.country == "Bolivia")
names(new)
# print(data[2,]$Site_Name)
# write.csv(new, "test.csv")

tabcols = c("alternate.id", "ba", "la")
head(new[,tabcols], 5)