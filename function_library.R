library(readxl)

elements = list("Manganese (ppm)"="Mn", "Rubidium (ppm)"="Rb", "Strontium (ppm)"="Sr",
                #"Zinc (zn)"="Zn")
                "Zirconium (ppm)"="Zr")

source_data = function() {
  data = readxl::read_excel("inbound/Obsidian_NAA_S-America_MURR2015.xlsx", 3)

  data = as.data.frame(data)

  # Replace spaces with underscores in column names.
  colnames(data) = sub(" ", "_", colnames(data))

  data
}