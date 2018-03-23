print("Running 'function_library.R'.")

library("readxl")

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
