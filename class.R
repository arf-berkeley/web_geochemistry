# library("dplyr")

source("config.R")

data = read.csv("inbound/obsidian-NAA-database.csv")

Internal = setClass("Internal",
	slots = c(
		df="data.frame",
		country="character",
		sources="character"
	),
	prototype=list(
		df=data,
		country="",
		sources=c()
	)#,
	# validity=function(self) {
	# 	if (!(self@country %in% source_countries)) {
	# 		return("Invalid country")
	# 	}

	# }
)

setGeneric(name="setCountry",
	def=function(self, country) {
		standardGeneric("setCountry")
	}
)

setMethod(f="setCountry",
	signature="Internal",
	definition=function(self, country){
		self@country = country
		self@df = data %>% filter(Site.Country == self@country)
		return(self)
	}
)

setGeneric(name="setSources",
	def=function(self, source_list){
		standardGeneric("setSources")
	}
)

setMethod(f="setSources",
	signature="Internal",
	definition=function(self, source_list){
		self@sources = source_list
		self@df = self@df %>% filter(Source.Name %in% self@sources)
		return(self)
	}
)

setGeneric(name="addSource",
	def=function(self, source){
		standardGeneric("addSource")
	}
)

setMethod(f="addSource",
	signature="Internal",
	definition=function(self, source){
		self@sources = c(self@sources, source)
		self@df = self@df %>% rbind(filter(data, Source.Name == source))
		print(self@sources)
		return(self)
	}
)

setGeneric(name="removeSource",
	def=function(self, source){
		standardGeneric("removeSource")
	}
)

setMethod(f="removeSource",
	signature="Internal",
	definition=function(self, source){
		self@sources = self@sources[self@sources != source]
		self@df = self@df %>% filter(Source.Name != source)
		print(self@sources)
		return(self)
	}
)

print(typeof(c("Alca-1", "Chivay", "Puzolana")))
# # print(names(data))
internal_data = Internal()
internal_data = setCountry(internal_data, "Peru")
internal_data = setSources(internal_data, c("Alca-1", "Chivay", "Puzolana"))
print(nrow(internal_data@df))
print(internal_data@country)
print(internal_data@sources)
internal_data = addSource(internal_data, "Alca-5")
print(internal_data@sources)
print(nrow(internal_data@df))
internal_data = removeSource(internal_data, "Alca-5")
print(internal_data@sources)
print(nrow(internal_data@df))
internal_data = removeSource(internal_data, "Alca-1")
print(internal_data@sources)
print(nrow(internal_data@df))
# # print(internal_data)

internal_data = setCountry(internal_data, "Colombia")
print(nrow(internal_data@df))
print(internal_data@country)
internal_data = setSources(internal_data, c("Rio Hondo"))
print(internal_data@sources)
print(nrow(internal_data@df))

internal_data = removeSource(internal_data, c("Rio Hondo"))
print(internal_data@sources)
print(nrow(internal_data@df))