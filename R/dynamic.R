# library("dplyr")
### Example of '%>%' usage: http://genomicsclass.github.io/book/pages/dplyr_tutorial.html#pipe-operator-

### Requires: 'config.R', but its already loaded by 'ui.R'
data = read.csv("data/obsidian-NAA-database.csv")

###########################################################
#' An S4 class for dynamically maintaining the data available to the application
#'
#' @slot df A data.frame containing all available data
#' @slot country Current country selected
#' @slot sources Current sources selected
#' @slot last_source Last source selected (should be deprecated in the future)
#' @slot x Current x element
#' @slot y Current y element
DynamicData = setClass("DynamicData",
	slots = c(
		df="data.frame",
		country="character",
		sources="character",
		last_source="character",
		x="character",
		y="character"
	),
	prototype=list(
		df=data.frame(),
		country="",
		sources=c(),
		last_source="",
		x="",
		y=""
	)#,
	# validity=function(self) {
	# 	if (!(self@country %in% source_countries)) {
	# 		return("Invalid country")
	# 	}

	# }
)


###########################################################
#' Set the default data for the dynamic data.frame
#'
#'
#' @param self The DynamicData() object
#' @param data A data.frame containing the default data
#' @return The updated DynamicData() object
setGeneric(name="setDefault", function(self, data) {
	standardGeneric("setDefault")
})

#// #//' @rdname DynamicData-methods
setMethod(f="setDefault",
	signature="DynamicData",
	definition=function(self, data) {
		self@df = data
		return(self)
	}
)

#' Set the current country for the dynamic data.frame
#'
#' @param self The DynamicData() object
#' @param country A string of the current country
#' @return The updated DynamicData() object
setGeneric(name="setCountry",
	def=function(self, country) {
		standardGeneric("setCountry")
	}
)

#// #//' @rdname DynamicData-methods
setMethod(f="setCountry",
	signature="DynamicData",
	definition=function(self, country) {
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
	signature="DynamicData",
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
	signature="DynamicData",
	definition=function(self, source){
		self@sources = c(self@sources, source)
		self@df = self@df %>% rbind(filter(data, Source.Name == source))
		self@last_source = source
		return(self)
	}
)

setGeneric(name="removeSource",
	def=function(self, source){
		standardGeneric("removeSource")
	}
)

setMethod(f="removeSource",
	signature="DynamicData",
	definition=function(self, source){
		self@sources = self@sources[self@sources != source]
		self@df = self@df %>% filter(Source.Name != source)
		self@last_source = source
		return(self)
	}
)

setGeneric(name="getSources",
	def=function(self){
		standardGeneric("getSources")
	}
)

setMethod(f="getSources",
	signature="DynamicData",
	definition=function(self){
		return(self@sources)
	}
)

setGeneric(name="setx",
	def=function(self, x){
		standardGeneric("setx")
	}
)

setMethod(f="setx",
	signature="DynamicData",
	definition=function(self, x){
		self@x = x
		return(self)
	}
)

setGeneric(name="sety",
	def=function(self, y){
		standardGeneric("sety")
	}
)

setMethod(f="sety",
	signature="DynamicData",
	definition=function(self, y){
		self@y = y
		return(self)
	}
)

setGeneric(name="addSelection",
	def=function(self, row) {
		standardGeneric("addSelection")
	}
)

setMethod(f="addSelection",
	signature="DynamicData",
	definition=function(self, row){
		self@selection = rbind(self@selection, row)
		print(nrow(self@selection))
		return(self)
	}
)


# print(typeof(c("Alca-1", "Chivay", "Puzolana")))
# # # print(names(data))
# internal_data = DynamicData()
# internal_data = setCountry(internal_data, "Peru")
# internal_data = setSources(internal_data, c("Alca-1", "Chivay", "Puzolana"))
# print(nrow(internal_data@df))
# print(internal_data@country)
# print(internal_data@sources)
# internal_data = addSource(internal_data, "Alca-5")
# print(internal_data@sources)
# print(nrow(internal_data@df))
# internal_data = removeSource(internal_data, "Alca-5")
# print(internal_data@sources)
# print(nrow(internal_data@df))
# internal_data = removeSource(internal_data, "Alca-1")
# print(internal_data@sources)
# print(nrow(internal_data@df))
# # # print(internal_data)

# internal_data = setCountry(internal_data, "Colombia")
# print(nrow(internal_data@df))
# print(internal_data@country)
# internal_data = setSources(internal_data, c("Rio Hondo"))
# print(internal_data@sources)
# print(nrow(internal_data@df))

# internal_data = removeSource(internal_data, c("Rio Hondo"))
# print(internal_data@sources)
# print(nrow(internal_data@df))




library("ggplot2")

###########################################################
#' An S4 class for dynamically tracking the layers (point or path) in conjunction with a ggplot2 figure
#'
#' @slot point A list of path layer names (first in, last out)
#' @slot path A list of point layer names (first in, last out)
#' @slot layer A complete list of layer names (point and path, first in, first out) 
#' @slot selection Current point selection
DynamicPlot = setClass("DynamicPlot",
	slots = c(
		point="character",
		path="character",
		layer="character",
		selection="data.frame"
	),
	prototype=list(
		point=c(),
		path=c(),
		layer=c(),
		selection=data.frame()
	)
)

###########################################################
#' Add layer to a DynamicPlot object
#'
#' @details This should only be called internally by addPoint() or addPath()
#'
#' @param self DynamicPlot object
#' @param id A unique id to define the layer (such as the source name)
#' @return Updated DynamicPlot object
setGeneric(name="addLayer", function(self, id) {
	standardGeneric("addLayer")
})

#// #//' @rdname DynamicPlot-methods
setMethod(f="addLayer", signature="DynamicPlot", function(self, id) {
	self@layer = c(self@layer, id)
	return(self)
})

# ##########################################################
#' Remove layer from a DynamicPlot object
#'
#' @details This should only be called internally by removePoint() or removePath()
#'
#' @param self DynamicPlot object
#' @param id A unique id to define the layer (such as the source name)
#' @return Updated DynamicPlot object
setGeneric(name="removeLayer", function(self, id) {
	standardGeneric("removeLayer")
})

#// #//' @rdname DynamicPlot-methods
setMethod(f="removeLayer", signature="DynamicPlot", function(self, id) {
	self@layer = self@layer[!(self@layer == id)]
	return(self)
})

# ##########################################################
#' Get the layer id for a given index
#'
# ' @details This should only be called internally by removePoint() or removePath()
#'
#' @param self DynamicPlot object
#' @param index Index of layer
#' @return The layer id
setGeneric(name="getLayer", function(self, index) {
	standardGeneric("getLayer")
})

#// #//' @rdname DynamicPlot-methods
setMethod(f="getLayer", signature="DynamicPlot", function(self, index) {
	return(self@layer[index+1])
})


setGeneric(name="addPoint",
	def=function(self, id) {
		standardGeneric("addPoint")
	}
)

setMethod(f="addPoint",
	signature="DynamicPlot",
	definition=function(self, id){
		self@point = c(self@point, id)
		self = addLayer(self, id)
		return(self)
	}
)

setGeneric(name="removePoint",
	def=function(self, id) {
		standardGeneric("removePoint")
	}
)

setMethod(f="removePoint",
	signature="DynamicPlot",
	definition=function(self, id){
		# print(paste("From:", length(self@point)))
		self@point = self@point[!(self@point == id)]
		self = removeLayer(self, id)
		# print(paste("To:", length(self@point)))
		return(self)
	}
)

setGeneric(name="getPointIndex",
	def=function(self, id) {
		standardGeneric("getPointIndex")
	}
)

setMethod(f="getPointIndex",
	signature="DynamicPlot",
	definition=function(self, id){
		# print(glue::glue("Looking for {id}"))
		index = match(id, self@point)
		# prijnt(glue::glue("Found at index {index}"))
		return(index)
	}
)


setGeneric(name="addPath",
	def=function(self, id) {
		standardGeneric("addPath")
	}
)

setMethod(f="addPath",
	signature="DynamicPlot",
	definition=function(self, id){
		self@path = c(self@path, id)
		self = addLayer(self, id)
		return(self)
	}
)

setGeneric(name="removePath",
	def=function(self, id) {
		standardGeneric("removePath")
	}
)

setMethod(f="removePath",
	signature="DynamicPlot",
	definition=function(self, id){
		self@path = self@path[!(self@path == id)]
		self = removeLayer(self, id)
		return(self)
	}
)

setGeneric(name="getPathIndex",
	def=function(self, id) {
		standardGeneric("getPathIndex")
	}
)

setMethod(f="getPathIndex",
	signature="DynamicPlot",
	definition=function(self, id){
		return(match(id, self@path))
	}
)


result = function(self) {
	cat("Points:", paste(self@point, collapse=", "), "\n")
	cat("Paths:", paste(self@path, collapse=", "), "\n")
	cat("Layers:", paste(self@layer, collapse=", "), "\n\n")
}

# test = DynamicPlot()

# label = "point1"
# print(glue::glue("addPoint('{label}')"))
# test = addPoint(test, label)
# result(test)

# label = "path1"
# print(glue::glue("addPath('{label}')"))
# test = addPath(test, label)
# result(test)

# label = "point2"
# print(glue::glue("addPoint('{label}')"))
# test = addPoint(test, label)
# result(test)

# label = "path2"
# print(glue::glue("addPath('{label}')"))
# test = addPath(test, label)
# result(test)

# label = "point1"
# print(glue::glue("removePoint('{label}')"))
# test = removePoint(test, label)
# result(test)

# label = "path1"
# print(glue::glue("removePath('{label}')"))
# test = removePath(test, label)
# result(test)

# print(getLayer(test, 1))
# print(getLayer(test, 2))
# print(getLayer(test, 3)) ### Should be 'NA' since there are only 2 elements in the list

