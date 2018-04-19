library("dplyr")
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
		print(paste("Check here:", x))
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
#' An S4 class for dynamically building and tracking the layers
#' (point or path) of a ggplot2 object
#'
#' @author Jesse Norris \email{jesse.norris@berkeley.edu}
#'
#' @slot plot Plot created by a ggplot2 object
#' @slot point A list of path layer ids (first in, last out)
#' @slot path A list of point layer ids (first in, last out)
#' @slot layer A complete list of layer ids (point and path, first in, first out) 
#' @slot selection Data.Frame of selected values for emphasis
DynamicPlot = setClass("DynamicPlot",
	slots = c(
		plot="gg",
		point="character",
		path="character",
		layer="character",
		selection="data.frame"
	)
)
############################################################

############################################################
#' Initialize a ggplot2 object
#'
#' Redefines the 'plot' slot with a new ggplot2 object.
#'
#' @note The ggplot2 object used to initialize the DynamicPlot object should
#'	have no layers.
#'
#' @seealso \code{\link{addPoint}} for adding 'point' layers and \code{\link{addPath}}
#'	for adding 'path' layers
#'
#' @param self DynamicPlot object
#' @param layer The initial plot layer as ggplot2 object
#' @return Updated DynamicPlot object
setGeneric(name="initializePlot", function(self, layer) {
	standardGeneric("initializePlot")
})

#' @describeIn DynamicPlot Initialize a ggplot2 object
setMethod(f="initializePlot", signature="DynamicPlot", function(self, layer) {
	self@plot = layer
	return(self)
})

############################################################
#' Add a plot element to the ggplot2 object
#'
#' @note \code{\link{addLayer}} should only be called internally by 
#'	\code{\link{addPoint}} or \code{\link{addPath}} as it does not 
#'	manipulate the ggplot2 object, only the list tracking the layer ids.
#'
#' @name  Add-DynamicPlot
#' @param self DynamicPlot object
#' @param id id to define the layer (such as the source name)
#' @param type Type of the layer ("point" or "path")
#' @return Updated DynamicPlot object
NULL
############################################################

#' @rdname Add-DynamicPlot
setGeneric(name="addLayer", function(self, id, type) {
	standardGeneric("addLayer")
})

setMethod(f="addLayer", signature="DynamicPlot", function(self, id, type) {
	self@layer = c(self@layer, paste(id, type))
	return(self)
})

#' @rdname Add-DynamicPlot
setGeneric(name="addPoint", function(self, element, id) {
	standardGeneric("addPoint")
})

#' @describeIn DynamicPlot Add 'point' layer
setMethod(f="addPoint", signature="DynamicPlot", function(self, element, id){
	### Add the new point element to the plot
	self@plot = self@plot %+% element

	### Append the layer id to the point and layer lists
	self@point = c(self@point, id)
	self = addLayer(self, id, "point")
	return(self)
})

#' @rdname Add-DynamicPlot
setGeneric(name="addPath", function(self, element, id) {
	standardGeneric("addPath")
})

#' @describeIn DynamicPlot Add 'path' layer
setMethod(f="addPath", signature="DynamicPlot", function(self, element, id){
	### Add the new point element to the plot
	self@plot = self@plot %+% element

	self@path = c(self@path, id)
	self = addLayer(self, id, "path")
	return(self)
})

############################################################
#' Remove a plot element from the ggplot2 object
#'
#' Removing the elements from the ggplot2 object is preformed with
#'	the \pkg{ggedit} CRAN package:
#'		\describe{
#'			\item{}{\url{https://cran.r-project.org/web/packages/ggedit/}}	
#'		}
#'
#' @note \code{\link{removeLayer}} should only be called internally by 
#'	\code{\link{removePoint}} or \code{\link{removePath}} as it does not 
#'	manipulate the ggplot2 object, only the list tracking the layer ids.
#'
#' @name  Remove-DynamicPlot
#' @param self DynamicPlot object
#' @param id id of the layer to remove
#' @param type Type of the layer ("point" or "path")
#' @return Updated DynamicPlot object
NULL
############################################################

#' @rdname Remove-DynamicPlot
setGeneric(name="removeLayer", function(self, id, type) {
	standardGeneric("removeLayer")
})

setMethod(f="removeLayer", signature="DynamicPlot", function(self, id, type) {
	self@layer = self@layer[!(self@layer == paste(id, type))]
	return(self)
})

#' @rdname Remove-DynamicPlot
setGeneric(name="removePoint", function(self, id) {
	standardGeneric("removePoint")
})

#' @describeIn DynamicPlot Remove 'point' layer
setMethod(f="removePoint", signature="DynamicPlot", function(self, id){
	### Grab the point layer index to remove from the plot
	index = getPointIndex(fig, id)
	self@plot = self@plot %>% ggedit::remove_geom("point", index)

	### Remove the layer id from the point and layer lists
	self@point = self@point[!(self@point == id)]
	self = removeLayer(self, id, "point")
	return(self)
})

#' @rdname Remove-DynamicPlot
setGeneric(name="removePath", function(self, id) {
	standardGeneric("removePath")
})

#' @describeIn DynamicPlot Remove 'path' layer
setMethod(f="removePath", signature="DynamicPlot", function(self, id){
	### Grab the path layer index to remove from the plot
	index = getPathIndex(fig, id)
	self@plot = self@plot %>% ggedit::remove_geom("path", index)

	### Remove the layer id from the path and layer lists
	self@path = self@path[!(self@path == id)]
	self = removeLayer(self, id, "path")
	return(self)
})

############################################################
#' Get the layer id of an index
#' 
#' @note This is used to get the `source' id following a "plotly_click" event.
#'
#' @name getLayer-DynamicPlot
#' @param self DynamicPlot object
#' @param index Index of layer
#' @return The layer id
NULL
############################################################

#' @rdname getLayer-DynamicPlot
setGeneric(name="getLayer", function(self, index) {
	standardGeneric("getLayer")
})

#' @describeIn DynamicPlot Get id for `layer' slot
setMethod(f="getLayer", signature="DynamicPlot", function(self, index) {
	return(self@layer[index+1])
})

############################################################
#' Get the index of a layer id
#'
#' The index is from the `layer', `point', or `path' list of the
#'	associated DynamicPlot slot.
#'
#' @note The `layer' slot appends " point" or " path" to its elements
#'	to differentiate between layer types of the same id (see \code{\link{addLayer}}).
#'
#' @name  getIndex-DynamicPlot
#' @param self DynamicPlot object
#' @param id id of the layer
#' @return Index of the layer id or 'NA'
NULL
############################################################

#' @rdname  getIndex-DynamicPlot
setGeneric(name="getLayerIndex", function(self, id) {
	standardGeneric("getLayerIndex")
})

#' @describeIn  getIndex-DynamicPlot Index of 'layer' slot
setMethod(f="getLayerIndex", signature="DynamicPlot", function(self, id) {
	return(match(id, self@layer))
})

#' @rdname  getIndex-DynamicPlot
setGeneric(name="getPointIndex", function(self, id) {
	standardGeneric("getPointIndex")
})

#' @describeIn  getIndex-DynamicPlot Index of 'point' slot
setMethod(f="getPointIndex", signature="DynamicPlot", function(self, id){
	return(match(id, self@point))
})

#' @rdname  getIndex-DynamicPlot
setGeneric(name="getPathIndex", function(self, id) {
	standardGeneric("getPathIndex")
})

#' @describeIn  getIndex-DynamicPlot Index of 'path' slot
setMethod(f="getPathIndex", signature="DynamicPlot", function(self, id){
	return(match(id, self@path))
})


############################################################
# Test the add/remove tracking of DynamicPlot()
############################################################
# test = DynamicPlot()

# result = function(self) {
# 	cat("Points:", paste(self@point, collapse=", "), "\n")
# 	cat("Paths:", paste(self@path, collapse=", "), "\n")
# 	cat("Layers:", paste(self@layer, collapse=", "), "\n\n")
# }

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

# print(getLayer(test, 0))
# print(getLayer(test, 1))
# print(getLayer(test, 2)) ### Should be 'NA' since there are only 2 elements in the list

# print(getPointIndex(test, "point2"))
# print(getPointIndex(test, "point1")) ### Should be 'NA'

# print(getPathIndex(test, "path2"))
# print(getPathIndex(test, "path1")) ### Should be 'NA'
############################################################


############################################################
# Test the plotting functionality of DynamicPlot()
#	fig@plot$labels$colour = NULL
############################################################
# x = "Rb"
# y = "Mn"
# plot = ggplot(data, mapping=aes_string(x=x, y=y, color="Source.Name"))

# print("Initializing empty ggplot2 plot")
# fig = DynamicPlot()
# fig = initializePlot(fig, plot)
# print(fig@plot)
# result(fig)

# print("Adding 'Alca-1' points")
# df = data %>% filter(Source.Name == "Alca-1")
# layer = geom_point(data %>% filter(Source.Name == "Alca-1"),
# 	mapping=aes_string(x=x, y=y, color="Source.Name"), size=3)
# fig = addPoint(fig, layer, "Alca-1")
# print(fig@plot)
# result(fig)

# print("Adding 'Alca-2' points")
# layer = geom_point(data %>% filter(Source.Name == "Alca-2"),
# 	mapping=aes_string(x=x, y=y, color="Source.Name"), size=3)
# fig = addPoint(fig, layer, "Alca-2")
# print(fig@plot)
# result(fig)

# print("Removing 'Alca-2' points")
# fig = removePoint(fig, "Alca-2")
# print(fig@plot)
# result(fig)

# print("Adding 'Alca-1' path")
# df = data %>% filter(Source.Name == "Alca-1")
# layer = stat_ellipse(data %>% filter(Source.Name == "Alca-1"),
# 	mapping=aes_string(x=x, y=y, color="Source.Name"), size=1)
# fig = addPath(fig, layer, "Alca-1")
# print(fig@plot)
# result(fig)

# print("Adding 'Alca-2' path")
# df = data %>% filter(Source.Name == "Alca-2")
# layer = stat_ellipse(data %>% filter(Source.Name == "Alca-2"),
# 	mapping=aes_string(x=x, y=y, color="Source.Name"), size=1)
# fig = addPath(fig, layer, "Alca-2")
# print(fig@plot)
# result(fig)

# print("Removing 'Alca-1' path")
# fig = removePath(fig, "Alca-1")
# print(fig@plot)
# result(fig)
############################################################