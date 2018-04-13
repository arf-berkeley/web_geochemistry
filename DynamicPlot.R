library("ggplot2")

DynamicPlot = setClass("DynamicPlot",
	slots = c(
		point="character",
		path="character"
		# npoint="numeric",
		# npath="numeric"
	),
	prototype=list(
	# 	plot=list(),
		point=c(),
		path=c()
		# npoint=0,
		# npath=0
	)
)

# setGeneric(name="init",
# 	def=function(self, plot) {
# 		standardGeneric("init")
# 	}
# )

# setMethod(f="init",
# 	signature="DynamicPlot",
# 	definition=function(self, plot){
# 		# self@order = c(self@order, layerId)
# 		self@plot = plot
# 		return(self)
# 	}
# )

setGeneric(name="addPointLayer",
	def=function(self, id) {
		standardGeneric("addPointLayer")
	}
)

setMethod(f="addPointLayer",
	signature="DynamicPlot",
	definition=function(self, id){
		self@point = c(self@point, id)
		return(self)
	}
)

setGeneric(name="removePointLayer",
	def=function(self, id) {
		standardGeneric("removePointLayer")
	}
)

setMethod(f="removePointLayer",
	signature="DynamicPlot",
	definition=function(self, id){
		# print(paste("From:", length(self@point)))
		self@point = self@point[!(self@point == id)]
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


setGeneric(name="addPathLayer",
	def=function(self, id) {
		standardGeneric("addPathLayer")
	}
)

setMethod(f="addPathLayer",
	signature="DynamicPlot",
	definition=function(self, id){
		self@path = c(self@path, id)
		return(self)
	}
)

setGeneric(name="removePathLayer",
	def=function(self, id) {
		standardGeneric("removePathLayer")
	}
)

setMethod(f="removePathLayer",
	signature="DynamicPlot",
	definition=function(self, id){
		self@path = self@path[!(self@path == id)]
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


# test = DynamicPlot()

# test = addLayer(test, "first", 'path')
# print(test@order)
# # print(paste(test@npath, test@npoint))

# test = addLayer(test, "second", 'path')
# print(test@order)

# test = addLayer(test, "third", 'path')
# print(test@order)

# # test = removeLayer(test, "second", 'path')
# # print(test@order)

# test = addLayer(test, "fourth", 'point')
# print(test@order)

# print(paste(test@npath, test@npoint))

# if (test@npoint > 0) {
# 	print("Yes!")
# 	print(test@npoint + 1)
# } else {
# 	print("No...")
# }

# test = removeLayer(test, "first", 'path')
# print(test@order)

# print(getLayerIndex(test, "fourth", 'point'))