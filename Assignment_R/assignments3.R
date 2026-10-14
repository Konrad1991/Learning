# Replacement functions with S4 classes
# =================================================
setClass(
  "Foo",
  slots = list(
    width = "integer",
    height = "integer"
  )
)
instance <- new("Foo", width = 1L, height = 2L)
instance@width
instance@height

# Getter
# =================================================
setGeneric("width", function(object) standardGeneric("width"))
setMethod("width", "Foo", function(object) object@width)

setGeneric("height", function(object) standardGeneric("height"))
setMethod("height", "Foo", function(object) object@height)

width(instance)
height(instance)

# Setter (replacement functions)
# =================================================
setGeneric("width<-", function(object, value) standardGeneric("width<-"))
setReplaceMethod("width", "Foo", function(object, value) {
  object@width <- value
  return(object) # Mandatory!!!
})

setGeneric("height<-", function(object, value) standardGeneric("height<-"))
setReplaceMethod("height", "Foo", function(object, value) {
  object@height <- value
  return(object) # Mandatory!!!
})

width(instance) <- 15L
height(instance) <- 30L
instance

# Same rewrite as for S3:
instance <- `width<-`(instance, value = 15L)

# Slots are type checked
# =================================================
# width(instance) <- 1 # Error: class "numeric" is not valid for @'width'
width(instance) <- 1L # Works
