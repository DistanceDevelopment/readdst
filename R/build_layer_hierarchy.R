#' Work out the layer hierarchy in the Distance database
#'
#' Use the DataLayers table to work out the hierarchy of the tables and layers in the database held by Distance.
#'
#' @author David L Miller
#' @importFrom Hmisc mdb.get
build_layer_hierarchy <- function(data_file){

  data_layers <- Hmisc::mdb.get(data_file, "DataLayers")
  data_layers$LayerName <- as.character(data_layers$LayerName)
  data_layers$ParentLayerName <- as.character(data_layers$ParentLayerName)

  # construct the layers in order left to right
  layers <- data_layers$LayerName[data_layers$LayerType==1]
  data_layers <- data_layers[data_layers$LayerType!=1,,drop=FALSE]

  last_layer <- layers

  # keep removing layers until we're done
  while(nrow(data_layers)>0){
    layers <- c(layers, data_layers$LayerName[data_layers$ParentLayerName ==
                                              last_layer])
    data_layers <- data_layers[data_layers$ParentLayerName !=
                                         last_layer,,drop=FALSE]

    # add layer to the vector
    last_layer <- layers[length(layers)]
  }

  return(layers)
}
