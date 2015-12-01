#' Work out the layer hierarchy in the Distance database
#'
#' Use the DataLayers table to work out the hierarchy of the tables and layers in the database held by Distance.
#'
#' @param data_file a data file to load the database from
#' @author David L Miller
build_layer_hierarchy <- function(data_file){

  data_layers <- db_get(data_file, "DataLayers")
  data_layers$LayerName <- as.character(data_layers$LayerName)
  data_layers$ParentLayerName <- as.character(data_layers$ParentLayerName)
  data_layers$LayerType <- as.numeric(data_layers$LayerType)

  # construct the layers in order left to right
  layers <- data_layers$LayerName[data_layers$LayerType==1]
  layers_names <- 1
  data_layers <- data_layers[data_layers$LayerType!=1,,drop=FALSE]

  last_layer <- layers

  # keep removing layers until we're done
  while(nrow(data_layers)>0){
    layers <- c(layers, data_layers$LayerName[data_layers$ParentLayerName ==
                                              last_layer])
    layers_names <- c(layers_names, data_layers$LayerType[data_layers$ParentLayerName ==
                                              last_layer])
    data_layers <- data_layers[data_layers$ParentLayerName !=
                                         last_layer,,drop=FALSE]

    # add layer to the vector
    last_layer <- layers[length(layers)]
  }

  names(layers) <- layers_names
  return(layers)
}
