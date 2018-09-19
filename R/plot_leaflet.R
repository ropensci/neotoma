#' @title Leaflet plots for neotoma data.
#' @description A plotting function to provide interactive data investigation using the leaflet tools.
#'   This package requires a connection to the internet for proper functioning.
#' @param x A neotoma data object
#' @param providerTiles Default "Stamen.TerrainBackground", a character string indicating the tile background to be used for plotting.
#' @param ... Other terms to be passed to the function.
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers markerOptions markerClusterOptions
#' @return A \code{leaflet} object
#' @export
plot_leaflet <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
  UseMethod('plot_leaflet')
}

plot_leaflet.site <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
  map <- leaflet(data = x)
  map <- addProviderTiles(map, providerTiles)
  map <- addCircleMarkers(map, 
                          lng = x$long, lat = x$lat, 
                          popup = paste0('<b>', x$site.name, '</b><br><b>Description:</b> ', x$description,
                                         '<br><a href=http://apps.neotomadb.org/explorer/?siteids=',x$site.id,'>Explorer Link</a>'),
                          clusterOptions = markerClusterOptions(),
                          options = markerOptions(riseOnHover = TRUE))
    
  map
}

#' @export
plot_leaflet.dataset <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
    site <- get_site(x)
    plot_leaflet(site, ...)
}

#' @export
plot_leaflet.dataset_list <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
  site <- get_site(x)
  
  plot_leaflet(site)
}

#' @export
plot_leaflet.download <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
  site <- get_site(x)
  plot_leaflet(site, ...)
}

#' @export
plot_leaflet.download_list <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
  dataset <- get_dataset(x)

  plot_leaflet(dataset, ...)
  
}
