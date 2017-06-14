#' @import leaflet
#' @export
plot_leaflet <- function(x, ...) {
  UseMethod('plot_leaflet')
}


plot_leaflet.site <- function(x, providerTiles = "Stamen.TerrainBackground", ...) {
  map <- leaflet(data = x)
  map <- addProviderTiles(map, providerTiles)
  map <- addCircleMarkers(map, lng = long, lat = lat)
    
  map
}

#' @export
plot_leaflet.dataset <- function(x, ...) {
    site <- get_site(x)
    plot_leaflet(site, ...)
}

#' @export
plot_leaflet.dataset_list <- function(x, ...) {
  site <- get_site(x)
  
  plot_leaflet(site)
}

#' @export
plot_leaflet.download <- function(x, ...) {
  site <- get_site(x)
  plot_leaflet(site, ...)
}

#' @export
plot_leaflet.download_list <- function(x, ...) {
  dataset <- get_dataset(x)

  plot_leaflet(dataset, ...)
  
}
