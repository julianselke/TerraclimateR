#' Extract Climate Data from a NetCDF File for a Single Coordinate Pair
#'
#' Load TerraClimate data a climatic variable and extract information of the closest data
#' point(s) to the specified coordinates.
#'
#' @param lat numeric. The latitude value.
#' @param lon numeric. The longitude value.
#' @param clim_var character. One of 'aet', 'def', 'pet',
#' 'ppt', 'q', 'soil', 'srad', 'swe', 'tmax', 'tmin', 'vap', 'ws', 'vpd', 'PDSI'
#' @param nc An object of class NetCDF including the \emph{clim_var} variable.
#' Will be accessed online if none provided.
#' @param lon_nc FOR INTERNAL USE ONLY - one dimensional array. The longitude values to retrieve data from.
#' @param lat_nc FOR INTERNAL USE ONLY - one dimensional array. The latitude values to retrieve data from.
#' @param year numeric. If specified data are retrieved for this year only.
#' @details If a coordinate has two or more equally distant data points in the
#'  NetCDF file, the mean value of these 'nearest neighbors' is calculated.
#' @return A vector of length (<year of latest records> - 1958 ) * 12. Values start at
#' January 1958.
#'
#' @import RNetCDF
#'
#' @examples
#' # ambiguous match for longitude
#' get_terraclim_single(12, 22, "def")
#' # no ambiguous match
#' get_terraclim_single(12.12345678, 22.12345678, "def")
#'
get_terraclim_single <- function(lat,
                                 lon,
                                 clim_var,
                                 nc = NULL,
                                 lon_nc = NULL,
                                 lat_nc = NULL,
                                 year = NULL) {
  if (is.na(lat) || is.na(lon)) {
    warning("latitude and/or longitude missing, returning NA")
    return(NA)
  }
  if (!(is.numeric(lat) && is.numeric(lon))) {
    stop("latitude and longitude must be numeric")
  }
  if (!is.character(clim_var)) stop("clim_var must to be of mode character")
  # open data for specified variable
  if (is.null(nc)){
    # close connection when function finishes
    on.exit(RNetCDF::close.nc(nc))
    baseurlagg <- paste0("http://thredds.northwestknowledge.net:8080/",
                         "thredds/dodsC/agg_terraclimate_",
                         clim_var,
                         "_1958_CurrentYear_GLOBE.nc")
    nc <- RNetCDF::open.nc(baseurlagg)
  }
  # extract data point coordinates
  if (is.null(lon_nc)) lon_nc <- RNetCDF::var.get.nc(nc, "lon")
  if (is.null(lat_nc)) lat_nc <- RNetCDF::var.get.nc(nc, "lat")
  # determine nearest data point to provided coordinates
  # check if there are multiple equally distant data points
  ALERT.lat <- ALERT.lon <- NULL
  # minimal distance to any data point
  min_lon <-  min(abs(lon_nc - lon))
  # index of nearest data point(s)
  lons <- which(abs(lon_nc - lon) == min_lon)
  # if more than one index store coordinate
  if (length(lons) > 1) ALERT.lon <- lon
  # same for latitude
  min_lat <- min(abs(lat_nc - lat))
  lats <- which(abs(lat_nc - lat) == min_lat)
  if (length(lats) > 1) ALERT.lat <- lat
  # get all latitude x longitude combinations of equally distant data points
  indices <- expand.grid(lats, lons)
  # if latitude or longitude or both have equally distant data points print
  # a message specifying which coordinate was 'ambiguous'
  if (nrow(indices) > 1) {
    warning(paste("ambiguous matches for coordinates\n",
                  "longituge :", ALERT.lon, "\n",
                  "latitude  :", ALERT.lat, "\n",
                  "calculating mean of equally distant data points"))
  }
  data <- list()
  # iterate over equally distant data point(s)
  for (i in 1:nrow(indices)) {
    data[[i]] <- RNetCDF::var.get.nc(nc,
                                     variable = clim_var,
                                     start = c(indices[i, 2], indices[i, 1], 1),
                                     count = c(1, 1, NA),
                                     unpack = TRUE)
  }
  # calculate mean value of equally distant data points
  # no effect if only one data point ( mean(x) = x )
  res <- rowMeans(matrix(unlist(data), ncol = nrow(indices)), na.rm = TRUE)
  # add attributes if returning to user
  if (sys.nframe() == 1) {
    if (is.null(year)) {
      res <- setNames(res,
                      paste(month.abb[rep(1:12, length(res)/12)],
                            rep(1958:(1958 + length(res)/12 - 1), each = 12)
                      )
      )
    } else {
      res <- setNames(res, paste(month.abb, year))
    }
    attr(res, "longitude") <- lon
    attr(res, "latitude") <- lat
    attr(res, "climate_variable") <- clim_var
    attr(res, "timestamp") <- Sys.time()
  }
  return(res)
}

