#' Download TerraClimate Data
#'
#'@description
#' Download data from the TerraClimate data set based on provided coordinates. One or more climatic
#' variables can be specified. Data can be retrieved for all available years or, optionally, for a
#' specific year. Climatic data are returned in long format which ensures seamless integration
#' into a \emph{tidy} workflow.
#'
#' @details
#' Coordinates are not checked for validity, i.e., if they are located over bodies of land and fall
#' in the range of -180:180 for longitude and -90:90 for latitude.
#'
#' The TerraClimate data set is extended on a yearly basis, which lead to a varying length of the
#' returned data.frame. The expected length can be calculated by:
#' \eqn{(year_{current} - 1958) * 12 * length_{clim\_var} * nrow_{df}}
#'
#' When working with larger data sets it is advisable to set conserve = FALSE to reduce memory
#' footprint. Retrieving all 14 TerraClimate variables (in 2023) for a data.frame with 200
#' observations results in a data.frame with 2,217,600 rows.
#'
#' Available TerraClimate variables:
#'
#' | **Code** | **Variable**                         | **Detail**                   | **Unit**    |
#' |:---------|:-------------------------------------|:-----------------------------|:------------|
#' | **aet**  | Actual Evapotranspiration            | monthly total                | \eqn{mm}    |
#' | **def**  | Climate Water Deficit                | monthly total                | \eqn{mm}    |
#' | **pet**  | Potential evapotranspiration         | monthly total                | \eqn{mm}    |
#' | **ppt**  | Precipitation                        | monthly total                | \eqn{mm}    |
#' | **q**    | Runoff                               | monthly total                | \eqn{mm}    |
#' | **soil** | Soil Moisture                        | total column at end of month | \eqn{mm}    |
#' | **srad** | Downward surface shortwave radiation |                              | \eqn{W/m^2} |
#' | **swe**  | Snow water equivalent                | at end of month              | \eqn{mm}    |
#' | **tmax** | Max Temperature                      | average for month            | \eqn{°C}    |
#' | **tmin** | Min Temperature                      | average for month            | \eqn{°C}    |
#' | **vap**  | Vapor pressure                       | average for month            | \eqn{kPa}   |
#' | **ws**   | Wind speed                           | average for month            | \eqn{m/s}   |
#' | **vpd**  | Vapor Pressure Deficit               | average for month            | \eqn{kPa}   |
#' | **PDSI** | Palmer Drought Severity Index        | at end of month              | unitless    |
#' @md
#'
#' @param df An object of class data.frame or matrix containing a column with observation
#' identifiers and two columns with corresponding coordinates, i.e., longitude and latitude.
#' @param id_var A character of length one or the unquoted name of the column storing observation
#' identifiers, e.g. "sample_id". Identifiers can be missing (`NA`). They will be set
#' to \emph{<NA><dot><row number>}.
#' @param lon_var A character of length one or the unquoted name of the column storing latitude
#' values, e.g. "Longitude".
#' @param lat_var A character of length one or the unquoted name of the column storing latitude
#' values, e.g. "LAT".
#' @param clim_vars A character vector specifying the terraclimate variables to download.
#' Any combination of "aet", "def", "pet", "ppt", "q", "soil", "srad", "swe", "tmax", "tmin",
#' "vap", "ws", "vpd", "PDSI", or "ALL" to load all of them. See detail section for more information
#' on the variables. Default is "ALL".
#' @param year A numeric value specifying the year for which data should be downloaded, e.g. 1984.
#' @param show_prog A logical value indicating whether to print progress bar. Default is TRUE.
#' Accuracy depends on the stability of the transmission rate.
#' @param conserve A logical value indicating whether to include columns other than `id_var` of the
#' original data frame provided in df. Default is FALSE. See details for recommendations when to
#' use.
#'
#' @return tidy data.frame
#' @export
#' @import RNetCDF
#' @examples
#'df <- data.frame(id = c("Vilcabamba", "Paracas", "Puerto Viejo", "Trinidad", "Cartagena"),
#'                 lat = c(-4.260641, -13.833881, 9.656562, 21.808230, 10.172151),
#'                 lon = c(-79.22274, -76.25046, -82.75600, -79.98102, -75.74658))
#'
#'x <- get_terraclim(df = df,
#'                   id_var = id,
#'                   lon_var = lon,
#'                   lat_var = lat,
#'                   clim_vars = c("tmin", "ppt"),
#'                   show_proc = TRUE,
#'                   conserve = TRUE)
#'
#'str(x)
#'
#'# calculate expected length
#' clim_vars <- c("tmin", "ppt")
#' (as.numeric(format(Sys.Date(), "%Y")) - 1958) * 12 * length(clim_vars) * nrow(df)
#'
get_terraclim <- function(df,
                          id_var,
                          lat_var,
                          lon_var,
                          clim_vars = c("ALL"),
                          year = NULL,
                          show_prog = TRUE,
                          conserve = FALSE) {
  # make both quoted and unquoted names characters
  id_var <- gsub("\\\"", "", deparse(substitute(id_var)))
  lon_var <- gsub("\\\"", "", deparse(substitute(lon_var)))
  lat_var <- gsub("\\\"", "", deparse(substitute(lat_var)))
  # available terraclimate NetCDFs
  terraclim_vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad",
                      "swe", "tmax", "tmin", "vap", "ws", "vpd", "PDSI")
  # match selection
  var_selection <- unlist(ifelse(all(clim_vars == "ALL"),
                          list(terraclim_vars),
                          list(terraclim_vars[terraclim_vars %in% clim_vars])))
  # no match
  if(length(var_selection) == 0) stop("no matching clim_vars provided")
  # invalid type
  if (!is.character(clim_vars)) {
    stop("clim_var must be of type character")
  }
  # invalid class
  if (!(inherits(df, "data.frame") | inherits(df, "matrix"))) {
    stop(sprintf("invalid class of df: %s\nobject of class data.frame or matrix needed",
                 paste(class(df), collapse = ", ")))
  }
  # is either data.frame or matrix or tibble, can be coerced
  df <- data.frame(df)
  # mismatch
  if (all(clim_vars != "ALL") && any(!clim_vars %in% terraclim_vars)) {
    stop(sprintf("invalid clim_vars provided: %s",
                 paste(clim_vars[!clim_vars %in% terraclim_vars], collapse = ", ")))
  }
  # column names of provided data.frame
  vars <- c(id_var, lon_var, lat_var)
  # invalid column name
  if (any(!vars %in% colnames(df))) {
    stop(sprintf("column(s) %s not found", paste(vars[!vars %in% colnames(df)], collapse = ", ")))
  }
  if (!is.null(year)) {
    if (!is.numeric(year)) stop("year must be numeric")
    if (!is.integer(year)) year <- as.integer(year)
    if (year < 1958) stop(sprintf("data records start in 1958 - no data available for year %s",
                                  year))
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    if (year > current_year) stop(sprintf("no data available for the future (year %s)", year))
  }
  # close connection when function finishes
  on.exit(if (isTRUE(failed_connection)) NULL else RNetCDF::close.nc(nc))
  failed_connection <- FALSE

  if (is.null(year)) {
    baseurlagg <- paste0("http://thredds.northwestknowledge.net:8080/",
                         "thredds/dodsC/agg_terraclimate_",
                         var_selection[1],
                         "_1958_CurrentYear_GLOBE.nc")
    nc <- RNetCDF::open.nc(baseurlagg)
  } else {
    tryCatch(
      expr = {
        # initialise sentinel
        # open data for first (or only) specified variable to get length
        baseurlagg <- paste0("http://thredds.northwestknowledge.net:8080/",
                             "/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_",
                             var_selection[1], "_" , year, ".nc")
        nc <- RNetCDF::open.nc(baseurlagg)
      },
      error = function(e) {
        # update sentinel
        failed_connection <<- TRUE
        stop(paste(e, ifelse(year - current_year > -1,
                                           "data might be too recent",
                                           NULL)))
      }
      )
  }
  # make unique names for missing ids
  # if conserve = TRUE unique ids are needed for unambiguous merging
  for (i in seq_len(nrow(df))) if (is.na(df[[id_var]][i])) df[[id_var]][i] <- paste0("NA.", i)
  df_orig <- df
  df <- df[complete.cases(df[, c(lat_var, lon_var)]), ]

  # lengths for return object construction and indexing
  if (is.null(year)) {
    nc_length <- RNetCDF::dim.inq.nc(nc, "time")$length
  } else {
    nc_length <- 12
  }
  df_length <- nrow(df)
  vs_length <- length(var_selection)
  # year and month ranges per sample and variable
  if (is.null(year)) {
    res_years  <- rep(1958:(1958 + nc_length/12 - 1), each = 12)
  } else {
    res_years <- year
  }
  res_months <- factor(month.abb[rep(1:12, nc_length/12)], levels = month.abb)

  # populate return object
  res <- data.frame(
    'sample'   = rep(rep(df[, id_var], each = nc_length), vs_length),
    'year'     = rep(res_years, df_length * vs_length),
    'month'    = rep(res_months, df_length * vs_length),
    'variable' = rep(factor(var_selection), each = df_length * nc_length),
    'value'    = NA
  )
  names(res)[names(res) == 'sample'] <- id_var

  # extract data point coordinates
  lon_nc <- RNetCDF::var.get.nc(nc, "lon")
  lat_nc <- RNetCDF::var.get.nc(nc, "lat")
  # iterate over terraclimate variables to extract
  for(i in seq_len(length(var_selection))) {
    clim_var <- var_selection[i]
    if (i > 1) {
      # close old connection (last connection is closed with on.exit)
      RNetCDF::close.nc(nc)
      if (is.null(year)) {
        baseurlagg <- paste0("http://thredds.northwestknowledge.net:8080/",
                             "thredds/dodsC/agg_terraclimate_",
                             var_selection[i],
                             "_1958_CurrentYear_GLOBE.nc")
        nc <- RNetCDF::open.nc(baseurlagg)
      } else {
        baseurlagg <- paste0("http://thredds.northwestknowledge.net:8080/",
                             "/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_",
                             var_selection[i], "_" , year, ".nc")
        nc <- RNetCDF::open.nc(baseurlagg)
      }
    }
    # iterate over coordinate pairs in data frame
    for(j in seq_len(nrow(df))) {
      # optionally show progress bar
      if (interactive() && isTRUE(show_prog)) {
        TerraclimateR:::show_progress(i = df_length * (i - 1) + j,
                                      n = vs_length * df_length,
                                      names = as.vector(outer(var_selection,
                                                              df[[id_var]],
                                                              paste, sep = " - ")),
                                      time = TRUE)
      }

      # row indices to populate in this iteration
      index_range <- seq(
        ((df_length * (i - 1) + j) - 1) * nc_length + 1,
        nc_length * (df_length * (i - 1) + j)
      )
      # populate return object with data
      res[index_range, "value"] <- get_terraclim_single(lat      = df[j, lat_var],
                                                        lon      = df[j, lon_var],
                                                        clim_var = clim_var,
                                                        lat_nc   = lat_nc,
                                                        lon_nc   = lon_nc,
                                                        nc       = nc,
                                                        year     = year)
    }
  }
  if (isTRUE(conserve)) res <- merge(res, df_orig, by = id_var, all.y = TRUE)
  return(res)
}
