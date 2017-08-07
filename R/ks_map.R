#' Quickly-Made County-Level Choropleth Map for the State of Kansas
#'
#' This function requires a certain data structure: A variable needs to be labeled “fips” (with five-digit county FIPS codes) or labeled “county” (with county names, in all lowercase); additionally, it requires some numeric vector whose values you will use to fill in the choropleth map.
#' 
#' @param data The data frame containing the variables you want to map
#' @param key Either “fips” or “county.” The function will throw an error if the key is not set to one of these two. It should be specified as a character string.
#' @param variable The variable you want to use as the fill for each county. This should be supplied as a character string.
#' @param label The legend title you wish to have; the label for your variable. This defaults to being blank.
#' @param lowcolor The color you wish to represent the minimum value of variable. This takes a hex value (or a string known by R) as a character string. It defaults to the ggplot2 default.
#' @param highcolor The color you wish to represent the maximum value of variable. This takes a hex value (or a string known by R) as a character string. It defaults to the ggplot2 default.
#' @param title The title that will go above your map. This defaults to being blank.
#' @import dplyr
#' @import maps
#' @export
ks_map <- function(data, key, variable, label="", lowcolor="#56B1F7", highcolor="#132B43", title="") {
  
  # throwing errors if data structure is not correct
  if (sum(names(data) %in% c("fips", "county"))==0) {
    stop("Your data must include a variable named either \"fips\"  or \"county\", see function details.")
  }
  
  if (is.numeric(data[,variable])==FALSE) {
    stop("The variable you specify must be of numeric class.")
  }
  
  # get mapping data
  mapdata <- ggplot2::map_data("county", "kansas")
  
  # combine with fips codes
  data("county.fips", envir=environment())
  mapdata <- county.fips %>%
    tidyr::separate(polyname, c("region", "subregion"), "[,]") %>% 
    filter(region=="kansas") %>% 
    select(-region) %>% 
    full_join(mapdata, by="subregion") %>% 
    rename(county=subregion) %>% 
    select(-region)
  
  if (key=="fips") {
    mapdata <- tidyr::left_join(mapdata, data, by="fips")
    plot <- ggplot2::ggplot(mapdata, ggplot::aes(x=long, y=lat, group=group, fill=get(variable))) +
      ggplot2::geom_polygon() +
      ggplot2::coord_map() +
      ggplot2::theme_void() +
      ggplot2::scale_fill_gradient(name=label, low=lowcolor, high=highcolor) + 
      ggplot2::labs(title=title)
    return(plot)
  } else if (key=="county") {
    # in case they forget to lowercase county names
    data[,"county"] <- tolower(data[,"county"])
    mapdata <- left_join(mapdata, data, by="county")
    plot <- ggplot2::ggplot(mapdata, ggplot2::aes(x=long, y=lat, group=group, fill=get(variable))) +
      ggplot2::geom_polygon() +
      ggplot2::coord_map() +
      ggplot2::theme_void() +
      ggplot2::scale_fill_gradient(name=label, low=lowcolor, high=highcolor) +
      ggplot2::labs(title=title)
    return(plot)
  } else {
    stop("Please specify key as either \"county\" or \"fips\"")
  }
}
