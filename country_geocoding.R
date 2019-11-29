library(ggmap)
library(purrr)
library(dplyr)

# Most of this code is an evil copy/paste from ggmap's geocode.
# All credits to David Kahle.
# https://github.com/dkahle/ggmap/blob/37a86724bcf651ec0ef4be72db46d3ac2a03963a/R/geocode.R#L100

# See ggmap instructions to obtain a google credentials
# register_google(key = "[ your api key ]")

country_geocoding <- function(location) {
  if (length(location) > 1) {
    out <- location %>%
      map(~ country_geocoding(.x)) %>%
      map(~ as_tibble(as.list(.x))) %>%
      bind_rows()

    return(out)
  }

  gc <- geocode(location, output = "all")
  
  NULLtoNA <- function (x) {
    if (is.null(x)) return(NA) else x
  }
  
  gcdf <- with(gc$results[[1]], {
    tibble(
      "lon" = NULLtoNA(geometry$location$lng),
      "lat" = NULLtoNA(geometry$location$lat),
      "type" = tolower(NULLtoNA(types[1])),
      "loctype" = tolower(NULLtoNA(geometry$location_type)),
      "address" = tolower(NULLtoNA(gc$results[[1]]$formatted_address)),
      "north" = NULLtoNA(geometry$viewport$northeast$lat),
      "south" = NULLtoNA(geometry$viewport$southwest$lat),
      "east" = NULLtoNA(geometry$viewport$northeast$lng),
      "west" = NULLtoNA(geometry$viewport$southwest$lng)
    )
  })
  
  name_to_grab  <- "long_name"
  output_values <- vapply(gc$results[[1]]$address_components, function (x) x[[name_to_grab]], character(1))
  output_names <- vapply(gc$results[[1]]$address_components, function (x) {
    if (length(x$types) == 0) return("query")
    unlist(x$types)[1]
  },
  character(1)
  )
  gcdf_more <- as_tibble(as.list(output_values), .name_repair = "unique")
  names(gcdf_more) <- output_names
  
  place <- tibble(gcdf, gcdf_more)
  tibble(lat=place$gcdf$lat, lon=place$gcdf$lon, country=place$gcdf_more$country)
}
