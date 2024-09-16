#' Download all available bands
#'
#' Lists all available bands for a GeoNEX Products Subset product.
#'
#' @param product a valid GeoNEX product name
#' @return A data frame of all available bands for a GeoNEX
#' Products Subsets products
#' @seealso \code{\link[GeoNEXTools]{gt_products}}
#' \code{\link[GeoNEXTools]{gt_sites}}
#' @export
#' @importFrom memoise memoise
#' @examples
#'
#' \donttest{
#' # list all available GeoNEX  Products Subsets products
#' bands <- gt_bands(product = "geonex_GO16_L1G")
#' head(bands)
#'
#'}
#'

gt_bands <- memoise::memoise(function(product){

  # load all products
  products <- GeoNEXTools::gt_products()$product

  # error trap
  if (missing(product) | !(product %in% products)){
    stop("please specify a product, or check your product name...")
  }

  # define url
  url <- paste(nex_server(), "JSON", product, "bands", sep = "/")

  # try to download the data
  bands <- try(jsonlite::fromJSON(url))

  # trap errors on download, return a general error statement
  if (inherits(bands, "try-error")){
    stop("Your requested timed out or the server is unreachable")
  }

  # return a data frame with all bands
  return(bands$bands)
})
