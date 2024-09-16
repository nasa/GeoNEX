#' Download all available products
#'
#' Lists all available GeoNEX Products Subset products.
#'
#' @return A data frame of all available GeoNEX Products Subsets products
#' @seealso \code{\link[GeoNEXTools]{gt_bands}}
#' \code{\link[GeoNEXTools]{gt_sites}}
#' @export
#' @importFrom memoise memoise
#' @examples
#'
#' \donttest{
#' # list all available GeoNEX Products Subsets products
#' products <- gt_products()
#' head(products)
#'}
#'


gt_products <- memoise::memoise(function(){

  # define url
  url <- paste(nex_server(), "JSON", "products", sep = "/")

  # try to download the data
  products <- try(jsonlite::fromJSON(url))

  # trap errors on download, return a general error statement
  if (inherits(products, "try-error")){
    stop("Your requested timed out or the server is unreachable")
  }

  # split out data
  products <- products$products

  # return a data frame with all products and their details
  return(products)
})
