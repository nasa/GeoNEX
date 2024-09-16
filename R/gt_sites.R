#' Download all available fixed sites
#'
#' Lists all available GeoNEX Products Subset pre-processed sites
#'
#' @param network the network for which to generate the site list,
#' when not provided the complete list is provided
#' @return A data frame of all available GeoNEX Products Subsets
#' pre-processed sites
#' @seealso \code{\link[GeoNEXTools]{gt_products}}
#' \code{\link[GeoNEXTools]{gt_bands}}
#' @export
#' @examples
#'
#' \donttest{
#' # list all available GeoNEX Products Subsets products
#' sites <- gt_sites()
#' print(head(sites))
#'}
#'

gt_sites <- memoise::memoise(function(
  network
){

  # define server settings
  if(missing(network)){
   url <- paste(nex_server(), "JSON", "sites", sep = "/")
  } else{
    url <- paste(nex_server(), "JSON", network ,"sites", sep = "/")
  }

  # try to download the data
  sites <- try(jsonlite::fromJSON(url))

  # trap errors on download, return a general error statement
  if (inherits(sites, "try-error")){
    stop("Your requested timed out or the server is unreachable")
  }

  # return a data frame with all products and their details
  return(sites$sites)
})
