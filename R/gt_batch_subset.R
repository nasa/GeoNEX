#' Batch download GeoNEX Products subsets
#'
#' Download a GeoNEX Products Subset product
#' for given multiple GeoNEX subset sites.
#'
#' @param df a CSV file or data frame holding site IDs
#' When providing a CSV make sure that the
#' data are comma separated.
#' @param product a valid GeoNEX product name
#' @param band band to download
#' @param start start date
#' @param end end date
#' @param km_lr km left-right to sample
#' @param km_ab km above-below to sample
#' @param out_dir location where to store all data
#' @param internal should the data be returned as an internal data structure
#' \code{TRUE} or \code{FALSE} (default = \code{TRUE})
#' @return A data frame combining meta-data and actual data values, data from
#' different sites is concatenated into one large dataframe. Subsets can be
#' created by searching on sitename.
#' @seealso \code{\link[GeoNEXTools]{gt_sites}}
#' \code{\link[GeoNEXTools]{gt_bands}}
#' \code{\link[GeoNEXTools]{gt_products}}
#' \code{\link[GeoNEXTools]{gt_subset}}
#' @export
#' @examples
#'
#' \dontrun{
#' # create data frame with a site_id column
#'df <- data.frame("site_id" = c("us_idaho_twrs",
#'                               "us_illinois_bondville"))
#'
#' print(df)
#'
#' # test batch download
#' subsets <- gt_batch_subset(df = df,
#'                         product = "geonex_GO16_L1G",
#'                         band = "BAND01",
#'                         internal = TRUE,
#'                         start = "2004-01-01",
#'                         end = "2004-03-31")
#'
#' # the same can be done using a CSV file with
#' # a data structure similar to the dataframe above
#'
#' write.table(df, file.path("my_sites.csv"),
#'  quote = FALSE,
#'  row.names = FALSE,
#'  col.names = TRUE,
#'  sep = ",")
#'
#' # test batch download form CSV
#' subsets <- gt_batch_subset(df = file.path("my_sites.csv"),
#'                         product = "geonex_GO16_L1G",
#'                         band = "BAND01",
#'                         internal = TRUE,
#'                         start = "2004-01-01",
#'                         end = "2004-03-31"
#'                         )
#'
#' head(subsets)
#'}

gt_batch_subset <- function(
  df,
  product,
  band,
  start = "2016-01-01",
  end = format(Sys.time(),"%Y-%m-%d"),
  km_lr = 0,
  km_ab = 0,
  out_dir = tempdir(),
  internal = TRUE
  ){

  # error trap
  if (missing(df)){
    stop("please specify a batch file...")
  }

  # check data frame
  if (!is.data.frame(df)){
    if(file.exists(df)){
      df <- utils::read.table(df,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)
    } else {
      stop("specified batch file does not exist")
    }
  }

  # load products
  products <- GeoNEXTools::gt_products()$product

  # error trap products
  if (missing(product) | !(product %in% products) ){
    stop("please specify a product, or check your product name...")
  }

  # load bands for product
  bands <- gt_bands(product)

  # error trap band
  if (missing(band) | !(band %in% bands$band) ){
    stop("please specify a band, or check your product band combination ...")
  }

  # If all tests pass construct the data frame over which we will
  # loop to process all the data
  df$product <- product
  df$band <- band
  df$start <- start
  df$end <- end
  df$km_lr <- km_lr
  df$km_ab <- km_ab
  df$out_dir <- path.expand(out_dir)
  df$internal <- internal

  # convert names tolower case
  # trapping naming issues of coordinates
  # and sites
  names(df) <- tolower(names(df))

  # paralllel loop (if requested)
  output <- apply(df, 1, function(x){
    GeoNEXTools::gt_subset(
      site_id = as.character(x['site_id']),
      product = as.character(x['product']),
      band = as.character(x['band']),
      km_lr = as.numeric(x['km_lr']),
      km_ab = as.numeric(x['km_ab']),
      start = as.character(x['start']),
      end = as.character(x['end']),
      out_dir = x['out_dir'],
      internal = x['internal'])
  })

  # return data
  if(internal){
    # row bind the nested list output
    output <- do.call("rbind", output)

    return(output)
  } else {
    invisible()
  }
}

