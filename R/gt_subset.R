#' Download GeoNEX Products subsets
#'
#' Download a GeoNEX Products Subset product
#' for a given GeoNEX subset sites.
#'
#' @param product a valid GeoNEX product name
#' @param band band or bands (as a character vector) to download
#' @param start start date
#' @param end end date
#' @param km_lr km left-right to sample (rounded to the nearest integer)
#' @param km_ab km above-below to sample (rounded to the nearest integer)
#' @param site_id site id
#' @param network the network for which to generate the site list,
#' when not provided the complete list is provided
#' @param site_name arbitrary site name used in writing data to file
#' (default = sitename)
#' @param out_dir path where to store the data if writing to disk
#' (default = tempdir())
#' @param internal should the data be returned as an internal data structure
#' \code{TRUE} or \code{FALSE} (default = \code{TRUE})
#' @return A data frame combining meta-data and actual data values.
#' @seealso \code{\link[GeoNEXTools]{gt_sites}}
#' \code{\link[GeoNEXTools]{gt_bands}}
#' \code{\link[GeoNEXTools]{gt_products}}
#' \code{\link[GeoNEXTools]{gt_batch_subset}}
#' @export
#' @importFrom memoise memoise
#' @examples
#'
#' \donttest{
#' # list all available GeoNEX Products Subsets products
#' # download data
#' subset <- gt_subset(product = "geonex_GO16_L1G",
#'                         band = "BAND01",
#'                         start = "2020-01-01",
#'                         end = "2020-03-31",
#'                         site_id = "us_alabama_al_fairhope_3_ne")
#'  head(subset)
#'}


gt_subset <- function(
  product,
  band,
  start = "2016-01-01",
  end = format(Sys.time(),"%Y-%m-%d"),
  km_lr = 0,
  km_ab = 0,
  site_id,
  network,
  site_name = "sitename",
  out_dir = tempdir(),
  internal = TRUE
  ){

  # error trap missing coordinates or site id
  if (missing(site_id) ){
    stop("please specify a valid site ID...")
  }

  # check if site_id is valid
  if(missing(network)){

    # load all sites
    sites <- GeoNEXTools::gt_sites()

    # check if the site id is valid
    if (!(site_id %in% sites$siteid)){
      stop("please specify a valid site id...")
    }

    network <- NULL

  } else {

    # load all sites
    sites <- GeoNEXTools::gt_sites(network = network)

    # check if the site id is valid
    if (!(site_id %in% sites$network_siteid)){
      stop("please specify a valid site id...")
    }
  }

  # load all products
  products <- GeoNEXTools::gt_products()$product

  # error trap product
  if (missing(product) | !(product %in% products) ){
    stop("please specify a product, or check your product name...")
  }

  # load all bands for product
  bands <- gt_bands(product)

  # error trap band
  if (missing(band) | !all(band %in% bands$band) ){
    stop("please specify a band, or check your product band combination ...")
  }

  km_ab=round(km_ab)
  km_lr=round(km_lr)
  if(km_ab<0 | km_lr<0){
    stop("km_ab and km_lr must be positive value ...")
  }
  resolution=bands$reoslution_degrees[which(rbind(bands$band)==band)]
  if (resolution==0.005){
    if(km_ab>8 | km_lr>8){
      stop("km_ab and km_lr must be less than 9 for the band whose resolution is 0.005 ...")
    }
    c_ind=9
    nrow=17
  }else if(resolution==0.01){
    if(km_ab>4 | km_lr>4){
      stop("km_ab and km_lr must be less than 5 for the band whose resolution is 0.01 ...")
    }
    c_ind=5
    nrow=9
  }else if(resolution==0.02){
    if(km_ab>2 | km_lr>2){
      stop("km_ab and km_lr must be less than 3 for the band whose resolution is 0.02 ...")
    }
    c_ind=3
    nrow=5
  }else{
    stop("Unknown resolution. Something wrong")
  }
  extract_pixel=as.vector(t(t(matrix(1:(nrow*nrow),nrow=nrow))[(c_ind-km_ab):(c_ind+km_ab),(c_ind-km_lr):(c_ind+km_lr)]))

  start_date <- as.Date(start)
  end_date <- as.Date(end) +1
  start_year <- as.numeric(format(start_date, "%Y"))
  end_year <- as.numeric(format(end_date, "%Y"))

  # grab site name
  site <- ifelse(missing(site_id), site_name, site_id)

# create a complete list of the data
  complete_data <- lapply(band, function(band_name){


    subset_data <- lapply(as.list(seq(start_year, end_year, 1 )), function(year){

      filename <- paste0(paste(product,
                                band,
                                toString(year),
                                sep = "_"),
  			  ".json")

      if(is.null(network)){

        url <- paste(nex_server(),
                     site_id,
  		               filename,
                     sep = "/")
      } else {

        url <- paste(nex_server(),
                     network,
                     site_id,
  		               filename,
                     sep = "/")
      }


      # try to download the data
      json_chunk <- httr::RETRY(verb = "GET",
                                url = url,
                                httr::write_memory(),
                                terminate_on = c(403, 404))

      # trap errors on download, return a detailed
      # API error statement
      if (httr::http_error(json_chunk)){
        warning(httr::content(json_chunk), call. = FALSE)
        return(NULL)
      }

      # grab content from cached json chunk
      chunk <- jsonlite::fromJSON(httr::content(json_chunk, "text",
                                                encoding = "UTF-8"),
                                  simplifyVector = TRUE)

      # convert to date object for easier handling
      chunk$subset$calendar_date <- as.POSIXct(chunk$subset$calendar_date, "%Y%m%dT%H%M", tz="GMT")

      # subset the dates
      chunk$subset <- chunk$subset[which(chunk$subset$calendar_date <= as.POSIXct(end, "%Y-%m-%d", tz="GMT") + 24*60*60 &
               chunk$subset$calendar_date >= as.POSIXct(start, "%Y-%m-%d", tz="GMT")),]

      # back to the string format for calendar_date
      chunk$subset$calendar_date <- format(chunk$subset$calendar_date, format="%Y%m%dT%H%M")

      return(chunk)

    })



    # split out a header including
    # additional ancillary data
    header <- subset_data[[1]][!(names(subset_data[[1]]) %in%
                                     c("header","subset"))]
    header$site <- site
    header$product <- product
    header$start <- start
    header$end <- end
    header$xllcorner <- header$xllcorner + header$cellsize * ((header$ncols - 1)/2 - km_lr)
    header$yllcorner <- header$yllcorner + header$cellsize * ((header$nrows - 1)/2 - km_ab)
    header$ncols <- 2*km_lr+1
    header$nrows <- 2*km_ab+1
    header$cellsize <- as.character(header$cellsize)


    # This is a check on the complete nature of the retrieved data
    # the process will not stall on errors occur in the download
    # process but just return NULL, these are trapped here and
    # reported as complete TRUE/FALSE in the header or the returned
    # object. Using this flag one can decide to reprocess.
    header$complete <- !any(unlist(lapply(subset_data, is.null)))


    # reshape the data converting it to a tidy data frame
    # data will be reported row wise
    subset_data <- do.call("rbind",
                           lapply(subset_data,
                                  function(x)x$subset))

    pixels <- as.data.frame(do.call("rbind", subset_data$data)[,extract_pixel])
    colnames(pixels) <- seq_len(ncol(pixels))

    # remove old nested list data and substitute with columns
    #subset_data <- cbind(subset_data[,!(names(subset_data) %in% "data")],
    #                          as.data.frame(pixels))
    subset_data <- cbind(subset_data[,"calendar_date"],
                              pixels)
    colnames(subset_data)[1]="calendar_date"

    subset_data <- stats::reshape(subset_data,
                           varying = grep("[0-9]",names(subset_data)),
                           direction = "long",
                           timevar = "pixel",
                           v.names = "value")


    # drop the id column
    subset_data <- subset_data[ , !(names(subset_data) %in% "id")]


    # combine header with the data, this repeats
    # some meta-data but makes file handling easier
    subset_data <- data.frame(header, subset_data,
                              stringsAsFactors = FALSE)


    # return a nested list with all data
    # to workspace or to file
    if (internal){
      return(subset_data)
    } else {

      if(! dir.exists(out_dir)){
        dir.create(out_dir)
      }

      # format filename
      filename <- sprintf("%s/%s_%s_%s_%s%s.csv",
                          path.expand(out_dir),
                          header$site,
                          header$product,
                          header$band,
                          header$start,
                          header$end)
      print(filename)

      # write file to disk
      utils::write.table(subset_data,
                         filename,
                         quote = FALSE,
                         row.names = FALSE,
                         col.names = TRUE,
                         sep = ",")
      print("write done")
    }
  })

 # return a larger tidy dataframe by
 # row binding list elements
 return(do.call("rbind", complete_data))


}
