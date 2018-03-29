#' Title
#'
#' @param shapefile_folder
#' @param gages_filename
#' @param flow_ids_filename
#' @param valid_artificial_segments_filename
#'
#' @return
#' @export
#'
#' @examples
loadData <- function(shapefile_folder=NULL,
                     gages_filename=NULL,
                     flow_ids_filename=NULL,
                     valid_artificial_segments_filename=NULL) {

  # determine if the user specified custom input files to use, in which case
  # the perprocessData function will be used to load and process that data
  if (is.null(shapefile_folder)){
    # if they didn't pass in a layer name then
    # they want to use the internal default dataframe
    segment_data_original <- default_segment_data_original
  } else{
    if (!file.exists(shapefile_folder)){
      stop(sprintf('Invalid shapefile folder provided: %s', shapefile_folder))
    }
    # determine the layer name by reading the filenames in the shapefile folder.
    # All the files in that folder should have the same file title, just different
    # file extensions, and that file title is the layer name expected by readOGR
    shape_filenames <- list.files(shapefile_folder, include.dirs = F, recursive=T)
    if (length(shape_filenames)==0){
      stop(sprintf('No files in the shapefile folder provided: %s', shapefile_folder))
    }
    layer_name <- tools::file_path_sans_ext(shape_filenames[1])

    # read in the shape file which contains all the data about the water segments
    segment_data_original <- rgdal::readOGR(dsn = shapefile_folder, layer = layer_name)

    # only keep the fields in the segment_data that will be used in the analysis
    # And only keep the dataframe, getting rid of the lines and other data in the layer
    # since it's not used here, and just takes up memory
    segment_fields <- c('COMID', 'TotDASqKM', 'FTYPE', 'LengthKM', 'GNIS_Name')
    for (cur_field in segment_fields){
      if (!(cur_field %in% colnames(segment_data_original))){
        stop(sprintf('%s field missing from gages file. Please check spelling of column header in csv file.', cur_field))
      }
    }
    segment_data_original <- segment_data_original@data[,segment_fields]

    # convert the COMID field from factor to string to integer
    segment_data_original$COMID <- as.integer(as.character(segment_data_original$COMID))
  }

  # if the user didn't pass in a filename for the gage station COMIDs
  # and Drainage Area (DA) then use the default, otherwise load what they passed in
  if (is.null(gages_filename)) {
    gages <- default_gages
  } else {
    # TODO - support other file formats...
    if (!file.exists(gages_filename)){
      stop(sprintf('Invalid gages filename provided: %s', gages_filename))
    }
    #read in CSV of the USGS gage stations
    gages <- utils::read.csv(gages_filename, header=TRUE, stringsAsFactors = FALSE)
    if (!('COMID' %in% colnames(gages))){
      stop('COMID field missing from gages file. Please check spelling of column header in csv file.')
    }
    gages <- gages$COMID
  }

  # if the user didn't pass in a filename for the to and from COMIDs for all segments
  # use the default, otherwise load what they passed in
  if (is.null(flow_ids_filename)) {
    flow_ids <- default_flow_ids
  } else {
    # TODO - support other file formats...
    if (!file.exists(flow_ids_filename)){
      stop(sprintf('Invalid flow_ids filename provided: %s', flow_ids_filename))
    }
    #Read in file(s) containing TO-COMID and FROM-COMID
    flow_ids <- foreign::read.dbf(flow_ids_filename)
    if (!('TOCOMID' %in% colnames(flow_ids))){
      stop('TOCOMID field missing from flow_ids file. Please check spelling of column header in database file.')
    }
    if (!('FROMCOMID' %in% colnames(flow_ids))){
      stop('FROMCOMID field missing from flow_ids file. Please check spelling of column header in database file.')
    }
    flow_ids <- flow_ids[,c('TOCOMID', 'FROMCOMID')]
  }

  # if the user didn't pass in a filename for the to and from COMIDs for all segments
  # use the default, otherwise load what they passed in
  if (is.null(valid_artificial_segments_filename)) {
    valid_artificial_segments <- default_valid_artificial_segments
  } else {
    # TODO - support other file formats...
    if (!file.exists(valid_artificial_segments_filename)){
      stop(sprintf('Invalid valid_artificial_segments filename provided: %s', valid_artificial_segments_filename))
    }
    #Read in file(s) containing TO-COMID and FROM-COMID
    valid_artificial_segments <- utils::read.csv(valid_artificial_segments_filename,
                                                header=TRUE,
                                                stringsAsFactors = FALSE)
    if (!('COMID' %in% colnames(valid_artificial_segments))){
      stop('COMID field missing from valid_artificial_segments file. Please check spelling of column header in csv file.')
    }
    valid_artificial_segments = valid_artificial_segments$COMID
  }

  # return all the dataframes in a list
  return( list(segment_data_original=segment_data_original,
               gages=gages,
               flow_ids=flow_ids,
               valid_artificial_segments=valid_artificial_segments) )
}
