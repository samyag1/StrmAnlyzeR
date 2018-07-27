#' Title
#'
#' @param debug_COMIDs
#' @param NHDFlowline_folder
#' @param gages_filename
#' @param to_from_COMIDs_filename
#' @param valid_artificial_segments_filename
#'
#' @return
#' @export
#'
#' @examples
getDebugInfo <- function(debug_COMIDs,
                         NHDFlowline_folder=NULL,
                         gages_filename=NULL,
                         to_from_COMIDs_filename=NULL,
                         valid_artificial_segments_filename=NULL) {

  ##############################################################
  ########READ IN AND PROCESS RAW DATA #########################
  ##############################################################

  # load the raw data into memory if at least one filename is provided by the user
  load_data <- (!is.null(NHDFlowline_folder) |
                  !is.null(gages_filename) |
                  !is.null(to_from_COMIDs_filename) |
                  !is.null(valid_artificial_segments_filename))
  if (load_data){

    # load the raw dataframes
    raw_data <- loadData(NHDFlowline_folder,
                         gages_filename,
                         to_from_COMIDs_filename,
                         valid_artificial_segments_filename)

    # Now preprocess the data using the data structures we just loaded
    preproc_data <- preprocessData(raw_data$segment_data_original,
                                   raw_data$gages,
                                   raw_data$flow_ids,
                                   raw_data$valid_artificial_segments)

    valid_artificial_segments <- raw_data$valid_artificial_segments
    segment_data <- preproc_data$segment_data
    segment_data_gage <- preproc_data$segment_data_gage
    segment_to_from_COMIDs <- preproc_data$segment_to_from_COMIDs
  } else {
    # otherwise the default data is to be used, so just use the defaults stored in the package
    valid_artificial_segments <- default_valid_artificial_segments
    segment_data <- default_segment_data
    segment_data_gage <- default_segment_data_gage
    segment_to_from_COMIDs <- default_segment_to_from_COMIDs
  }

  # find those COMIDs passed in that are not in the NHDFlowline data (segment_data)
  missing_COMIDs <- debug_COMIDs[!(debug_COMIDs %in% segment_data$COMID)]
  print('The following COMIDs were passed in as debug COMIDs but do not exist in the NHDFlowline files being used.')
  print(missing_COMIDs)

  # get the segment data, and upstream/downstream COMIDs for all the debug COMID segments
  segment_data_debug <- segment_data[segment_data$COMID %in% debug_COMIDs, ]

  # add the upstream and downstream COMID fields to the dataframe, initializing with NA
  segment_data_debug[,c('Up_COMIDs', 'Down_COMIDs')] <- NA

  # iterate over all debug COMIDs and find the upstream and downstream COMIDs for each one,
  # adding them to the return dataframe as a string
  for (i in 1:nrow(segment_data_debug)) {
    cur_COMID <- segment_data_debug[i,'COMID']
    downstream_COMIDs <- segment_to_from_COMIDs[segment_to_from_COMIDs$COMID==cur_COMID,'TOCOMID']
    segment_data_debug[i, 'Down_COMIDs'] <- paste(downstream_COMIDs, collapse=" ")
    upstream_COMIDs <- segment_to_from_COMIDs[segment_to_from_COMIDs$TOCOMID==cur_COMID,'COMID']
    segment_data_debug[i, 'Up_COMIDs'] <- paste(upstream_COMIDs, collapse=" ")
  }

  # if there are missing COMIDs, then just add them to the end of the data frame with just the COMID
  if (length(missing_COMIDs) > 0) {
    startIdx <- nrow(segment_data_debug) +1
    endIdx <- startIdx + length(missing_COMIDs) - 1
    segment_data_debug[startIdx:endIdx,'COMID'] <- missing_COMIDs
  }

  return (segment_data_debug)
}
