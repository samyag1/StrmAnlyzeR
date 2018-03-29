#' Title
#'
#' @param pct_threshold_down
#' @param pct_threshold_up
#' @param shapefile_folder
#' @param gages_filename
#' @param flow_ids_filename
#' @param valid_artificial_segments_filename
#'
#' @return
#' @export
#'
#' @examples
analyzeStreams <- function(pct_threshold_down = 1.1,
                    pct_threshold_up = .9,
                    shapefile_folder=NULL,
                    gages_filename=NULL,
                    flow_ids_filename=NULL,
                    valid_artificial_segments_filename=NULL) {

  ##############################################################
  ######## Error Checking #########################
  ##############################################################
  if (pct_threshold_down > 1.5){
    stop(sprintf('Provided downstream threshold greater than 50 percent, which is invalid: %.02f', pct_threshold_down))
  }
  if (pct_threshold_up < 0.5){
    stop(sprintf('Provided upstream threshold greater than 50 percent, which is invalid: %.02f', pct_threshold_up))
  }

  ##############################################################
  ########READ IN AND PROCESS RAW DATA #########################
  ##############################################################

  # load the raw data into memory if at least one filename is provided by the user
  load_data <- (!is.null(shapefile_folder) |
                !is.null(gages_filename) |
                !is.null(flow_ids_filename) |
                !is.null(valid_artificial_segments_filename))
  if (load_data){

    # load the raw dataframes
    raw_data <- loadData(shapefile_folder,
                         gages_filename,
                         flow_ids_filename,
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

  #####################################
  # Do Downstream analysis
  #####################################

  # do the upstream analysis which will find all the upstream segments whose drainage
  # area is below the threshold percentage of the original segment, for all gage segments
  down_output <- doDownstreamAnalysis(pct_threshold_down,
                                      segment_data_gage,
                                      segment_data,
                                      valid_artificial_segments,
                                      segment_to_from_COMIDs)

  #####################################
  # Do Upstream analysis
  #####################################

  # do the upstream analysis which will find all the upstream segments whose drainage
  # area is below the threshold percentage of the original segment, for all gage segments
  up_output <- doUpstreamAnalysis(pct_threshold_up,
                                  segment_data_gage,
                                  segment_data,
                                  valid_artificial_segments,
                                  segment_to_from_COMIDs)

  #####################################
  # Assemble Final Output
  #####################################

  # concatonate the up and down output with the final output
  final_output <- cbind(segment_data_gage, down_output, up_output)

  # finally calculate the sum of the upstream and downstream segments and length
  final_output['step_n_total'] <- rowSums(cbind(down_output$step_n_down, up_output$step_n_up), na.rm=TRUE) + 1
  final_output['cum_len_total'] <- rowSums(cbind(down_output$cum_len_down,
                                                 up_output$cum_len_up,
                                                 segment_data_gage$LengthKM),
                                           na.rm=TRUE)
  final_output['total_COMIDs'] <- sprintf('%s %s %s',
                                          down_output$down_COMIDs,
                                          up_output$up_COMIDs,
                                            segment_data_gage$COMID)

  # Get the COMIDs for all the up/down segments within threshold that do not have gage stations
  up_down_COMIDs_nogage <- getAllThresholdedCOMIDs(final_output)

  # now get all the segment data for the up and down segments that do not have gage stations
  segment_data_up_down <- segment_data[segment_data$COMID %in% up_down_COMIDs_nogage, ]

  # find all the columns from final_output that aren't in the new segment up/down segment data,
  # and add them to the segment_data_up_down with all NA values
  columns_not_up_down <- colnames(final_output)
  columns_not_up_down <- columns_not_up_down[!(columns_not_up_down %in% colnames(segment_data_up_down))]
  segment_data_up_down[,columns_not_up_down] <- NA

  # and bind it to the gage segment data to create the final output
  final_output <- rbind(final_output, segment_data_up_down)

  # create a column that indicates whether the segment contains a gage station since
  # we will add all segments within the up/down thresholds to the final output dataframe
  # and many of those do not have gage stations
  final_output[,'has_gage'] <- !(final_output$COMID %in% up_down_COMIDs_nogage)

  # Return the dataframe containing all the up and downstream data calculated for
  # all the segments containing USGS gage stations
  return(final_output)
}
