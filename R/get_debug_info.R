get_debug_info <- function(debug_COMIDs,
                           NHDFlowline_folder = NULL,
                           gages_filename = NULL,
                           to_from_COMIDs_filename = NULL,
                           val_artificial_segs_filename = NULL) {

  ##############################################################
  ########READ IN AND PROCESS RAW DATA #########################
  ##############################################################

  # load the raw data into memory if at least one filename is
  # provided by the user
  load_data <- (!is.null(NHDFlowline_folder) |
                  !is.null(gages_filename) |
                  !is.null(to_from_COMIDs_filename) |
                  !is.null(val_artificial_segs_filename))
  if (load_data){

    # load the raw dataframes
    raw_data <- load_data(NHDFlowline_folder,
                          gages_filename,
                          to_from_COMIDs_filename,
                          val_artificial_segs_filename)

    # Now preprocess the data using the data structures we just loaded
    preproc_data <- preprocess_data(raw_data$segment_data_original,
                                    raw_data$gages,
                                    raw_data$flow_ids,
                                    raw_data$valid_artificial_segments)

    segment_data <- preproc_data$segment_data
    segment_to_from_COMIDs <- preproc_data$segment_to_from_COMIDs
  } else {
    # otherwise the default data is to be used, so just use
    # the defaults stored in the package
    segment_data <- default_segment_data
    segment_to_from_COMIDs <- default_segment_to_from_COMIDs
  }

  # find those COMIDs passed in that are not in the
  # NHDFlowline data (segment_data)
  missing_COMIDs <- debug_COMIDs[!(debug_COMIDs %in% segment_data$COMID)]
  print("The following COMIDs were passed in as debug COMIDs
        but do not exist in the NHDFlowline files being used.")
  print(missing_COMIDs)

  # get the segment data, and upstream/downstream COMIDs
  # for all the debug COMID segments
  segment_data_debug <- segment_data[segment_data$COMID %in% debug_COMIDs, ]

  # add the upstream and downstream COMID fields to the dataframe,
  # initializing with NA
  segment_data_debug[, c("Up_COMIDs", "Down_COMIDs")] <- NA

  # iterate over all debug COMIDs and find the upstream and
  # downstream COMIDs for each one, adding them to the
  # return dataframe as a string
  for (i in 1:nrow(segment_data_debug)) {
    cur_COMID <- segment_data_debug[i, "COMID"]
    cur_COMID_mask <- segment_to_from_COMIDs$COMID == cur_COMID
    downstream_COMIDs <- segment_to_from_COMIDs[cur_COMID_mask, "TOCOMID"]
    segment_data_debug[i, "Down_COMIDs"] <- paste(downstream_COMIDs,
                                                  collapse = " ")
    upstream_COMIDs <- segment_to_from_COMIDs[cur_COMID_mask, "COMID"]
    segment_data_debug[i, "Up_COMIDs"] <- paste(upstream_COMIDs,
                                                collapse = " ")
  }

  # if there are missing COMIDs, then just add them to the end
  # of the data frame with just the COMID
  if (length(missing_COMIDs) > 0) {
    start_idx <- nrow(segment_data_debug) + 1
    end_idx <- start_idx + length(missing_COMIDs) - 1
    segment_data_debug[start_idx:end_idx, "COMID"] <- missing_COMIDs
  }

  return (segment_data_debug)
}
