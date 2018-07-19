doDownstreamAnalysis <- function(pct_threshold_down,
                                 segment_data_gage,
                                 segment_data,
                                 valid_artificial_segments,
                                 segment_to_from_COMIDs){

  # create a dataframe with the new columns that we'll populate
  vec_na <- rep(NA, nrow(segment_data_gage))
  output_df <- data.frame('cum_len_down'=vec_na, 'step_n_down'=vec_na, 'down_COMIDs'=vec_na)

  # iterate over all the segments with USGS gage stations
  # and calculate the downstream segments whose
  # total drainage area is greater than the threshold
  for (cur_row in 1:nrow(segment_data_gage)) {

    # get the COMID and drainage area (DA) of the current segment
    cur_COMID <- segment_data_gage[cur_row, 'COMID']
    cur_DA <- segment_data_gage[cur_row,'TotDASqKM']

    # Do some error checking to fail explicitly, instead of later on
    if (cur_DA == 0) {
      stop(sprintf('Drainage area (TotDASqKM) equals 0 for COMID: %s', cur_COMID))
    }
    if (is.na(cur_DA)) {
      stop(sprintf('Drainage area (TotDASqKM) is NA for COMID: %s', cur_COMID))
    }

    # calculate the downstream segments below threshold for the current segment
    vals <- findDownstreamSegments(cur_COMID,
                                   cur_DA,
                                   pct_threshold_down,
                                   segment_to_from_COMIDs,
                                   segment_data,
                                   valid_artificial_segments)

    # if there are any downstream segments within threshold, then calculate the
    # information to store, and store it
    if (length(vals) > 0) {

      # first check that there are no duplicate entries, since we now allow divergences, which
      # means that streams can split apart and come back together.
      duplicates = duplicated(vals$ToCOMID)
      vals = vals[!duplicates,]

      # calculate summary stats and store the downstream segments
      output_df[cur_row, 'cum_len_down'] <- sum(as.numeric(vals[, 'Length']))
      output_df[cur_row, 'step_n_down'] <- nrow(vals)
      output_df[cur_row, 'down_COMIDs'] <- paste(vals[, 'ToCOMID'], collapse=" ")
    }
  }

  # return the updates segment_data for gage stations
  return(output_df)
}
