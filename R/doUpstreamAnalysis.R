doUpstreamAnalysis <- function(pct_threshold_up,
                               segment_data_gage,
                               segment_data,
                               valid_artificial_segments,
                               segment_to_from_COMIDs) {

  # create a dataframe with the new columns that we'll populate
  vec_na <- rep(NA, nrow(segment_data_gage))
  output_df <- data.frame('cum_len_up'=vec_na, 'step_n_up'=vec_na, 'up_COMIDs'=vec_na)

  # iterate over all the segments with USGS gage stations and calculate the upstream segments
  # whose total drainage area is less than the threshold
  for (cur_row in 1:nrow(segment_data_gage)) {

    # get the COMID and drainage area (DA) of the current segment
    cur_COMID <- segment_data_gage[cur_row, 'COMID']
    cur_DA <- segment_data_gage[cur_row,'TotDASqKM']

    # find the upstream segments that are within threshold for the current gage segment
    vals <- findUpstreamSegments(cur_COMID,
                                 cur_DA,
                                 pct_threshold_up,
                                 segment_to_from_COMIDs,
                                 segment_data,
                                 valid_artificial_segments)

    if (length(vals) > 0) {
      output_df[cur_row, 'cum_len_up'] <- sum(as.numeric(vals[, 'Length']))
      output_df[cur_row, 'step_n_up'] <- nrow(vals)
      output_df[cur_row, 'up_COMIDs'] <- paste(vals[, 'FromCOMID'], collapse=" ")
    }
  }

  # return the updates segment_data for gage stations
  return(output_df)
}
