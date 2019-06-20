do_downstream_analysis <- function(pct_threshold_down,
                                   segment_data_gage,
                                   segment_data,
                                   valid_artificial_segments,
                                   segment_to_from_COMIDs){

  # create a dataframe with the new columns that we'll populate
  vec_na <- rep(NA, nrow(segment_data_gage))
  output_df <- data.frame("cum_len_down" = vec_na,
                          "step_n_down" = vec_na,
                          "down_COMIDs" = vec_na)

  # iterate over all the segments with USGS gage stations
  # and calculate the downstream segments whose
  # total drainage area is greater than the threshold
  for (cur_row in 1:nrow(segment_data_gage)) {

    # get the COMID and drainage area (DA) of the current segment
    cur_COMID <- segment_data_gage[cur_row, "COMID"]
    cur_DA <- segment_data_gage[cur_row, "TotDASqKM"]

    # Do some error checking for invalid segment data
    if (cur_DA == 0) {
      print(sprintf(paste0("Drainage area (TotDASqKM) equals 0 for COMID: %s.",
                    "Continuing analysis"),
                    cur_COMID))
      next
    }
    if (is.na(cur_DA)) {
      print(sprintf(paste0("Drainage area (TotDASqKM) is NA for COMID: %s.",
                    "Continuing analysis"),
                    cur_COMID))
      next
    }

    # calculate the downstream segments below threshold for the current segment
    vals <- find_downstream_segments(cur_COMID,
                                     cur_DA,
                                     pct_threshold_down,
                                     segment_to_from_COMIDs,
                                     segment_data,
                                     valid_artificial_segments)

    # if there are any downstream segments within threshold, then calculate the
    # information to store, and store it
    if (length(vals) > 0) {

      # first check that there are no duplicate entries, since we now allow
      # divergences, which means that streams can split apart and come back
      # together.
      duplicates <- duplicated(vals$ToCOMID)
      vals <- vals[!duplicates, ]

      # calculate summary stats and store the downstream segments
      output_df[cur_row, "cum_len_down"] <- sum(as.numeric(vals[, "Length"]))
      output_df[cur_row, "step_n_down"] <- nrow(vals)
      output_df[cur_row, "down_COMIDs"] <- paste(vals[, "ToCOMID"],
                                                 collapse = " ")
    }
  }

  # return the updates segment_data for gage stations
  return(output_df)
}


do_upstream_analysis <- function(pct_threshold_up,
                                 segment_data_gage,
                                 segment_data,
                                 valid_artificial_segments,
                                 segment_to_from_COMIDs) {

  # create a dataframe with the new columns that we'll populate
  vec_na <- rep(NA, nrow(segment_data_gage))
  output_df <- data.frame("cum_len_up" = vec_na,
                          "step_n_up" = vec_na,
                          "up_COMIDs" = vec_na)

  # iterate over all the segments with USGS gage stations and
  # calculate the upstream segments whose total drainage area
  # is less than the threshold
  for (cur_row in 1:nrow(segment_data_gage)) {

    # get the COMID and drainage area (DA) of the current segment
    cur_COMID <- segment_data_gage[cur_row, "COMID"]
    cur_DA <- segment_data_gage[cur_row, "TotDASqKM"]

    # Do some error checking for invalid segment data
    if (cur_DA == 0) {
      print(sprintf(paste0("Drainage area (TotDASqKM) equals 0 for COMID: %s.",
                    "Continuing analysis"),
                    cur_COMID))
      next
    }
    if (is.na(cur_DA)) {
      print(sprintf(paste0("Drainage area (TotDASqKM) is NA for COMID: %s.",
                    "Continuing analysis"),
                    cur_COMID))
      next
    }

    # find the upstream segments that are within threshold
    # for the current gage segment
    vals <- find_upstream_segments(cur_COMID,
                                   cur_DA,
                                   pct_threshold_up,
                                   segment_to_from_COMIDs,
                                   segment_data,
                                   valid_artificial_segments)

    if (length(vals) > 0) {

      # first check that there are no duplicate entries, since we now
      # allow divergences, which means that streams can split apart
      # and come back together.
      duplicates <- duplicated(vals$FromCOMID)
      vals <- vals[!duplicates, ]

      # calculate summary stats and store the upstream segments
      output_df[cur_row, "cum_len_up"] <- sum(as.numeric(vals[, "Length"]))
      output_df[cur_row, "step_n_up"] <- nrow(vals)
      output_df[cur_row, "up_COMIDs"] <- paste(vals[, "FromCOMID"],
                                               collapse = " ")
    }
  }

  # return the updates segment_data for gage stations
  return(output_df)
}


# function that takes a single segment's COMID and a percentage threshold as arguments and returns the next n stream segments' drainage areas
find_downstream_segments <- function(comid,
                                     original_DA,
                                     pct_threshold,
                                     segment_to_from_COMIDs,
                                     segment_data,
                                     valid_artificial_segments) {

  # find the downstream node that connects to the current node
  comid_mask <- segment_to_from_COMIDs$COMID == comid
  downstream_COMIDs <- segment_to_from_COMIDs[comid_mask, "TOCOMID"]

  # get the relevant fields for the downstream segment
  fields_used <- c("TotDASqKM", "LengthKM", "COMID", "FTYPE")

  # If the downstream segment doesn't exist, it's because
  # it was removed as a replicate (i.e. divergence)
  # So only get it's value if it is present
  lens <- c()
  down_COMIDs <- c()
  for (cur_down_COMID in downstream_COMIDs){

    # It's possible that a downstream COMID can be in the
    # TO COMIDs file, but not the Flowline shapefile (segment_data),
    # in which case print a message and move on.
    if (!cur_down_COMID %in% segment_data$COMID){
      print(sprintf(paste0("Encountered To COMID: %i that is not in",
                    "the flowline shapefile. Continuing analysis"),
                    cur_down_COMID))
      next
    }

    # get the current upstream node's fields used
    cur_down_COMID_mask <- segment_data$COMID == cur_down_COMID
    cur_down_vals <- segment_data[cur_down_COMID_mask, fields_used]

    # get the current upstream node's drainage area
    cur_down_DA <- cur_down_vals[1, "TotDASqKM"]

    # calculate the percentage of the original DA that the current
    # upstream node's DA takes up
    cur_DA_pcnt <- cur_down_DA / original_DA

    # get teh FTYPE for the current upstram node
    cur_down_ftype <- cur_down_vals[1, "FTYPE"]

    # if the current upstream node is more than the percentage threshold
    # we've defined, then add it to our list of above threshold upstream
    # nodes, and recursively call this function on the current upstream node
    valid_node <- is_valid_node(cur_down_COMID,
                                cur_down_ftype,
                                valid_artificial_segments)
    if (cur_DA_pcnt < pct_threshold & valid_node) {

      # get the current downstream's segment length
      cur_down_length <- cur_down_vals[1, "LengthKM"]

      # add the current upstream node values to the return vectors
      lens <- c(cur_down_length)
      down_COMIDs <- c(cur_down_COMID)

      # recursively call this function on the current upstream node
      recursive_vals <- find_downstream_segments(cur_down_COMID,
                                                 original_DA,
                                                 pct_threshold,
                                                 segment_to_from_COMIDs,
                                                 segment_data,
                                                 valid_artificial_segments)

      if (nrow(recursive_vals) > 0) {
        # now concotanote the results of the recusive call with
        # those of the current call
        lens <- c(lens, recursive_vals$Length)
        down_COMIDs <- c(down_COMIDs, recursive_vals$ToCOMID)
      }

      # now break out since only 1 upstream node can be above thr threshold
      break
    }

    # otherwise move on to the next downstream node
  }

  # retrun a vector of the vectors we care about (DA, length, FTYPE)
  return_DF <- data.frame(Length = lens, ToCOMID = down_COMIDs)
  return(return_DF)
}


find_upstream_segments <- function(comid,
                                   original_DA,
                                   pct_threshold,
                                   segment_to_from_COMIDs,
                                   segment_data,
                                   valid_artificial_segments) {
  # define the fields that will be used
  fields_used <- c("TotDASqKM", "LengthKM", "FTYPE")

  # find all of the upstream nodes that connect to the current node
  comid_mask <- segment_to_from_COMIDs$TOCOMID == comid
  upstream_COMIDs <- segment_to_from_COMIDs[comid_mask, "COMID"]

  # iterate over the upstream comIDs until we find one that has more
  # than the pct we're looking for
  lens <- c()
  up_COMIDs <- c()
  for (cur_up_COMID in upstream_COMIDs){

    # It's possible that a downstream COMID can be int he TO COMIDs file,
    # but not the Flowline shapefile (segment_data), in which case print
    # a message and move on.
    if (!cur_up_COMID %in% segment_data$COMID){
      print(sprintf(paste0("Encountered From COMID: %i that is not in the",
                    "flowline shapefile. Continuing analysis"),
                    cur_up_COMID))
      next
    }

    # get the current upstream node's fields used
    cur_up_COMID_mask <- segment_data$COMID == cur_up_COMID
    cur_up_vals <- segment_data[cur_up_COMID_mask, fields_used]

    # get the current upstream node's drainage area
    cur_up_DA <- cur_up_vals[1, "TotDASqKM"]

    # calculate the percentage of the original DA that
    # the current upstream node's DA takes up
    cur_DA_pcnt <- cur_up_DA / original_DA

    # get teh FTYPE for the current upstram node
    cur_up_ftype <- cur_up_vals[1, "FTYPE"]

    # if the current upstream node is more than the percentage threshold
    # we've defined, then add it to our list of above threshold upstream
    # nodes, and recursively call this function on the current upstream node
    valid_node <- is_valid_node(cur_up_COMID,
                                cur_up_ftype,
                                valid_artificial_segments)
    if (cur_DA_pcnt > pct_threshold & valid_node) {

      # get the current upstream's segment length
      cur_up_length <- cur_up_vals[1, "LengthKM"]

      # add the current upstream node values to the return vectors
      lens <- c(cur_up_length)
      up_COMIDs <- c(cur_up_COMID)

      # recursively call this function on the current upstream node
      recursive_vals <- find_upstream_segments(cur_up_COMID,
                                             original_DA,
                                             pct_threshold,
                                             segment_to_from_COMIDs,
                                             segment_data,
                                             valid_artificial_segments)

      if (nrow(recursive_vals) > 0) {
        # now concotanote the results of the recusive call
        # with those of the current call
        lens <- c(lens, recursive_vals$Length)
        up_COMIDs <- c(up_COMIDs, recursive_vals$FromCOMID)
      }

      # now break out since only 1 upstream node can be above thr threshold
      break
    }

    # otherwise move on to the next upstream node
  }

  # retrun a vector of the vectors we care about (DA, length, FTYPE)
  return_DF <- data.frame(Length = lens, FromCOMID = up_COMIDs)
  return(return_DF)
}

get_thresholded_COMIDs <- function(final_output){

  # concatonate all the Total COMID strings together,
  #separating each with a space
  total_COMIDs_collapsed <- paste(final_output[, "total_COMIDs"],
                                  collapse = " ")

  # now convert that single string containing all
  # the total COMIDs into a list
  list_COMIDs <- unlist(strsplit(total_COMIDs_collapsed, " "))
  total_COMIDs_single <- suppressWarnings(as.numeric(list_COMIDs))

  # remove NAs
  total_COMIDs_single <- total_COMIDs_single[!is.na(total_COMIDs_single)]

  # remove duplicates
  total_COMIDs_single <- total_COMIDs_single[!duplicated(total_COMIDs_single)]

  # remove segments with gages
  segs_without_gages <- !(total_COMIDs_single %in% final_output$COMID)
  total_COMIDs_single <- total_COMIDs_single[segs_without_gages]

  return(total_COMIDs_single)
}


is_valid_node <- function(comids, ftypes, valid_COMIDs){
  stopifnot(sum(is.na(comids)) == 0)
  is_stream_river <- !is.na(ftypes) & ftypes == "StreamRiver"
  is_valid_artificial <- comids %in% valid_COMIDs
  return(is_stream_river | is_valid_artificial)
}
