# function that takes a single segment's COMID and a percentage threshold as arguments and returns the next n stream segments' drainage areas
findDownstreamSegments <- function(comid,
                                   original_DA,
                                   pct_threshold,
                                   segment_data,
                                   valid_artificial_segments) {

  # find the downstream node that connects to the current node
  down_COMID <- segment_data[segment_data$COMID == comid,]$TOCOMID

  # get the relevant fields for the downstream segment
  fields_used <- c('TotDASqKM', 'LengthKM', 'COMID', 'FTYPE')
  down_vals <- segment_data[segment_data$COMID == down_COMID,fields_used]

  # If the downstream segment doesn't exist, it's because it was removed as a replicate (i.e. divergence)
  # So only get it's value if it is present
  lens <- c()
  down_COMIDs <- c()
  if (nrow(down_vals) > 0 & !is.na(down_COMID)) {

    # get the current upstream node's drainage area
    down_DA <- down_vals[1,'TotDASqKM']

    # calculate the percentage of the original DA that the current upstream node's DA takes up
    DA_pcnt <- down_DA / original_DA

    # get teh FTYPE for the current upstram node
    down_ftype <- down_vals[1,'FTYPE']

    # if the downstream percentage of the original is less than the perentage threshold and it's a valid node
    # then add it to the return list and call this function recursively on the current downstream node
    if (DA_pcnt < pct_threshold & isValidNode(down_COMID, down_ftype, valid_artificial_segments)) {

      # get the length of the downstream segment
      down_length <- down_vals[1,'LengthKM']

      # add the current downstream node to our return list
      lens <- c(down_length)
      down_COMIDs <- c(down_COMID)

      # recursively call this function on the current upstream node
      recursive_vals <- findDownstreamSegments(down_COMID,
                                               original_DA,
                                               pct_threshold,
                                               segment_data,
                                               valid_artificial_segments)

      # if there were more downstream segments below threshold, then add them to the return values
      if (nrow(recursive_vals) > 0) {
        # now concotanote the results of the recusive call with those of the current call
        lens <- c(lens, recursive_vals[, 'Length'])
        down_COMIDs <- c(down_COMIDs, recursive_vals[, 'ToCOMID'])
      }
    }
  }

  # retrun a vector of the vectors we care about (DA, length, FTYPE)
  return_DF <- data.frame(Length=lens, ToCOMID=down_COMIDs)
  return(return_DF)
}
