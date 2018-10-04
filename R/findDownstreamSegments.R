# function that takes a single segment's COMID and a percentage threshold as arguments and returns the next n stream segments' drainage areas
findDownstreamSegments <- function(comid,
                                   original_DA,
                                   pct_threshold,
                                   segment_to_from_COMIDs,
                                   segment_data,
                                   valid_artificial_segments) {

  # find the downstream node that connects to the current node
  downstream_COMIDs <- segment_to_from_COMIDs[segment_to_from_COMIDs$COMID==comid,'TOCOMID']

  # get the relevant fields for the downstream segment
  fields_used <- c('TotDASqKM', 'LengthKM', 'COMID', 'FTYPE')

  # If the downstream segment doesn't exist, it's because it was removed as a replicate (i.e. divergence)
  # So only get it's value if it is present
  lens <- c()
  down_COMIDs <- c()
  for (cur_down_COMID in downstream_COMIDs){

    # It's possible that a downstream COMID can be int he TO COMIDs file, but not the
    # Flowline shapefile (segment_data), in which case print a message and move on.
    if (!cur_down_COMID %in% segment_data$COMID){
      print(sprintf('Encountered To COMID: %i that is not in the flowline shapefile. Continuing analysis', cur_down_COMID))
      next
    }

    # get the current upstream node's fields used
    cur_down_vals <- segment_data[segment_data$COMID == cur_down_COMID, fields_used]

    # get the current upstream node's drainage area
    cur_down_DA <- cur_down_vals[1,'TotDASqKM']

    # calculate the percentage of the original DA that the current upstream node's DA takes up
    cur_DA_pcnt <- cur_down_DA / original_DA

    # get teh FTYPE for the current upstram node
    cur_down_ftype <- cur_down_vals[1,'FTYPE']

    # if the current upstream node is more than the percentage threshold we've defined, then
    # add it to our list of above threshold upstream nodes, and recursively call this function
    # on the current upstream node
    #  if (cur.DA.pcnt > pct & !is.na(cur.up.ftype) & cur.up.ftype == 'StreamRiver') {
    if (cur_DA_pcnt < pct_threshold & isValidNode(cur_down_COMID, cur_down_ftype, valid_artificial_segments)) {

      # get the current downstream's segment length
      cur_down_length <- cur_down_vals[1,'LengthKM']

      # add the current upstream node values to the return vectors
      lens <- c(cur_down_length)
      down_COMIDs <- c(cur_down_COMID)

      # recursively call this function on the current upstream node
      recursive_vals <- findDownstreamSegments(cur_down_COMID,
                                               original_DA,
                                               pct_threshold,
                                               segment_to_from_COMIDs,
                                               segment_data,
                                               valid_artificial_segments)

      if (nrow(recursive_vals) > 0) {
        # now concotanote the results of the recusive call with those of the current call
        lens <- c(lens, recursive_vals$Length)
        down_COMIDs <- c(down_COMIDs, recursive_vals$ToCOMID)
      }

      # now break out since only 1 upstream node can be above thr threshold
      break
    }

    # otherwise move on to the next downstream node
  }

  # retrun a vector of the vectors we care about (DA, length, FTYPE)
  return_DF <- data.frame(Length=lens, ToCOMID=down_COMIDs)
  return(return_DF)
}
