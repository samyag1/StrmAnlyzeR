findUpstreamSegments <- function(comid,
                                 original_DA,
                                 pct_threshold,
                                 segment_to_from_COMIDs,
                                 segment_data,
                                 valid_artificial_segments) {
  # define the fields that will be used
  fields_used <- c('TotDASqKM', 'LengthKM', 'FTYPE')

  # find all of the upstream nodes that connect to the current node
  upstream_COMIDs <- segment_to_from_COMIDs[segment_to_from_COMIDs$TOCOMID==comid,'COMID']

  # iterate over the upstream comIDs until we find one that has more than the pct we're looking for
  lens <- c()
  up_COMIDs <- c()
  for (cur_up_COMID in upstream_COMIDs){

    # It's possible that a downstream COMID can be int he TO COMIDs file, but not the
    # Flowline shapefile (segment_data), in which case print a message and move on.
    if (!cur_up_COMID %in% segment_data$COMID){
      print(sprintf('Encountered From COMID: %i that is not in the flowline shapefile. Continuing analysis', cur_up_COMID))
      next
    }

    # get the current upstream node's fields used
    cur_up_vals <- segment_data[segment_data$COMID == cur_up_COMID, fields_used]

    # get the current upstream node's drainage area
    cur_up_DA <- cur_up_vals[1,'TotDASqKM']

    # calculate the percentage of the original DA that the current upstream node's DA takes up
    cur_DA_pcnt <- cur_up_DA / original_DA

    # get teh FTYPE for the current upstram node
    cur_up_ftype <- cur_up_vals[1,'FTYPE']

    # if the current upstream node is more than the percentage threshold we've defined, then
    # add it to our list of above threshold upstream nodes, and recursively call this function
    # on the current upstream node
    #  if (cur.DA.pcnt > pct & !is.na(cur.up.ftype) & cur.up.ftype == 'StreamRiver') {
    if (cur_DA_pcnt > pct_threshold & isValidNode(cur_up_COMID, cur_up_ftype, valid_artificial_segments)) {

      # get the current upstream's segment length
      cur_up_length <- cur_up_vals[1,'LengthKM']

      # add the current upstream node values to the return vectors
      lens <- c(cur_up_length)
      up_COMIDs <- c(cur_up_COMID)

      # recursively call this function on the current upstream node
      recursive_vals <- findUpstreamSegments(cur_up_COMID,
                                             original_DA,
                                             pct_threshold,
                                             segment_to_from_COMIDs,
                                             segment_data,
                                             valid_artificial_segments)

      if (nrow(recursive_vals) > 0) {
        # now concotanote the results of the recusive call with those of the current call
        lens <- c(lens, recursive_vals$Length)
        up_COMIDs <- c(up_COMIDs, recursive_vals$FromCOMID)
      }

      # now break out since only 1 upstream node can be above thr threshold
      break
    }

    # otherwise move on to the next upstream node
  }

  # retrun a vector of the vectors we care about (DA, length, FTYPE)
  return_DF <- data.frame(Length=lens, FromCOMID=up_COMIDs)
  return(return_DF)
}
