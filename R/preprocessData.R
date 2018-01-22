#' Title
#'
#' @param segment_data_original
#' @param gages
#' @param flow_ids
#' @param valid_artifical_segments
#'
#' @return
#' @export
#'
#' @examples
preprocessData <- function(segment_data_original,
                           gages,
                           flow_ids,
                           valid_artifical_segments) {

  ##########################################################################################
  ###################### Subset the segment data ###########################################
  ##########################################################################################

  # This will create a new dataframe with all the data from both the segment data
  # and the flow_ids, which contains To and From ComIDs. This new dataframe will be
  # longer than segment data, because it has multiple rows for segments with multiple
  # downstream segments
  segment_to_from_COMIDs <- base::merge(segment_data_original,
                                        flow_ids,
                                        by.x = "COMID",
                                        by.y = "FROMCOMID",
                                        all.x = T,
                                        all.y = F)

  # Remove segments that serve as from-segments for more than one node,
  # because they're upsteam of divergences and thus problematic
  multi_from_segments <- segment_to_from_COMIDs$COMID[duplicated(segment_to_from_COMIDs$COMID)]
  segment_data <- subset(segment_data_original, !(segment_data_original$COMID %in% multi_from_segments))
  segment_to_from_COMIDs <- subset(segment_to_from_COMIDs, !(segment_to_from_COMIDs$COMID %in% multi_from_segments))

  # Add the TOCOMID values to the segment_data as a column
  segment_data$TOCOMID <- segment_to_from_COMIDs[order(match(segment_to_from_COMIDs$COMID, segment_data$COMID)),]$TOCOMID

  # remove the rows in the segment_to_from_COMIDs dataframe that have missing ToCOMIDs and onbly keep
  # the necessary columns, namely COMID and TOCOMID
  segment_to_from_COMIDs <- segment_to_from_COMIDs[!is.na(segment_to_from_COMIDs$TOCOMID),c('TOCOMID','COMID')]

  ##########################################################################################
  ###################### Create a Subset for just the USGS gage stations ###################
  ##########################################################################################

  # subset the segment data finding only those streams with gages gage data
  # NOTE: THE SHAPEFILE DRIVER TRUNCATED THE COLUMN NAME from NHDV2_COMID to NHDV2_COMI
  segment_data_gage <- segment_data[segment_data$COMID %in% gages,]

  # Only keep segments with gage stations in them that are valid nodes,
  # which include 'StreamRiver' segments and artifical segments that are deemed valid
  valid_segment_idxs <- isValidNode(segment_data_gage$COMID,
                                    segment_data_gage$FTYPE,
                                    valid_artifical_segments)
  segment_data_gage <- segment_data_gage[valid_segment_idxs,]

  # make sure that all the segments with gage stations have unique COMIDs
  assertthat::are_equal(unique(segment_data_gage[,'COMID']),nrow(segment_data_gage))

  # return the preprocessed data
  return(list(segment_data=segment_data,
              segment_data_gage=segment_data_gage,
              segment_to_from_COMIDs=segment_to_from_COMIDs))
}
