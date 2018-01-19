#' Title
#'
#' @param segment_COMID
#' @param output_data
#'
#' @return
#' @export
#'
#' @examples
getSegmentUpDownData <-function(segment_COMID, output_data){

  # get the string that contains all the COMIDs, separated by a single space (whitespace)
  up_down_COMIDs <- output_data[output_data$COMID == segment_COMID, 'total_COMIDs']

  # split that list into the up and down stream COMIDs
  up_down_COMIDs <- as.numeric(unlist(strsplit(up_down_COMIDs, " ")))

  # remove the NAs
  up_down_COMIDs <- up_down_COMIDs[!is.na(up_down_COMIDs)]

  # now get all the segment data for the up and down segments and return it
  segment_up_down_data <- output_data[output_data$COMID %in% up_down_COMIDs, ]
  return(segment_up_down_data)
}
