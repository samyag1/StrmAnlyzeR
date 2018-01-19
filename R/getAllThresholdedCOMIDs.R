getAllThresholdedCOMIDs <- function(final_output){

  # concatonate all the Total COMID strings together, separating each with a space
  total_COMIDs_collapsed <- paste(final_output[,'total_COMIDs'], collapse=" ")

  # now convert that single string containing all the total COMIDs into a list
  total_COMIDs_single <- as.numeric(unlist(strsplit(total_COMIDs_collapsed, " ")))

  # remove NAs
  total_COMIDs_single <- total_COMIDs_single[!is.na(total_COMIDs_single)]

  # remove duplicates
  total_COMIDs_single <- total_COMIDs_single[!duplicated(total_COMIDs_single)]

  # remove segments with gages
  total_COMIDs_single <- total_COMIDs_single[!(total_COMIDs_single %in% final_output$COMID)]

  return(total_COMIDs_single)
}
