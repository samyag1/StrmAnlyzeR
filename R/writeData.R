#' Title
#'
#' @param output_data Data to write out
#' @param csv_filename Filename to write out the dataframe
#' @param ogr_folder Folder to write the OGR shape files
#'
#' @return
#' @export
#'
#' @examples
writeData <- function(output_data, csv_filename=NULL, ogr_folder=NULL){
  if (!is.null(csv_filename)) {
    utils::write.csv(output_data, csv_filename)
  }

  if (!is.null(ogr_folder)) {
    rgdal::writeOGR(output_data$total_COMIDs, # TODO - make sure this is the same as totComIDsVecNoNA
                    ogr_folder,
                    'StrmAnlyzeR_Output',
                    driver = 'ESRI Shapefile',
                    check_exists = TRUE,
                    overwrite_layer = TRUE)
  }
}
