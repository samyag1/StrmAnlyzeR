#' Stream Network Analysis
#'
#' The user provides stream gages of interest, and the main package function determines the upstream and downstream National Hydrography Dataset (NHD) stream segments and total stream network length that 
#' are within a user-defined drainage area percentage threshold of gages.
#'
#' General user input include: 1) a list of gage stations to analyze, and 2) the upstream and downstream drainage area percentage
#' threshold parameters. Additionally, the user can provide
#' custom NHD Flowline shape files for the region of interest
#' (the default contains information for California) along with a custom
#' dbf databse containing the To and From COMIDs that indicate connections
#' between all the stream segments. Finally, some valid stream segments are
#' labeled as "artifical" segments for various reasons, and these
#' can be specified for inclusion in the analysis.
#'
#' @param pct_threshold_down Numeric Scalar. Threshold for inclusion in
#'   downstream analysis. Value is percentage of gage station's drainage
#'   area / 100.0 to be included in the downstream analysis. The number
#'   must be greater than 1.0. Default value = 1.1 (10\% greater)
#' @param pct_threshold_up Numeric Scalar. Threshold for inclusion in
#'   upstream analysis. Value is percentage of gage station's drainage
#'   area / 100.0 to be included in the upstream analysis. The number
#'   must be less than 1.0 and greater than 0.0. Default value = 0.9 (10\% less)
#' @param NHDFlowline_folder String. Path containing the NHD flowline
#'  files. Value of "Null" indicates use of default file which contains
#'  all stream segments for California.
#' @param gages_filename String. Filename of csv file containing the COMIDs
#'   where gage stations are located, and from which the upstream and
#'   downstream analyses are done.
#' @param to_from_COMIDs_filename String. Filename of dbf file containing
#'   the TO and FROM COMIDs which represent the connections between stream
#'   segments used in the analysis.
#' @param val_artificial_segs_filename String. Filename of csv file containing
#'   COMIDs of segments with an FTYPE of "artificial" that are actually valid
#'   stream segments.
#'
#' @return data.frame. Results of stream network analysis for all gage station
#'   stream segments.
#' @export
#'
#' @examples
#' # Default thresholds and data files
#' \dontrun{analyze_streams()}
#'
#' # Custom thresholds and default data files
#' \dontrun{analyze_streams(1.3, 0.7)}
#'
#' # Default thresholds and custom data files
#' \dontrun{analyze_streams(NHDFlowline_folder = "myFolderName",
#'                          gages_filename = "myFilename",
#'                          to_from_COMIDs_filename = "myFilename",
#'                          val_artificial_segs_filename = "myFilename")}
analyze_streams <- function(pct_threshold_down = 1.1,
                            pct_threshold_up = .9,
                            NHDFlowline_folder = NULL,
                            gages_filename = NULL,
                            to_from_COMIDs_filename = NULL,
                            val_artificial_segs_filename = NULL) {

  ##############################################################
  ##################### Error Checking #########################
  ##############################################################
  if (pct_threshold_down > 1.5){
    stop(sprintf(paste0("Provided downstream threshold greater than ",
                        "50 percent, which is invalid: %.02f"),
                 pct_threshold_down))
  }
  if (pct_threshold_up < 0.5){
    stop(sprintf(paste0("Provided upstream threshold greater than ",
                        "50 percent, which is invalid: %.02f"),
                 pct_threshold_up))
  }

  ##############################################################
  ######## READ IN AND PROCESS RAW DATA ########################
  ##############################################################

  # load the raw data into memory if at least one filename is
  # provided by the user
  load_data <- (!is.null(NHDFlowline_folder) |
                  !is.null(gages_filename) |
                  !is.null(to_from_COMIDs_filename) |
                  !is.null(val_artificial_segs_filename))
  if (load_data){

    # load the raw dataframes
    raw_data <- load_data(NHDFlowline_folder,
                          gages_filename,
                          to_from_COMIDs_filename,
                          val_artificial_segs_filename)

    # Now preprocess the data using the data structures we just loaded
    preproc_data <- preprocess_data(raw_data$segment_data_original,
                                    raw_data$gages,
                                    raw_data$flow_ids,
                                    raw_data$valid_artificial_segments)

    valid_artificial_segments <- raw_data$valid_artificial_segments
    segment_data <- preproc_data$segment_data
    segment_data_gage <- preproc_data$segment_data_gage
    segment_to_from_COMIDs <- preproc_data$segment_to_from_COMIDs
  } else {
    # otherwise the default data is to be used, so just use the
    # defaults stored in the package
    valid_artificial_segments <- default_valid_artificial_segments
    segment_data <- default_segment_data
    segment_data_gage <- default_segment_data_gage
    segment_to_from_COMIDs <- default_segment_to_from_COMIDs
  }

  #####################################
  # Do Downstream analysis
  #####################################

  # do the downstream analysis, which will find all the downstream segments
  # that have a drainage area within the threshold percentage of the original
  # segment, for all gage segments
  down_output <- do_downstream_analysis(pct_threshold_down,
                                        segment_data_gage,
                                        segment_data,
                                        valid_artificial_segments,
                                        segment_to_from_COMIDs)

  #####################################
  # Do Upstream analysis
  #####################################

  # do the upstream analysis, which will find all the downstream segments
  # that have a drainage area within the threshold percentage of the original
  # segment, for all gage segments
  up_output <- do_upstream_analysis(pct_threshold_up,
                                    segment_data_gage,
                                    segment_data,
                                    valid_artificial_segments,
                                    segment_to_from_COMIDs)

  #####################################
  # Assemble Final Output
  #####################################

  # concatonate the upstream and downstram output with the final output
  final_output <- cbind(segment_data_gage, down_output, up_output)

  # finally calculate the sum of the upstream and downstream segments
  # and length
  final_output["step_n_total"] <- rowSums(cbind(down_output$step_n_down,
                                                up_output$step_n_up),
                                          na.rm = TRUE) + 1
  final_output["cum_len_total"] <- rowSums(cbind(down_output$cum_len_down,
                                                 up_output$cum_len_up,
                                                 segment_data_gage$LengthKM),
                                           na.rm = TRUE)
  final_output["total_COMIDs"] <- sprintf("%s %s %s",
                                          down_output$down_COMIDs,
                                          up_output$up_COMIDs,
                                          segment_data_gage$COMID)

  # Get the COMIDs for all the upstream/downstream segments within threshold
  # that do not have gage stations
  up_down_COMIDs_nogage <- get_thresholded_COMIDs(final_output)

  # now get all the segment data for the upstream and downstream segments
  # that do not have gage stations
  segment_data_up_down <- segment_data[segment_data$COMID %in%
                                         up_down_COMIDs_nogage, ]

  # find all the columns from final_output that are not in the new
  # segment upstream/downstream segment data, and add them to the segment_data_up_down
  # with all NA values
  columns_not_up_down <- colnames(final_output)
  columns_not_up_down <- columns_not_up_down[!(columns_not_up_down %in%
                                               colnames(segment_data_up_down))]
  segment_data_up_down[, columns_not_up_down] <- NA

  # and bind it to the gage segment data to create the final output
  final_output <- rbind(final_output, segment_data_up_down)

  # create a column that indicates whether the segment contains a
  # gage station, since we will add all segments within the upstream/downstream
  # thresholds to the final output dataframe and many of those
  # do not have gage stations
  final_output[, "has_gage"] <- !(final_output$COMID %in%
                                  up_down_COMIDs_nogage)

  # Return the dataframe containing all the upstream and downstream
  # data calculated for all the segments containing USGS gage stations
  return(final_output)
}


#' Extract Upstream and Downstream Output
#'
#' Utility function to extract information about all the
#' upstream and downstream segments within threshold of a
#' given gage station.
#'
#' @param segment_COMID Integer. Gage station COMID to find all
#'   upstream and downstream segments within the threshold.
#' @param output_data data.frame. Output dataframe from the
#'   analyse_streams function.
#'
#' @return data.frame. Rows from the \code{output_data} parameter
#'   for the upstream and downstream COMIDs
#' @export
#'
#' @examples
#' \dontrun{get_segment_data(gage_station1_COMID, output)}
get_segment_data <- function(segment_COMID, output_data){

  # get the string that contains all the COMIDs,
  # separated by a single space (whitespace)
  up_down_COMIDs <- output_data[output_data$COMID == segment_COMID,
                                "total_COMIDs"]

  # split that list into the upstream and downstream COMIDs
  up_down_COMIDs <- as.numeric(unlist(strsplit(up_down_COMIDs, " ")))

  # remove the NAs
  up_down_COMIDs <- up_down_COMIDs[!is.na(up_down_COMIDs)]

  # now get all the segment data for the upstream and downstream segments and return it
  segment_up_down_data <- output_data[output_data$COMID %in% up_down_COMIDs, ]
  return(segment_up_down_data)
}


#' Write Stream Network Analysis Output
#'
#' Function to write out the results of the stream network analysis
#' returned from the \code{analyze_streams()} function. Output can
#' be written as a either a csv file, a set of ogr shape files, or both.
#'
#' @param output_data data.frame. Output data.frame from the
#'   \code{analyze_streams} function.
#' @param csv_filename String. Name of filename where the output will be
#'   saved as a comma seperated value (csv) file.
#' @param ogr_folder  String. Name of folder with the set of OGR shape files
#'   will be saved.
#'
#' @export
#'
#' @examples
#' # Write a csv file
#' \dontrun{write_data(output, csv_filename = "my_csv_filename")}
#'
#' # Write OGR shape files
#' \dontrun{write_data(output, ogr_folder = "my_ogr_folder")}
write_data <- function(output_data,
                       csv_filename=NULL,
                       ogr_folder=NULL) {
  if (!is.null(csv_filename)) {
    utils::write.csv(output_data, csv_filename)
  }

  if (!is.null(ogr_folder)) {
    rgdal::writeOGR(output_data$total_COMIDs,
                    ogr_folder,
                    "StrmAnlyzeR_Output",
                    driver = "ESRI Shapefile",
                    check_exists = TRUE,
                    overwrite_layer = TRUE)
  }
}
