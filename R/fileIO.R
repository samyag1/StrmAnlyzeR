load_data <- function(NHDFlowline_folder = NULL,
                      gages_filename = NULL,
                      to_from_COMIDs_filename = NULL,
                      val_artificial_segs_filename = NULL) {

  # determine if the user specified custom input files to use, in which case
  # the perprocessData function will be used to load and process that data
  if (is.null(NHDFlowline_folder)){
    # if they didn't pass in a layer name then
    # they want to use the internal default dataframe
    segment_data_original <- default_segment_data_original
  } else {
    if (!file.exists(NHDFlowline_folder)){
      stop(sprintf("Invalid shapefile folder provided: %s",
                   NHDFlowline_folder))
    }
    # determine the layer name by reading the filenames in the
    # shapefile folder. All the files in that folder should have
    # the same file title, just different file extensions, and
    # that file title is the layer name expected by readOGR
    shape_filenames <- list.files(NHDFlowline_folder,
                                  include.dirs = F,
                                  recursive = T)
    if (length(shape_filenames) == 0){
      stop(sprintf("No files in the shapefile folder provided: %s",
                   NHDFlowline_folder))
    }
    layer_name <- tools::file_path_sans_ext(shape_filenames[1])

    # read in the shape file which contains all the data
    # about the water segments
    segment_data_original <- rgdal::readOGR(dsn = NHDFlowline_folder,
                                            layer = layer_name)

    # only keep the fields in the segment_data that will be used
    # in the analysis. And only keep the dataframe, getting rid
    # of the lines and other data in the layer since it's not used
    # here, and just takes up memory
    segment_fields <- c("COMID", "TotDASqKM", "FTYPE", "LengthKM", "GNIS_Name")
    for (cur_field in segment_fields){
      if (!(cur_field %in% colnames(segment_data_original@data))){
        stop(sprintf(paste0("%s field missing from shape file.",
                            "Please check spelling of field in OGR file."),
                     cur_field))
      }
    }
    segment_data_original <- segment_data_original@data[, segment_fields]

    # convert the COMID field from factor to string to integer
    comid_char <- as.character(segment_data_original$COMID)
    segment_data_original$COMID <- as.integer(comid_char)
  }

  # if the user didn't pass in a filename for the gage station COMIDs
  # and Drainage Area (DA) then use the default, otherwise load what
  # they passed in
  if (is.null(gages_filename)) {
    gages <- default_gages
  } else {
    # TODO - support other file formats...
    if (!file.exists(gages_filename)){
      stop(sprintf("Invalid gages filename provided: %s", gages_filename))
    }

    #read in CSV of the USGS gage stations
    gages <- utils::read.csv(gages_filename,
                             header = TRUE,
                             stringsAsFactors = FALSE)
    if (!("COMID" %in% colnames(gages))){
      stop(paste0("COMID field missing from gages file.",
                  "Please check spelling of column header in csv file."))
    }
    gages <- unique(gages$COMID)
  }

  # if the user didn't pass in a filename for the to and from COMIDs
  # for all segments use the default, otherwise load what they passed in
  if (is.null(to_from_COMIDs_filename)) {
    flow_ids <- default_flow_ids
  } else {
    # TODO - support other file formats...
    if (!file.exists(to_from_COMIDs_filename)){
      stop(sprintf("Invalid flow_ids filename provided: %s",
                   to_from_COMIDs_filename))
    }

    #Read in file(s) containing TO-COMID and FROM-COMID
    flow_ids <- foreign::read.dbf(to_from_COMIDs_filename)
    if (!("TOCOMID" %in% colnames(flow_ids))){
      stop(paste0("TOCOMID field missing from flow_ids file.",
                  "Please check spelling of column header in database file."))
    }
    if (!("FROMCOMID" %in% colnames(flow_ids))){
      stop(paste0("FROMCOMID field missing from flow_ids file.",
                  "Please check spelling of column header in database file."))
    }
    flow_ids <- flow_ids[, c("TOCOMID", "FROMCOMID")]
  }

  # if the user didn't pass in a filename for the to and from COMIDs
  # for all segments use the default, otherwise load what they passed in
  if (is.null(val_artificial_segs_filename)) {
    valid_artificial_segments <- default_valid_artificial_segments
  } else {
    # TODO - support other file formats...
    if (!file.exists(val_artificial_segs_filename)){
      stop(sprintf("Invalid valid_artificial_segments filename provided: %s",
                   val_artificial_segs_filename))
    }

    #Read in file(s) containing TO-COMID and FROM-COMID
    valid_artificial_segments <- utils::read.csv(val_artificial_segs_filename,
                                                 header = TRUE,
                                                 stringsAsFactors = FALSE)
    if (!("COMID" %in% colnames(valid_artificial_segments))){
      stop(paste0("COMID field missing from valid_artificial_segments file.",
                  "Please check spelling of column header in csv file."))
    }
    valid_artificial_segments <- valid_artificial_segments$COMID
  }

  # return all the dataframes in a list
  return( list(segment_data_original = segment_data_original,
               gages = gages,
               flow_ids = flow_ids,
               valid_artificial_segments = valid_artificial_segments) )
}

preprocess_data <- function(segment_data_original,
                            gages,
                            flow_ids,
                            valid_artifical_segments) {

  ##################################################################
  ###################### Subset the segment data ###################
  ##################################################################

  # This will create a new dataframe with all the data from both the
  # segment data and the flow_ids, which contains To and From ComIDs.
  # This new dataframe will be longer than segment data, because it
  # has multiple rows for segments with multiple downstream segments
  segment_to_from_COMIDs <- base::merge(segment_data_original,
                                        flow_ids,
                                        by.x = "COMID",
                                        by.y = "FROMCOMID",
                                        all.x = T,
                                        all.y = F)

  # make segment data just be the original data without removing duplicates
  segment_data <- segment_data_original

  # remove the rows in the segment_to_from_COMIDs dataframe that have
  # missing ToCOMIDs and onbly keep the necessary columns, namely
  # COMID and TOCOMID
  to_COMIDs <- segment_to_from_COMIDs$TOCOMID
  from_COMIDs <- segment_to_from_COMIDs$COMID
  val_TOCOMIDs_mask <- !is.na(to_COMIDs)
  segment_to_from_COMIDs <- segment_to_from_COMIDs[val_TOCOMIDs_mask,
                                                   c("TOCOMID", "COMID")]

  # Remove all rows where the TOCOMID & COMID are 0. This happens when
  # a segment ends (or begins), either at the ocean, a lake or just
  # from man made reasons.
  segment_to_from_COMIDs <- segment_to_from_COMIDs[to_COMIDs != 0 &
                                                   from_COMIDs != 0, ]
#  segment_to_from_COMIDs <- segment_to_from_COMIDs[from_COMIDs != 0, ]

  #######################################################################
  ############## Create a Subset for just the USGS gage stations ########
  #######################################################################

  # subset the segment data finding only those streams with gages gage data
  # NOTE: THE SHAPEFILE DRIVER TRUNCATED THE COLUMN NAME from NHDV2_COMID
  # to NHDV2_COMI
  segment_data_gage <- segment_data[segment_data$COMID %in% gages, ]

  # Only keep segments with gage stations in them that are valid nodes,
  # which include 'StreamRiver' segments and artifical segments that
  # are deemed valid
  valid_segment_idxs <- is_valid_node(segment_data_gage$COMID,
                                      segment_data_gage$FTYPE,
                                      valid_artifical_segments)
  segment_data_gage <- segment_data_gage[valid_segment_idxs, ]

  # make sure that all the segments with gage stations have unique COMIDs
  assertthat::are_equal(unique(segment_data_gage[, "COMID"]),
                        nrow(segment_data_gage))

  # return the preprocessed data
  return(list(segment_data = segment_data,
              segment_data_gage = segment_data_gage,
              segment_to_from_COMIDs = segment_to_from_COMIDs))
}
