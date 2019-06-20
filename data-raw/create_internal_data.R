library(rgdal)
library(foreign)

# source the fileIO file which contains the load_data and preprocess_data functions
source("R/fileIO.R")

# Specify whether to overwrite the current system.rda file. Defaults to false to prevent accidents
overwrite_current=TRUE

# define the default filenames to be used in the internal data
DEFAULT_NHDFLOWLINE_FOLDER = 'Default_NHDFlowline'
DEFAULT_GAGES_FILENAME <- "Default_gages.csv"
DEFAULT_to_from_COMIDs_filename = "Default_to_from_COMIDs.dbf"
DEFAULT_VALID_ARTIFICIAL_SEGMENTS_FILENAME = 'Default_valid_artificial_segments.csv'

# call the preprocessData function with the default data files
raw_data <- load_data(NHDFlowline_folder = DEFAULT_NHDFLOWLINE_FOLDER,
                      gages_filename = DEFAULT_GAGES_FILENAME,
                      to_from_COMIDs_filename = DEFAULT_to_from_COMIDs_filename,
                      val_artificial_segs_filename = DEFAULT_VALID_ARTIFICIAL_SEGMENTS_FILENAME)

# store the raw data loaded into the default names for storage
default_segment_data_original <- raw_data$segment_data_original
default_gages <- raw_data$gages
default_flow_ids <- raw_data$flow_ids
default_valid_artificial_segments <- raw_data$valid_artificial_segments

# Now preprocess the data using the data structures we just loaded
preproc_data <- preprocess_data(raw_data$segment_data,
                                raw_data$gages,
                                raw_data$flow_ids,
                                raw_data$valid_artificial_segments)

# store the preprocessed data loaded into the default names for storage
default_segment_data <- preproc_data$segment_data
default_segment_data_gage <- preproc_data$segment_data_gage
default_segment_to_from_COMIDs <- preproc_data$segment_to_from_COMIDs

# save all the default dataframes to a file called system.rda in the R folder
devtools::use_data(default_segment_data_original,
                   default_gages,
                   default_flow_ids,
                   default_valid_artificial_segments,
                   default_segment_data,
                   default_segment_data_gage,
                   default_segment_to_from_COMIDs,
                   internal = TRUE,
                   overwrite = overwrite_current)

