library(rgdal)
library(foreign)

# define the default filenames to be used in the internal data
DEFAULT_LAYER_NAME = 'Default_NHDFlowline'
DEFAULT_GAGES_FILENAME <- "Default_CA_gages.csv"
DEFAULT_FLOW_IDS_FILENAME = "Default_flow_IDs.dbf"
DEFAULT_VALID_ARTIFICIAL_SEGMENTS_FILENAME = 'Default_valid_artificial_segments.csv'

# Specify whether to overwrite the current system.rda file. Defaults to false to prevent accidents
overwrite_current=FALSE

# call the preprocessData function with the default data files
raw_data <- StrmAnlyzeR::loadData(shapefile_folder=DEFAULT_LAYER_NAME,
                    gages_filename = DEFAULT_GAGES_FILENAME,
                    flow_ids_filename=DEFAULT_FLOW_IDS_FILENAME,
                    valid_artificial_segments_filename=DEFAULT_VALID_ARTIFICIAL_SEGMENTS_FILENAME)

# store the raw data loaded into the default names for storage
default_segment_data_original <- raw_data$segment_data_original
default_gages <- raw_data$gages
default_flow_ids <- raw_data$flow_ids
default_valid_artificial_segments <- raw_data$valid_artificial_segments

# Now preprocess the data using the data structures we just loaded
preproc_data <- StrmAnlyzeR::preprocessData(raw_data$segment_data,
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

