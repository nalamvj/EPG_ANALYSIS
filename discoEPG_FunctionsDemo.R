# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ----- R package "discoEPG" -- Function Demonstration Code
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# clear the environment of all objects (beware!)
# rm(list = ls())

# install discoEPG package
if(!require(remotes)) install.packages('remotes')
remotes::install_github("nalamvj/EPG_ANALYSIS/discoEPG",
                        auth_token = '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',
                        force = TRUE)

# load package
require(discoEPG)

# load parameter metadata
paramMetadata <- read.csv("/Users/c/Documents/EPG Project/EPGShinyApp/Deployed/ParameterMetadata.csv")
colnames(paramMetadata) <- tolower(colnames(paramMetadata))
paramMetadata$acronym <- gsub('\\*\\d+$', '', paramMetadata$acronym) # remove asterices

# create waveform labels
waveform_labels = c("np" = 1,
                    "C" = 2,
                    "E1e" = 3,
                    "E1" = 4,
                    "E2" = 5,
                    "F" = 6,
                    "G" = 7,
                    "pd-S" = 8,
                    "pd-S-II-2" = 9,
                    "pd-S-II-3" = 10,
                    "pd-L" = 11,
                    "pd-L-II-2" = 13,
                    "pd-L-II-3" = 14,
                    "end" = 12)


# ------------------------------------------------------------------------------
# ----- Function 1: read_ana -- Read 1 ANA file.
# ------------------------------------------------------------------------------

# call 'read_ana' function and store results in a list object
ana_out <- read_ana(filepath = "/Users/c/Documents/EPG Project/Info for Clay/Grids/1-15-2018 day-ch7_VJN.ANA",
                    validate = TRUE, # perform data validation
                    waveform_labels = waveform_labels)

# the list object has 2 items: 'data' and 'errors'
# data frame with labelled waveforms
head(ana_out$data)
# validation errors (character; or NULL if there were no validation errors)
ana_out$errors

# ------------------------------------------------------------------------------
# ----- Function 2: combine_ana -- Read multiple ANA files and combine.
# ------------------------------------------------------------------------------

# folder (directory) for .ANA files 
ana_dir <- "/Users/c/Documents/EPG Project/Info for Clay/Grids/"

# character vector of all .ANA files in above folder
filenames <- list.files(path = ana_dir, pattern = ".ANA")

# read all .ANA files in directory into a single dataframe
anaDatList <- combine_ana(directory = ana_dir,
                          filenames = filenames,
                          validate = TRUE, # perform data validation
                          waveform_labels = waveform_labels)

# extract data
anaData <- anaDatList$data

# extract and print validation errors
cat(anaDatList$errors)

# TODO: Refactor combine_ana to give a df with errors and filenames, use this to filter data
# subset ANA data list to include only those ANA files with no validation errors
# datList <- datList[-errorInd]



# ------------------------------------------------------------------------------
# ----- Function 3: ana_to_parameters -- Convert all ANA data into parameters.
# ------------------------------------------------------------------------------

# pass the above data frame to this function and return computed parameter values in a data frame
paramData <- ana_to_parameters(ana_df = anaData, waveform_labels = waveform_labels)


# ------------------------------------------------------------------------------
# ----- FUNCTION 4: compute_summary_stats -- Compute summary statistics, by treatment.
# ------------------------------------------------------------------------------

# NOTE: We must assign treatments to 'paramData' df before computing summary statistics.
# (This is normally done in Shiny app via an interactive table)

# initialize 'treatment' column
paramData$treatment <- NULL

# get all filenames in master data frame
allfiles <- unique(paramData$filename)

# assign treatments for files with names containing 'day' or 'night'
dayfiles <- grep('day', allfiles, value = TRUE)
nightfiles <- grep('night', allfiles, value = TRUE)
paramData$treatment[paramData$filename %in% dayfiles] <- 'day'
paramData$treatment[paramData$filename %in% nightfiles] <- 'night'

# remove data from files not matching 'day' or 'night' in file name
paramData <- subset(paramData, filename %in% c(dayfiles, nightfiles))

# confirm all rows have treatment assigned
table(paramData$treatment, useNA = 'ifany')

# finally, call the function to compute summary statistics
sumStats <- compute_summary_stats(parameter_df = paramData,
                                  parameter_metadata = paramMetadata,
                                  time_units='minutes',
                                  treatments=NULL,
                                  acronyms=NULL,
                                  alpha = 0.05,
                                  p_adjust_method='bonferroni')

# ------------------------------------------------------------------------------
# ----- FUNCTION 5: ana_to_timeseries
# ------------------------------------------------------------------------------

anaData$treatment <- NULL
anaData$treatment[anaData$filename %in% dayfiles] <- 'day'
anaData$treatment[anaData$filename %in% nightfiles] <- 'night'
# remove data from files not matching 'day' or 'night' in file name
anaData <- subset(anaData, filename %in% c(dayfiles, nightfiles))

# confirm all rows have treatment assigned
table(anaData$treatment, useNA = 'ifany')

# subset to data in a single treament (one treament per kinetogram)
day_data <- subset(anaData, treatment == 'day')

# run timeseries function and return a list with data and plot
ts_out <- ana_to_timeseries(ana_df = day_data, waveform_labels = waveform_labels)

# extract data -- mode(s) of most common waveform(s) for each second
ts_dat <- ts_out$data

# show plot
ts_out$plot


# ------------------------------------------------------------------------------
# ----- FUNCTION 6: ana_to_kinetogram_df
# ------------------------------------------------------------------------------


# --------- Function  to Calculate Transitions and % Time Spent in each Activity ===========

ana_df <- anaData
kineDat <- ana_to_kinetogram_df(ana_df = ana_df)

## ---- Function to convert data for a single treatment to kinetograph
kgramSumStats <- kineDat$summaryStats

# ------------------------------------------------------------------------------
# ----- FUNCTION 7: make_kinetogram
# ------------------------------------------------------------------------------

# --------- Function to Plot a Single Kinetogram (1 treatment) ===========

# Note, this is only plotting 1 kinetogram/treatment, and not showing any statistical differences.
make_kinetogram(kinetogramSummaryStats = subset(kgramSumStats, treatment == 'day'))
make_kinetogram(kinetogramSummaryStats = subset(kgramSumStats, treatment == 'night'))


# ------------------------------------------------------------------------------
# ----- FUNCTION 7: compare_kinetograms
# ------------------------------------------------------------------------------

# --------- Function to Plot Return 2 kinetograms (1 per treatment) and show Statistical Differences ===========

# note, edges/nodes that are statistically different are shown in RED color.
# the color is applied to the GREATER-valued parameter of the 2 treatments.

# Returns a list of kinetograms, named by treatment
kgrams <- compare_kinetograms(kineDat, alpha = 0.05)

# show kinetograms 
kgrams$day
kgrams$night
