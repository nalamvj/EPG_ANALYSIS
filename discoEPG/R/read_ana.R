#' Read a single '.ANA' file.
#'
#' @param filepath String; path to .ANA file.
#' @param waveform_labels Named vector; values are waveform numbers and names are waveform labels.
#' @param validate Logical; If TRUE (defualt), performs data validation.
#' @param filename_shiny String; Special argument only used in shiny app. Leave as default (NULL).
#' @return List of 2: 'data' with the labelled dataframe, and 'errors' a string with validation errors (or NULL if no errors).
#' @examples
#' waveform_labels <- c(
#'"C" = 2,
#'"E1e" = 3,
#'"E1" = 4,
#'"E2" = 5,
#'"F" = 6,
#'"G" = 7,
#'"pd-S" = 8,
#'"pd-S-II-2" = 9,
#'"pd-S-II-3" = 10,
#'"pd-L" = 11,
#'"pd-L-II-2" = 13,
#'"pd-L-II-3" = 14,
#'"end" = 12)
#'
#' ana_out <- read_ana(filepath="/Users/c/Documents/EPG Project/Info for Clay/Grids/1-15-2018 day-ch7_VJN.ANA",
#'  waveform_labels=waveform_labels)
#'
#'head(ana_out$data)
#'cat(ana_out$errors)
read_ana <- function(filepath, waveform_labels=NULL, validate=TRUE, filename_shiny=NULL) {
  df <- tryCatch(
    read.delim(filepath, header = FALSE, fileEncoding = "UTF-16LE", col.names = c("Waveform","Time","mV")),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  # Add waveform label column to df
  df$WaveformLabel <- names(waveform_labels)[match(df$Waveform, waveform_labels)]

  # Data validation
  # If not called within Shiny, we can extract the filename from the filepath
  if(is.null(filename_shiny)) {
    idx <- unlist(gregexpr("\\/", filepath))
    idx <- idx[length(idx)] + 1
    filename <- substr(filepath, idx, nchar(filepath))
  } else {
    filename <- filename_shiny
  }
  errors <- if(is.null(df)) paste0("Error reading file '", filename, "'. \n") else NULL
  if(validate & !is.null(df)) {
    if(is.null(waveform_labels)) {
      stop("'waveform_labels' must be provided for data validation to occur.")
    } else {
      if(!is.integer(df$Waveform)) {
        errors <- paste0(errors, "Waveform column in file '", filename, "' contains non-integer values. \n")
      }
      if(any(is.na(df$Waveform))) {
        errors <- paste0(errors, "Waveform column in file '", filename, "' contains NA values. \n")
      }
      if(any(is.na(df$WaveformLabel))) {
        errors <- paste0(errors, "Waveform column in file '", filename, "' contains invalid waveform numbers. \n")
      }
      lastWaveformLabel <- df$WaveformLabel[nrow(df)]
      if(is.na(lastWaveformLabel) || lastWaveformLabel != 'end') {
        errors <- paste0(errors, "No end of EPG data in file '", filename, "'. \n")
      }
      if(!all(rle(df$Waveform)$lengths == 1)) {
        errors <- paste0(errors, "Same wave followed in file '", filename, "'. \n")
      }
      E1idx <- which(df$WaveformLabel %in% c('E1', 'E1e'))[1]
      E2idx <- which(df$WaveformLabel== 'E2')[1]
      if(!is.na(E2idx) & (is.na(E1idx) || E1idx > E2idx)) {
        errors <- paste0(errors, "No E1 before E2 in file '", filename, "', \n")
      }
      if(!is.numeric(df$Time)) {
        errors <- paste0(errors, "Time column in file '", filename, "' contains non-numeric values. \n")
      }
      if(any(is.na(df$Time))) {
        errors <- paste0(errors, "Time column in file '", filename, "' contains NA values. \n")
      }
      if(df$Time[1] != 0) {
        errors <- paste0(errors, "No beginning of probe in file '", filename, "' \n")
      }
      # Make sure Time is strictly increasing
      if(any(diff(df$Time) <= 0)) {
        errors <- paste0(errors, "Time column in file '", filename, "' is not strictly increasing. \n")
      }
      # Compute transitions and check for invalid/rare transitions
      trans <- compute_transitions(df)
      validTrans <- c('C,E1', 'C,F', 'C,G', 'C,np', 'C,pd',
                      'E1,C', 'E1,E2', 'E2,C', 'E2,E1', 'F,C', 'G,C', 'np,C', 'pd,C')
      invalidTrans <- subset(trans, !transition %in% validTrans)
      invalidTrans$transition <- gsub(',', ' -> ', invalidTrans$transition)
      if(nrow(invalidTrans) > 0) {
        errors <- paste0(errors, "Invalid or rare transitions found in file '", filename, "': ",
                         paste(invalidTrans$transition, collapse = '; '), "\n")
      }

    }
  }
  return(list(data=df, errors=errors))
}
