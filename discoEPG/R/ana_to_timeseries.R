
ana_to_timeseries <- function(ana_df, waveform_labels) {

  ana_df_list <- split(ana_df, ana_df$filename)

  # Check that all experiments are of equal duration
  maxTimes <- sapply(ana_df_list, function(df) max(df$Time))
  if(length(unique(maxTimes)) > 1) stop(".ANA files differ in experiment durations!")

  expandDatList <- lapply(ana_df_list, function(df) {

    dfExpand <- data.frame(Time = 0:max(df$Time), Waveform = integer(length = length(0:max(df$Time))))
    for(i in 1:(nrow(df)-1)) {
      ind <- dfExpand$Time >= df$Time[i] & dfExpand$Time < df$Time[i+1]
      dfExpand$Waveform[ind] <- df$Waveform[i]
    }
    # fill in last row with '12' (end)
    dfExpand$Waveform[nrow(dfExpand)] <- 12
    # drop time column as they're identical
    return(dfExpand$Waveform)
  })

  # Matrix of waveforms for each second. Rows=seconds, Columns=files.
  expandMat <- do.call(cbind, expandDatList)

  # Combine E1/E1e (relabel as waveform = 999)
  idx_E <- waveform_labels[names(waveform_labels) %in% c('E1', 'E1e')]
  expandMat[expandMat %in% idx_E] <- 888
  # Combine all "pd" phases (relabel as waveform = 999)
  idx_pd <- waveform_labels[names(waveform_labels) %in% c('pd-S', 'pd-S-II-2', 'pd-S-II-3', 'pd-L', 'pd-L-II-2', 'pd-L-II-3')]
  expandMat[expandMat %in% idx_pd] <- 999

  # Rebuild waveform_labels
  waveform_labels <- waveform_labels[!names(waveform_labels) %in% c('E1', 'E1e', 'pd-S', 'pd-S-II-2', 'pd-S-II-3', 'pd-L', 'pd-L-II-2', 'pd-L-II-3')]
  waveform_labels <- c(waveform_labels, c('E1' = 888, 'pd' = 999))

  # Function returns the mode(s) of a vector
  getMultiModes <- function(vector) {
    freqs <- tabulate(vector)
    return(which(freqs == max(freqs)))
  }
  multiModes <- apply(expandMat, 1, getMultiModes)

  mmMatrix <- matrix(nrow = length(multiModes), ncol = max(sapply(multiModes, length)))
  for(i in 1:length(multiModes)) {
    mmMatrix[i, 1:length(multiModes[[i]])] <- multiModes[[i]]
  }
  mmMatrix <- data.frame(time=1:nrow(mmMatrix), mmMatrix)
  modeDat <- mmMatrix %>%
    pivot_longer(cols = -one_of('time'), names_to = NULL, values_to = 'waveform_mode', values_drop_na = TRUE)

  # Use waveform_labels to add 'activity' columnm
  modeDat$activity <- as.factor(names(waveform_labels)[match(modeDat$waveform_mode, waveform_labels)])

  # re-order factor, in same order as "waveform_labels"
  modeDat$activity <- factor(toupper(modeDat$activity),
                             levels = toupper(intersect(names(waveform_labels), unique(modeDat$activity))))
  # remove 'end' for plotting
  ts_plot <- ggplot(subset(modeDat, modeDat$activity != 'END'), aes(x=activity, y=time, color=activity)) +
    geom_point(shape=20, size=1) +
    theme_classic() +
    coord_flip()

  return(list(data=modeDat, plot=ts_plot))

}
