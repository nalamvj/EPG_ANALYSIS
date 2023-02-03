
compute_transitions <- function(df) {
  # drop last row (assumes we have QAQC'ed already using 'read_ana' function)
  df <- df[-nrow(df), ]

  # Combine E1/E1e
  df$WaveformLabel[df$WaveformLabel == 'E1e'] <- 'E1'
  # Combine all "pd" phases
  df$WaveformLabel[df$WaveformLabel %in% c('pd-S', 'pd-S-II-2', 'pd-S-II-3', 'pd-L', 'pd-L-II-2', 'pd-L-II-3')] <- 'pd'

  # Tabulate transitions between activities
  transitions <- paste(df$WaveformLabel[1:(nrow(df)-1)], df$WaveformLabel[2:length(df$WaveformLabel)], sep = ',')
  transitions <- data.frame(table(transitions))
  colnames(transitions) <- c('transition', 'n')

  return(transitions)
}
