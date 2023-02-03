
ana_to_parameters <- function(ana_df, waveform_labels) {
  byCols <- if('treatment' %in% colnames(ana_df)) c('filename', 'treatment') else 'filename'
  paramDat <- plyr::ddply(ana_df, byCols, function(df) {
    df <- compute_parameters(EPG_analysis=df, waveform_labels=waveform_labels)
    df$acronym <- rownames(df)
    return(df)
  })
  # TODO: Not sure why but Beery's latest function doesn't convert value to numeric
  paramDat$Value <- as.numeric(paramDat$Value)
  return(paramDat)
}
