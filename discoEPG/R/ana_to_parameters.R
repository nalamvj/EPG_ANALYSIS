
ana_to_parameters <- function(ana_df, waveform_labels) {

  param_dat <- split(ana_df, ana_df$filename)
  param_dat <- lapply(param_dat, function(x) {
    pdat <- compute_parameters(x, waveform_labels)
    pdat$filename <- unique(x$filename)
    pdat$acronym <- rownames(pdat)
    rownames(pdat) <- NULL
    return(pdat)
  })
  param_dat <- do.call(rbind, param_dat)
  param_dat$Value <- as.numeric(param_dat$Value)
  colnames(param_dat) <- tolower(colnames(param_dat))
  rownames(param_dat) <- NULL
  return(param_dat)
}
