# Calculate Kinetogram summary stats for 1 or 2 treatments.
# If 2 treamtents' data is provided, then calculate overall (Wilcox test) p-values.

ana_to_kinetogram_df <- function(ana_df, alpha=0.05, p_adjust_method = 'bonferroni') {

  treatments <- unique(ana_df$treatment)

  anaDatList <- split(ana_df, ana_df$filename)

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

    return(data.frame(filename=unique(df$filename), transitions))
  }

  getDurations <- function(df) {
    # Combine E1/E1e
    df$WaveformLabel[df$WaveformLabel == 'E1e'] <- 'E1'
    # Combine all "pd" phases
    df$WaveformLabel[df$WaveformLabel %in% c('pd-S', 'pd-S-II-2', 'pd-S-II-3', 'pd-L', 'pd-L-II-2', 'pd-L-II-3')] <- 'pd'

    # drop last row (assumes we have QAQC'ed already using 'read_ana' function)
    df <- df[-nrow(df), ]
    endTime <- df$Time[nrow(df)]
    durations <- data.frame(acronym=df$WaveformLabel, start=df$Time, end=c(df$Time[2:(length(df$Time))], NA))
    durations <- durations[-nrow(durations), ] # drop last row
    durations$duration <- durations$end - durations$start
    durations <- durations %>%
      dplyr::group_by(acronym) %>%
      dplyr::summarise(duration_pct = 100 * sum(duration) / endTime, .groups = 'keep') %>%
      as.data.frame()
    durations$filename <- unique(df$filename)

    return(durations)
  }

  # --- Compute Phase Durations and Transition Frequencies
  transitions <- lapply(anaDatList, function(df) compute_transitions(df))
  transitions <- do.call(rbind, transitions)
  transitions <- tidyr::pivot_wider(transitions, names_from = 'transition', values_from = 'n')
  transitions$totalTransitions <- transitions %>%
    select(-one_of('filename')) %>%
    rowSums(na.rm = TRUE)
  durations <- lapply(anaDatList, function(df) getDurations(df))
  durations <- do.call(rbind, durations)
  durations <- tidyr::pivot_wider(durations, names_from = 'acronym', values_from = 'duration_pct')

  dat <- merge(transitions, durations, by = 'filename')
  dat <- merge(dat, unique(ana_df[,c('filename', 'treatment')]), by = 'filename')
  dat[is.na(dat)] <- 0 # replace NA with zero so we can calculate summary stats & pvalues

  # --- Compute Summary Stats

  sumstat_mean <- dat %>%
    group_by(treatment) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))

  # Get transitions as percent of total transitions (for arrow widths in kinetogram)
  transCols <- grep(',', colnames(sumstat_mean), value = TRUE)
  arrowWidths <- sumstat_mean[ , c('treatment', transCols, 'totalTransitions')]
  for(row in 1:nrow(arrowWidths)) {
    arrowWidths[row , transCols] <- arrowWidths[row , transCols] / arrowWidths$totalTransitions
  }
  arrowWidths$totalTransitions <- NULL
  # Convert from percentage to arrow width
  arrowWidths[ , transCols] <- arrowWidths[ , transCols] * 30

  # Convert to wide
  sumstat_mean <- sumstat_mean %>%
    select(-one_of('totalTransitions')) %>%
    pivot_longer(cols = -one_of('treatment'), names_to = 'parameter', values_to = 'mean')

  std_error <- function(x) {
    x <- x[!is.na(x)]
    sd(x) / sqrt(length(x))
  }

  sumstat_se <- dat %>%
    select(-one_of('totalTransitions')) %>%
    group_by(treatment) %>%
    summarise(across(where(is.numeric), std_error)) %>%
    pivot_longer(cols = -one_of('treatment'), names_to = 'parameter', values_to = 'se')

  # Combine mean, se tables
  sumstat <- merge(sumstat_mean, sumstat_se, by = c('treatment', 'parameter'), all = TRUE)

  # Merge arrow width data to summary stats
  arrowWidths <- pivot_longer(arrowWidths, cols = -one_of('treatment'), names_to = 'parameter', values_to = 'arrowWidth')
  sumstat <- merge(sumstat, arrowWidths, by = c('treatment', 'parameter'), all = TRUE)


  # --- Run Wilcox Tests To Find Significant Differences Between 2 Treatments

  # If 2 treatments are chosen, calculate overall Wilcox test p-values
  if(length(treatments) == 2) {

    paramCols <- setdiff(colnames(dat), c('filename', 'treatment'))

    wilcoxList <- lapply(paramCols, function(pcol) {
      tryCatch( {
        x <- dat[[pcol]][dat$treatment == treatments[1]]
        y <- dat[[pcol]][dat$treatment == treatments[2]]
        pval <- wilcox.test(x, y, na.action = 'na.omit')$p.value
        out <- data.frame(trt1 = treatments[1], trt2 = treatments[2], pvalue=pval, parameter = pcol)

        # pOverall <- kruskal.test(dat[[pcol]], dat$treatment, na.action = 'na.omit')$p.value
        # out <- data.frame(trt1 = 'overall', trt2 = 'overall', pvalue=pOverall)
        # out$parameter <- pcol
        return(out)
      },
      # warning = function(w) {
      #   return(w)
      # },
      error = function(e) {
        return(NULL)
      })
    })

    wilcoxDat <- do.call(rbind, wilcoxList)

  }

  return(list(summaryStats = sumstat, pValues = wilcoxDat))

}
