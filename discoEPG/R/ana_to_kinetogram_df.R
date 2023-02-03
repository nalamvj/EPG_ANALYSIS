ana_to_kinetogram_df <- function(anaDatList, trtKey, treatments=NULL, alpha=0.05, p_adjust_method = 'bonferroni') {

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
  transitions <- pivot_wider(transitions, names_from = 'transition', values_from = 'n')
  transitions$totalTransitions <- transitions %>%
    select(-one_of('filename')) %>%
    rowSums(na.rm = TRUE)
  durations <- lapply(anaDatList, function(df) getDurations(df))
  durations <- do.call(rbind, durations)
  durations <- pivot_wider(durations, names_from = 'acronym', values_from = 'duration_pct')

  dat <- merge(transitions, durations, by = 'filename')
  dat <- merge(dat, trtKey, by = 'filename')

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


  # --- Run Dunn Tests To Find Significant Differences Between Treatments (Overall and Pairwise)

  # If at least 2 treatments are chosen, calculate pairwise wilcox test and p-values
  if(is.null(treatments) || length(treatments) > 1) {

    paramCols <- setdiff(colnames(dat), c('filename', 'treatment'))

    dunnList <- lapply(paramCols, function(pcol) {
      tryCatch( {
        # Do Kruskal-Wallis overall test, and then pairwise-comparisons if p < alpha
        pOverall <- kruskal.test(dat[[pcol]], dat$treatment, na.action = 'na.omit')$p.value
        out <- data.frame(trt1 = 'overall', trt2 = 'overall', pvalue=pOverall)
        if(pOverall < alpha & length(treatments) > 2) {
          dunnResults <- dunn.test::dunn.test(dat[[pcol]], dat$treatment, method = p_adjust_method)
          dunnResults <- data.frame(comparison = dunnResults$comparisons, pvalue = dunnResults$P.adjusted)
          x <- strsplit(dunnResults$comparison, ' - ', fixed = TRUE)
          dunnResults$trt1 <- sapply(x, function(y) y[1])
          dunnResults$trt2 <- sapply(x, function(y) y[2])
          dunnResults$comparison <- NULL
          out <- rbind(out, dunnResults)
        }
        out$parameter <- pcol
        return(out)
      },
      # warning = function(w) {
      #   return(w)
      # },
      error = function(e) {
        return(NULL)
      })
    })

    dunnDat <- do.call(rbind, dunnList)

  }

  return(list(summaryStats = sumstat, pValues = dunnDat))

}
