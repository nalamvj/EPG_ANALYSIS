
compute_summary_stats <- function(parameter_df,
                                  parameter_metadata,
                                  time_units=NULL,
                                  treatments=NULL,
                                  acronyms=NULL,
                                  alpha = 0.05,
                                  p_adjust_method='bonferroni') {

  colnames(parameter_df) <- tolower(colnames(parameter_df))

  if(!is.null(acronyms)) {
    parameter_df <- subset(parameter_df, parameter_df$acronym %in% acronyms)
  }
  if(!is.null(treatments)) {
    parameter_df <- subset(parameter_df, parameter_df$treatment %in% treatments)
  } else { # if treatment argument not passed in, use all treatments data
    treatments <- unique(parameter_df$treatment)
  }

  parameter_df <- parameter_df %>%
    select(-one_of('description')) %>%
    pivot_wider(values_from = 'value', names_from = 'acronym')

  paramCols <- setdiff(colnames(parameter_df), c('filename', 'treatment'))

  #TODO: "funs() was deprecated in dplyr 0.8.0"
  # Summary Stats (long format, for table display)

  sumstat_mean <- parameter_df %>%
    group_by(treatment) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = -one_of('treatment'), names_to = 'acronym', values_to = 'mean')

  std_error <- function(x) {
    x <- x[!is.na(x)]
    sd(x) / sqrt(length(x))
  }

  sumstat_se <- parameter_df %>%
    group_by(treatment) %>%
    summarise(across(where(is.numeric), std_error)) %>%
    pivot_longer(cols = -one_of('treatment'), names_to = 'acronym', values_to = 'se')

  # Combine mean, se tables
  sumstat <- merge(sumstat_mean, sumstat_se, by = c('treatment', 'acronym'))

  # Merge to parameter metadata (csv file) to get units and parameter groups ("experiment")
  summaryStats <- merge(sumstat,
                        select(parameter_metadata, 'acronym','unit','experiment'),
                        by = 'acronym')

  if(!(is.null(time_units) || time_units == 'seconds')) {
    # convert time-based parameters from seconds into minutes or hours, based on user input
    secIdx <- summaryStats$unit == 'seconds'
    if(time_units == 'minutes') {
      summaryStats$mean[secIdx] <- summaryStats$mean[secIdx] / 60
      summaryStats$se[secIdx] <- summaryStats$se[secIdx] / 60
      summaryStats$unit[secIdx] <- 'minutes'
    } else if(time_units == 'hours') {
      summaryStats$mean[secIdx] <- summaryStats$mean[secIdx] / 3600
      summaryStats$se[secIdx] <- summaryStats$se[secIdx] / 3600
      summaryStats$unit[secIdx] <- 'hours'
    } else stop("'time_units' argument must be one of either 'seconds', 'minutes' or 'hours'")
  }

  # Combine mean and se into 1 column
  summaryStats$mean.se <- paste(round(summaryStats$mean, 1), round(summaryStats$se, 1), sep = ' \u00B1 ')

  # Convert to wide format
  summaryStats <- summaryStats %>%
    select('experiment','acronym','unit','treatment','mean.se') %>%
    pivot_wider(names_from = 'treatment', values_from = 'mean.se', names_prefix = 'mean.se^^')


  # If at least 2 treatments are chosen, calculate pairwise wilcox test and p-values
  if(length(treatments) > 1) {

    dunnList <- lapply(paramCols, function(pcol) {
      tryCatch( {
        if(length(treatments) > 2) { # 3 or more treatments
          # Do Kruskal-Wallis overall test, and then pairwise-comparisons if p < alpha
          pOverall <- kruskal.test(parameter_df[[pcol]], parameter_df$treatment, na.action = 'na.omit')$p.value
          out <- data.frame(trt1 = 'overall', trt2 = 'overall', pvalue=pOverall)
          # If overall test is significant and there are > 2 treatments, do pairwise Dunn tests
          if(pOverall < alpha) {
            dunnResults <- dunn.test::dunn.test(parameter_df[[pcol]], parameter_df$treatment, method = p_adjust_method)
            dunnResults <- data.frame(comparison = dunnResults$comparisons, pvalue = dunnResults$P.adjusted)
            x <- strsplit(dunnResults$comparison, ' - ', fixed = TRUE)
            dunnResults$trt1 <- sapply(x, function(y) y[1])
            dunnResults$trt2 <- sapply(x, function(y) y[2])
            dunnResults$comparison <- NULL
            out <- rbind(out, dunnResults)
          }
        } else { # only 2 treatments
          x <- parameter_df[[pcol]][parameter_df$treatment == treatments[1]]
          y <- parameter_df[[pcol]][parameter_df$treatment == treatments[2]]
          pval <- wilcox.test(x, y, na.action = 'na.omit')$p.value
          out <- data.frame(trt1 = treatments[1], trt2 = treatments[2], pvalue=pval)
        }

        out$acronym <- pcol
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
    dunnDat$pvalue <- round(dunnDat$pvalue, 2)
    dunnDat$trtCombo <- with(dunnDat, paste0('dunn_pval^^', paste(trt1, trt2, sep = ', ')))
    dunnDat$trtCombo[dunnDat$trtCombo == 'dunn_pval^^overall, overall'] <- 'dunn_pval^^overall'
    dunnDatWide <- dunnDat %>%
      select(-one_of('trt1','trt2')) %>%
      pivot_wider(id_cols = 'acronym', names_from = 'trtCombo', values_from = 'pvalue')

    # Merge to pairwise Wilcoxan test
    summaryStats <- merge(summaryStats, dunnDatWide, by = 'acronym')
  }

  return(summaryStats)

}

