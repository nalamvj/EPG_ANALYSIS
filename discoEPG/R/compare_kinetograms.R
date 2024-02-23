# Use the 'make_kinetogram' function to make 2 kinetograms, 1 per treatment
# Accepts the output of the function "ana_to_kinetograms" (if used with 2 treatments only!)
# 'alpha' is the significance level
compare_kinetograms <- function(kineDat, alpha=0.05) {

  summaryStats <- kineDat$summaryStats
  summaryStats <- subset(summaryStats, parameter != 'totalTransitions')
  summaryStats$rownum <- 1:nrow(summaryStats)
  if(length(unique(summaryStats$treatment)) != 2) stop("Function requires there be data from 2 treatments!")

  summaryStats$nodeColor <- 'black'
  summaryStats$edgeColor <- 'black'

  # For any significant p-values, change either edge (transitions) or node (phase) color from default black to red/.
  pValues <- kineDat$pValues
  pValues <- subset(pValues, parameter != 'totalTransitions')
  sigP <- subset(pValues, pvalue <= alpha)

  if(nrow(sigP) > 0) {
    for (i in 1:nrow(sigP)) {
      x <- summaryStats[which(summaryStats$parameter == sigP$parameter[i]),]
      x <- x[which(x$mean == max(x$mean)), ]
      if(grepl(',', x$parameter)) {
        summaryStats$edgeColor[x$rownum] <- 'red'
      } else {
        summaryStats$nodeColor[x$rownum] <- 'red'
      }
    }
  }

  # Split list and apply function
  kgrams <- lapply(split(summaryStats, summaryStats$treatment), make_kinetogram)
  return(kgrams)
}
