
# Make a single kinetogram of a single treatment
make_kinetogram <- function(kinetogramSummaryStats) {

  kdat <- kinetogramSummaryStats
  if(!('edgeColor' %in% colnames(kdat))) kdat$edgeColor <- 'black'
  if(!('nodeColor' %in% colnames(kdat))) kdat$nodeColor <- 'black'

  kdat$parameter <- gsub('np', 'NP', kdat$parameter)


  # Duration in each phase, as percentages: Used to determine node sizes.
  durations <- dplyr::select(kdat, 'parameter', 'mean')
  durations <- durations[!grepl(',', durations$parameter), ]
  durations$mean <- round(durations$mean, 0)
  # if(any(!grepl(',', pVals$parameter))) {
  #   durations <- merge(durations,
  #                      pVals[!grepl(',', pVals$parameter), c('parameter', 'pvalue')],
  #                      by = 'parameter', all = TRUE)
  # }

  # Fill any missing durations
  durationsAll <- data.frame(parameter = c('C','E1','E2','F','G','NP','pd'), mean=0)
  durations <- rbind(durations, subset(durationsAll, !parameter %in% durations$parameter))
  durations <- durations[order(durations$parameter), ]

  # Get Edge Widths and Colors data
  edgeSpecs <- dplyr::select(kdat, 'parameter', 'arrowWidth', 'edgeColor')
  edgeSpecs <- edgeSpecs[grepl(',', edgeSpecs$parameter), ]

  # omit edgewidths not present
  okWidths <- c('C,E1', 'C,F', 'C,G', 'C,NP', 'C,pd',
                'E1,C', 'E1,E2', 'E2,C', 'E2,E1', 'F,C', 'G,C', 'NP,C', 'pd,C')
  edgeSpecs <- subset(edgeSpecs, parameter %in% okWidths)
  allWidths <- data.frame(parameter = okWidths, arrowWidth = 0, edgeColor = 'black')
  edgeSpecs <- rbind(edgeSpecs, subset(allWidths, !parameter %in% edgeSpecs$parameter))
  edgeSpecs <- edgeSpecs[order(edgeSpecs$parameter), ]

  # Get Node Colors
  nodeSpecs <- dplyr::select(kdat, 'parameter', 'nodeColor')

  # Add graph title w/treatment
  graphTitle <- unique(kdat$treatment)

  kinetogram <- DiagrammeR::grViz("
    digraph dot {
    graph [layout = dot, rankdir = LR, overlap = false, label = '@@6', labelloc = t, labelsize = 5]

    # --- Nodes ----
    node [shape = circle, fixedsize = true, style = filled, penwidth = 0.1, fontsize = 4]
      NP [label = '@@1-6', width = @@2-6, color = @@5-6, fillcolor = white, fontcolor = black]
      C [label = '@@1-1', width = @@2-1, color = @@5-1, fillcolor = grey]
      F [label = '@@1-4', width = @@2-4, color = @@5-4, fillcolor = FireBrick]
      pd [label = '@@1-7', width = @@2-7, color = @@5-7, fillcolor = LemonChiffon]
      G [label = '@@1-5', width = @@2-5, color = @@5-5, fillcolor = LightBlue]
      E1 [label = '@@1-2', width = @@2-2, color = @@5-2, fillcolor = SeaGreen]
      E2 [label = '@@1-3', width = @@2-3, color = @@5-3, fillcolor = SeaGreen]

    # --- Edges ----
    edge [arrowsize = 0.1, color = black]
      C -> E1 [penwidth = '@@3-1', color = @@4-1]
      C -> F [penwidth = '@@3-2', color = @@4-2]
      C -> G [penwidth = '@@3-3', color = @@4-3]
      C -> NP [penwidth = '@@3-4', color = @@4-4]
      C -> pd [penwidth = '@@3-5', color = @@4-5]

      E1 -> C [penwidth = '@@3-6', color = @@4-6]
      E1 -> E2 [penwidth = '@@3-7', color = @@4-7]

      E2 -> C [penwidth = '@@3-8', color = @@4-8]
      E2 -> E1 [penwidth = '@@3-9', color = @@4-9]

      F -> C [penwidth = '@@3-10', color = @@4-10]
      G -> C [penwidth = '@@3-11', color = @@4-11]
      NP -> C [penwidth = '@@3-12', color = @@4-12]
      pd -> C [penwidth = '@@3-13', color = @@4-13]

    subgraph {
      rank = same; pd; C; F;
    }

  }

  # --- Dynamically set values ----

  # --- Node labels (percentages)
  [1]: paste0(durations$parameter, '\\n', durations$mean, '%')
  [2]: durations$mean / 100
  [3]: edgeSpecs$arrowWidth / 15
  [4]: edgeSpecs$edgeColor
  [5]: nodeSpecs$nodeColor
  [6]: graphTitle
  ")

  return(kinetogram)

}
