make_kinetogram <- function(kineDat, trt) {

  # pVals <- kineDat$pValues
  # pVals <- subset(pVals, trt1 %in% trt & trt2 %in% trt)

  kdat <- kineDat$summaryStats
  kdat <- subset(kdat, treatment == trt)
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


  edgeWidths <- dplyr::select(kdat, 'parameter', 'arrowWidth')
  edgeWidths <- edgeWidths[grepl(',', edgeWidths$parameter), ]

  # omit edgewidths not shown: TODO: Check with Vamsi on this!
  okWidths <- c('C,E1', 'C,F', 'C,G', 'C,NP', 'C,pd',
                'E1,C', 'E1,E2', 'E2,C', 'E2,E1', 'F,C', 'G,C', 'NP,C', 'pd,C')
  edgeWidths <- subset(edgeWidths, parameter %in% okWidths)
  allWidths <- data.frame(parameter = okWidths, arrowWidth = 0)
  edgeWidths <- rbind(edgeWidths, subset(allWidths, !parameter %in% edgeWidths$parameter))
  edgeWidths <- edgeWidths[order(edgeWidths$parameter), ]

  kinetogram <- DiagrammeR::grViz("
    digraph dot {
    graph [layout = dot, rankdir = LR, overlap = false]

    # --- Nodes ----
    node [shape = circle, fixedsize = true, style = filled, penwidth = 0.1, fontsize = 4]
      NP [label = '@@1-6', width = @@2-6, fillcolor = white, fontcolor = black]
      C [label = '@@1-1', width = @@2-1, fillcolor = grey]
      F [label = '@@1-4', width = @@2-4, fillcolor = FireBrick]
      pd [label = '@@1-7', width = @@2-7, fillcolor = LemonChiffon]
      G [label = '@@1-5', width = @@2-5, fillcolor = LightBlue]
      E1 [label = '@@1-2', width = @@2-2, fillcolor = SeaGreen]
      E2 [label = '@@1-3', width = @@2-3, fillcolor = SeaGreen]

    # --- Edges ----
    edge [arrowsize = 0.1, color = black]
      C -> E1 [penwidth = '@@3-1']
      C -> F [penwidth = '@@3-2']
      C -> G [penwidth = '@@3-3']
      C -> NP [penwidth = '@@3-4']
      C -> pd [penwidth = '@@3-5']

      E1 -> C [penwidth = '@@3-6']
      E1 -> E2 [penwidth = '@@3-7']

      E2 -> C [penwidth = '@@3-8']
      E2 -> E1 [penwidth = '@@3-9']

      F -> C [penwidth = '@@3-10']
      G -> C [penwidth = '@@3-11']
      NP -> C [penwidth = '@@3-12']
      pd -> C [penwidth = '@@3-13']

    subgraph {
      rank = same; pd; C; F;
    }

  }

  # --- Dynamically set values ----

  # --- Node labels (percentages)
  [1]: paste0(durations$parameter, '\\n', durations$mean, '%')
  [2]: durations$mean / 100
  [3]: edgeWidths$arrowWidth / 15

  ")

  return(kinetogram)

}
