rm(list=ls())

lapply(c('discoEPG', 'shiny', 'shinyWidgets', 'dplyr', 'rhandsontable', 'gt',
         'svglite', 'DiagrammeR', 'DiagrammeRsvg', 'ggplot2'), library, character.only = TRUE)

# Code for running on Clay's laptop
# appPath <- "/Users/c/Documents/EPG Project/EPGShinyApp/Deployed/"
# parameterMetadata <- read.csv(paste0(appPath, "ParameterMetadata.csv"))

# Code for running on Shiny server
appPath <- ""
parameterMetadata <- read.csv("ParameterMetadata.csv")


# Rest of the code is "platform-agnostic"
colnames(parameterMetadata) <- tolower(colnames(parameterMetadata))
parameterMetadata$acronym <- gsub('\\*\\d+$', '', parameterMetadata$acronym) # remove asterix, to help with merge to R parameter data
parameterMetadata <- rename(parameterMetadata, parameter = definition)

waveform_labels = c("np" = 1,
                    "C" = 2,
                    "E1e" = 3,
                    "E1" = 4,
                    "E2" = 5,
                    "F" = 6,
                    "G" = 7,
                    "pd-S" = 8,
                    "pd-S-II-2" = 9,
                    "pd-S-II-3" = 10,
                    "pd-L" = 11,
                    "pd-L-II-2" = 13,
                    "pd-L-II-3" = 14,
                    "end" = 12)


# ---- Shiny App Starts Here ----
shinyApp(

  ui <- fluidPage(

    tabsetPanel(id = "tabsets",

                # ---- Tab 0: Home Page - Background & Instructions
                tabPanel(title = 'Home Page', fluid = TRUE, value = 'homeTab',
                         # titlePanel("EPG Shiny App: Start Here"),
                         mainPanel(
                           imageOutput('discoLogo', height = '100px'),
                           br(),
                           hr(),
                           h4(strong("Project Description")),
                           p(style="text-align: justify; font-size = 50px",
                             "Electrical penetration graph (EPG) is a technique used to study the feeding behavior of aphids on plants. In this technique, the aphid and plant are made part of an electrical circuit, which is completed when aphid mouthparts penetrate plant tissue. When the aphid stylet is inserted intercellularly, the voltage is positive and when inserted intracellularly, the voltage is negative. Waveforms in EPG have been correlated to specific aphid feeding behaviors by stylectomy followed by microscopy of the plant tissue to determine the approximate location of the stylet as well as observing aphid head movement, posture, and muscle dynamics. EPG is well established and has been widely used to study the mechanisms of plant virus transmission by aphids, the effect of resistant and susceptible lines on aphid feeding behaviors, and to better our understanding of the mechanisms that aphids use to continuously drink from the phloem."
                           ),
                           p(style="text-align: justify; font-size = 50px",
                             "During EPG, fluctuating voltage signals in the circuit are graphed, and a researcher interprets resulting waveforms as specific stylet activity. EPG research can generate hundreds of recordings with thousands of events (individual occurrences of a waveform) per experiment. After measurement of waveforms, data consist of a list of different behaviors and associated durations. These data are further processed to yield hundreds of variables that are compiled, statistically analyzed, and converted to easily understood, visually compelling information before publication. Therefore, efficient measurement and analysis of these large data sets are critical."
                           ),
                           p(style="text-align: justify; font-size = 50px",
                             "Software such as Stylet+, Windaq, and MacStylet allow users to view waveform recordings and provide ‘calls’ or annotations to waveforms. These programs also generate flat files (with the extension ‘.ANA’) composed of a number corresponding to the particular waveform in the first column and the time at which that particular waveform occurred in the second column. Currently, there are no programs that allows users to calculate aphid feeding behavior parameters and also perform statistical analysis from ANA files quickly and easily."
                           ),
                           p(style="text-align: justify; font-size = 50px",
                             "The discoEPG package gives users the ability to easily calculate feeding behavior parameters directly from ‘.ANA’ files, perform statistical analysis on calculated parameters, and generate summary tables and graphs all in one package. Users can upload their ANA files, assign treatments, and calculate feeding behavior parameters in the 'Upload Data' tab. In the 'Validation Errors' tab, users can check their waveform calls and ensure the typical sequence of feeding behaviors are being followed in their annotations. The 'Summary Statistics' tab allows users to generate summary tables and perform statistical analysis between user selected treatments. The tables provide the mean and standard error of a particular, the group p-value, and individual comparison p-values for the selected parameter. Recordings that display over 70% of the recording duration in the F, G or NP waveforms are excluded from statistical analysis. The 'Boxplots' tab provides users boxplot figures of a selected parameter and treatment. The 'Timeseries Plot' tab provides users with timeseries plots, which display the feeding behavior that was displayed by the majority of aphids at a specific time during the recording. The 'Kinetogram' tab provides users with a behavioral kinetogram that shows the number of transitional events for each waveform."
                           ),
                           hr(),
                           h4(strong("Getting Started: Uploading Data and Assigning Treatments")),
                           p(style="text-align: justify; font-size = 50px",
                             "The first steps in using this app are outlined below. You will start in the 'Upload Data' tab, by clicking the 'Browse' button:"
                           ),
                           imageOutput('uploadDataScreenshot', height = '500px'),
                           br(),
                           p(style="text-align: justify; font-size = 50px",
                             "The above screenshot shows the file selection dialogue. After selecting the .ANA files, you will be presented with a table where you can assign treatments to each analysis:"
                           ),
                           imageOutput('assignTreatmentsScreenshot', height = '400px'),
                           br(),
                           p(style="text-align: justify; font-size = 50px",
                             "Once you have assigned treatments to each .ANA file, click the 'Assign Treatments and Calculate Parameters' button. You can now view either the raw .ANA file data (displayed below), or the computed parameter values (not shown), by using the radio-button toggle in the menu bar:"
                           ),
                           imageOutput('afterAssignTreatmentsScreenshot', height = '400px'),
                           br(),
                           p(style="text-align: justify; font-size = 50px",
                             "You're all done with data processing! Now you can move on to other tabs to see data validation errors, summary statistics, and plots."
                           )
                         )
                ),

                # ---- Tab 1: Upload Data & Calculate Parameters
                tabPanel(title = "Upload Data", fluid = TRUE, value = "dataTab",
                         titlePanel("EPG Shiny App: Upload Data and Calculate Parameters"),
                         sidebarLayout(
                           sidebarPanel(
                             fileInput(inputId = "files", label = "Choose ANA File(s)", multiple = T, placeholder = "Please Upload Files"),
                             conditionalPanel("output.filesUploaded", {
                               radioButtons(inputId = 'tableChoice', label = "Choose data to display",
                                            choices = c('Files/Treatment Assignments' = 'files',
                                                        'Raw Data' = 'rawData',
                                                        'Computed Parameters' = 'paramData')

                               )
                             }),
                             conditionalPanel("output.filesUploaded", {
                               actionButton(inputId = 'assignTrtCalcParam', label = "Assign Treatments and Compute Parameters")
                             }),
                             # conditionalPanel("output.metadataSaved", {
                             #   actionButton(inputId = 'calculateParameters', label = "Calculate Parameter Values")
                             # }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               downloadButton(outputId = "downloadData", label = "Download Parameter Data")
                             })
                           ),
                           mainPanel(
                             conditionalPanel("output.filesUploaded && input.tableChoice == 'files'", {
                               rHandsontableOutput("metadata_hot")
                             }),
                             conditionalPanel("output.trtAssignParamCalc && input.tableChoice == 'rawData'", {
                               gt_output("data_gt")
                             }),
                             conditionalPanel("output.trtAssignParamCalc && input.tableChoice == 'paramData'", {
                               gt_output("params_gt")
                             })
                           )
                         )
                ),

                # ---- Tab 1b: Validation Errors
                tabPanel(title = "Validation Errors", fluid = TRUE, value = "validationErrors",
                         titlePanel("EPG Shiny App: Raw Data Validation Errors"),
                         verbatimTextOutput("validationText")
                ),

                # ---- Tab 2: Summary Statistics
                tabPanel(title = "Summary Statistics", fluid = TRUE, value = "summaryStatsTab",
                         titlePanel("EPG Shiny App: Summary Statistics, by Treatment"),
                         sidebarLayout(
                           sidebarPanel(
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "treatmentPicker", label = "Select Treatment(s)",
                                                         choices = "Please enter treatments!",
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "parameterGroupPicker", label = "Select Parameter Grouping(s)",
                                                         choices = unique(parameterMetadata$experiment),
                                                         selected = unique(parameterMetadata$experiment),
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "parameterPicker", label = "Select Parameter(s)",
                                                         choices = unique(parameterMetadata$acronym),
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               radioButtons(inputId = "timeUnits", label = "Select Units of Time", choices = c('seconds', 'minutes', 'hours'), selected = 'seconds')
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               actionButton(inputId = 'calculateSummaryStats', label = "Calculate Summary Statistics")
                             }),
                             conditionalPanel("output.sumstatsCalculated", {
                               downloadButton(outputId = "downloadSummaryStats", label = "Download Summary Statistics")
                             })
                           ),
                           mainPanel(
                             conditionalPanel("output.trtAssignParamCalc", {
                               gt_output("summaryStats_gt")
                             })
                           )
                         )
                ),


                # ---- Tab 3: Boxplots
                tabPanel(title = "Boxplots", fluid = TRUE, value = "boxplotsTab",
                         titlePanel("EPG Shiny App: Boxplots, by Treatment"),
                         sidebarLayout(
                           sidebarPanel(
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "treatmentPickerBoxplots", label = "Select Treatment(s)",
                                                         choices = "Please enter treatments!",
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               radioButtons(inputId = 'boxplotRadio', label = "Which boxplots to show?", choices = c('Statistically significant', 'Selected parameters'), selected = 'Statistically significant')
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "boxplotParameterPicker", label = "Select Parameter(s)",
                                                         choices = unique(parameterMetadata$acronym),
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               actionButton(inputId = 'showBoxplots', label = "Show Boxplots")
                             })
                           ),
                           mainPanel(
                             plotOutput("boxplots")
                           )
                         )
                ),


                # ---- Tab 4: TimeSeries Plots
                tabPanel(title = "Timeseries Plot", fluid = TRUE, value = "timeseriesTab",
                         titlePanel("EPG Shiny App: Timeseries plots, by Treatment"),
                         sidebarLayout(
                           sidebarPanel(
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "treatmentPickerTimeseries", label = "Select Treatment(s)",
                                                         choices = "Please enter treatments!",
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               actionButton(inputId = 'plotTimeseries', label = "Plot Timeseries")
                             }),
                             conditionalPanel("output.timeseriesCalculated", {
                               downloadButton(outputId = "downloadTimeseriesData", label = "Download Timeseries Data")
                             }),
                             conditionalPanel("output.timeseriesCalculated", {
                               downloadButton(outputId = "downloadTimeseriesPlot", label = "Download Timeseries Plot")
                             })
                           ),
                           mainPanel(
                             conditionalPanel("output.trtAssignParamCalc", {
                               plotOutput("timeseriesPlot")
                             })
                           )
                         )
                ),


                # ---- Tab 5: Kinetogram
                tabPanel(title = "Kinetogram", fluid = TRUE, value = "kinetogramTab",
                         titlePanel("EPG Shiny App: Kinetograms, by Treatment"),
                         sidebarLayout(
                           sidebarPanel(
                             conditionalPanel("output.trtAssignParamCalc", {
                               shinyWidgets::pickerInput(inputId = "treatmentPickerKinetogram", label = "Select Exactly 2 Treatment(s)",
                                                         choices = "Please enter treatments!",
                                                         multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE))
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               actionButton(inputId = 'showKinetogram', label = "Show Kinetogram")
                             }),
                             conditionalPanel("output.kinetogramCalculated", {
                               downloadButton(outputId = "downloadKinetogramData", label = "Download Kinetogram Data")
                             }),
                             conditionalPanel("output.kinetogramCalculated", {
                               downloadButton(outputId = "downloadKinetogramPlot1", label = "Download Kinetogram Plot, 1st Treatment")
                             }),
                             conditionalPanel("output.kinetogramCalculated", {
                               downloadButton(outputId = "downloadKinetogramPlot2", label = "Download Kinetogram Plot, 2nd Treatment")
                             }),

                           ),
                           mainPanel(
                             p(style="text-align: justify; font-size = 50px",
                               "Note: Statistically significant differences between treatments are shown by red arrows or node outlines."
                             ),
                             conditionalPanel("output.trtAssignParamCalc", {
                               grVizOutput("kinetogramPlot1")
                             }),
                             conditionalPanel("output.trtAssignParamCalc", {
                               grVizOutput("kinetogramPlot2")
                             })
                           )
                         )
                )

    )

  ),


  server <- function(input, output, session) {

    ### ----- Define Control Variables ----

    output$filesUploaded <- reactive(if(length(input$files) > 0) TRUE else FALSE)
    output$trtAssignParamCalc <- reactive(if(input$assignTrtCalcParam > 0) TRUE else FALSE)
    output$sumstatsCalculated <- reactive(if(input$calculateSummaryStats > 0) TRUE else FALSE)
    output$timeseriesCalculated <- reactive(if(input$plotTimeseries > 0) TRUE else FALSE)
    output$kinetogramCalculated <- reactive(if(input$showKinetogram > 0) TRUE else FALSE)
    outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)
    outputOptions(output, "trtAssignParamCalc", suspendWhenHidden = FALSE)
    outputOptions(output, "sumstatsCalculated", suspendWhenHidden = FALSE)
    outputOptions(output, "timeseriesCalculated", suspendWhenHidden = FALSE)
    outputOptions(output, "kinetogramCalculated", suspendWhenHidden = FALSE)


    ### ----- Tab 0: Home Page ----

    # Render images
    output$discoLogo <- renderImage(list(src = paste0(appPath, 'discoEPGLogo.png'), contentType = 'image/png', width = 500, height = 100), deleteFile = F)
    output$uploadDataScreenshot <- renderImage(list(src = paste0(appPath, 'uploadDataScreenshot.png'), contentType = 'image/png', width = 1500, height = 500), deleteFile = F)
    output$assignTreatmentsScreenshot <- renderImage(list(src = paste0(appPath, 'assignTreatmentsScreenshot.png'), contentType = 'image/png', width = 1000, height = 400), deleteFile = F)
    output$afterAssignTreatmentsScreenshot <- renderImage(list(src = paste0(appPath, 'afterAssignTreatmentsScreenshot.png'), contentType = 'image/png', width = 1500, height = 400), deleteFile = F)


    ### ----- Tab 1: Data Processing ----

    # Table for Assigning Treatments
    metadata_handson <- reactive({
      filenames <- sort(input$files$name)
      df <- data.frame(treatment = rep('', length(filenames)), filename = filenames)
      rhandsontable(df) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$metadata_hot <- renderRHandsontable(metadata_handson())


    dataAndErrorsList <- reactive({

      filepaths <- input$files$datapath
      filenames <- input$files$name

      # Read in all .ANA data into a list of dataframes and error messages
      out <- combine_ana(filenames = filenames, directory = NULL, filepaths_shiny = filepaths, waveform_labels = waveform_labels)

      nErrors <- length(out$errors)
      # If any errors in ANA files, show a pop-up warning to the user
      if(nErrors > 0) {
        showNotification(ui=paste0("** WARNING ** Data validation checks failed in ", nErrors, " files! See the 'Data Validation' page for details."),
                         type = 'warning', closeButton = TRUE, duration = 10)
      }

      return(out)
    })


    # Assign treatments to all data and return a single data frame
    alldata <- reactive({
      dat <- dataAndErrorsList()$data

      # Get updated treatment assignments and merge to data
      meta <- hot_to_r(input$metadata_hot)
      dat <- merge(dat, meta, by = 'filename')
      return(dat)
    })  %>% bindEvent(input$assignTrtCalcParam)

    # Output data table
    output$data_gt <- render_gt(alldata())

    # Calculate Parameters and return a data frame
    paramData <- reactive({

      dat <- alldata()
      if(nrow(dat) > 0) {
        # Convert combined ANA data to parameters
        epgData <- ana_to_parameters(ana_df = dat, waveform_labels = waveform_labels)

        # Check if any files have > 70% of total time spent in F, G or np. If so, remove these from analysis and show notification in app.
        idx <- which(epgData$acronym == "%timeinF+G+np" & epgData$value > 70)

        if(length(idx) > 0) {
          filesToRemove <- unique(epgData$filename[idx])
          showNotification(ui=paste("These files have > 70% of time spent in F/G/np phases combined, and will be omitted from analyses: ", paste(filesToRemove, collapse = ', ')),
                           type = 'warning', closeButton = TRUE, duration = NULL)
          epgData <- subset(epgData, !(filename %in% filesToRemove))
        }

        # Remove this variable, not used in analyses
        epgData <- subset(epgData, acronym != "%timeinF+G+np")

        # Merge to treatments
        meta <- hot_to_r(input$metadata_hot)
        epgData <- merge(epgData, meta, by = c('filename'))
        return(epgData)
      } else {
        return(NULL)
      }
    }) %>% bindEvent(input$assignTrtCalcParam)

    # Export Data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("ShinyAppParameterDataExport_", Sys.time(), ".csv")
      },
      content = function(file) {
        write.csv(paramData(), file, row.names = FALSE)
      }
    )

    # Format Parameter Table For Display
    output$params_gt <- render_gt({
      paramDat <- select(paramData(), -one_of('description', 'treatment'))

      trtKey <- hot_to_r(input$metadata_hot)
      trtKey <- subset(trtKey, filename %in% unique(paramDat$filename))

      # Need to group columns by treatment. and then sort alphabetically(?)
      trtKey <- trtKey[order(trtKey$treatment, trtKey$filename), ]

      # Rearrange so each filename has its own column
      paramList <- split(paramDat, paramDat$filename)
      paramList <- lapply(paramList, function(df) {
        colnames(df)[colnames(df) == 'value'] <- unique(df$filename)
        select(df, -one_of('filename'))
      })
      paramDat <- paramList[[1]]
      if(length(paramList) > 1) {
        for(i in 2:length(paramList)) {
          paramDat <- merge(paramDat, paramList[[i]], by='acronym')
        }
      }

      # Sort columns and add Treatment prefix to file columns
      paramDat <- relocate(paramDat, c('acronym', trtKey$filename))
      for(tx in unique(trtKey$treatment)) {
        idx <- colnames(paramDat) %in% trtKey$filename[trtKey$treatment == tx]
        colnames(paramDat)[idx] <- paste(tx, colnames(paramDat)[idx], sep = '^^^')
      }

      paramTbl <- paramDat %>%
        gt() %>%
        cols_label(acronym = 'Parameter') %>%
        tab_spanner_delim(columns = -one_of('acronym'), delim = '^^^')

      return(paramTbl)

    }) %>% bindEvent(input$assignTrtCalcParam)

    ### ----- Tab 1b: Validation Errors ----
    output$validationText <- renderText({
      errors <- dataAndErrorsList()$errors
      nErrors <- length(errors)
      # If any errors in ANA files, show a pop-up warning to the user
      if(nErrors > 0) {
        # msg <- unlist(errorList[errorInd])
        msg <- paste(errors, collapse = '')
      } else {
        msg <- ""
      }
      return(msg)
    })


    ### ----- Tab 2: Summary Statistics Tables ----

    summaryStats <- reactive({
      compute_summary_stats(parameter_df = paramData(),
                            parameter_metadata = parameterMetadata,
                            time_units = input$timeUnits,
                            treatments = input$treatmentPicker,
                            acronyms = input$parameterPicker,
                            alpha = 0.05,
                            p_adjust_method = 'bonferroni')
    }) %>% bindEvent(input$calculateSummaryStats)

    # Summary Stats Table (for Display)
    output$summaryStats_gt <- render_gt({

      sumStats <- summaryStats()
      # Convert NA to empty cell
      dunnCols <- grep('dunn_pval', colnames(sumStats))
      for(dc in dunnCols) {
        sumStats[is.na(sumStats[ , dc]), dc] <- ''
      }

      # Make pretty
      sumStats <- sumStats %>%
        select(-one_of('experiment')) %>%
        rename(Parameter = acronym, Unit = unit)

      if("dunn_pval^^overall" %in% colnames(sumStats)) {
        sumStats <- arrange(sumStats, `dunn_pval^^overall`)
      }

      colnames(sumStats) <- gsub("mean.se", "Mean \u00B1 SE", colnames(sumStats))
      colnames(sumStats) <- gsub("dunn_pval", "p-value", colnames(sumStats))

      sumStats_gt <- sumStats %>%
        gt() %>%
        tab_header(title = "Summary Statistics, by Treatment") %>%
        tab_spanner_delim(delim = '^^')  %>%
        tab_stubhead(label = "Treatments")

      return(sumStats_gt)

    })

    # Update treatments available in pick-list based on parameter group pick-list, and data
    observeEvent(input$tabsets, {
      # Run only if Summary Stats Tab/Page is selected, and treatments have been assigned
      if(input$tabsets == 'summaryStatsTab' & input$assignTrtCalcParam > 0) {
        tx <- unique(hot_to_r(input$metadata_hot)$treatment)
        updatePickerInput(session = session, inputId = "treatmentPicker", choices = tx, selected = tx)
      }

    }, ignoreInit = TRUE)

    # Update parameters available in pick-list based on parameter group pick-list, and data
    observeEvent(input$parameterGroupPicker, {
      parameterGroups <- input$parameterGroupPicker
      params <- summaryStats() %>%
        filter(experiment %in% parameterGroups) %>%
        select('acronym') %>%
        distinct() %>%
        as.vector()

      updatePickerInput(session = session, inputId = "parameterPicker", choices = params, selected = params)
    }, ignoreInit = TRUE)

    # Export Summary Statistics (This works on a remotely served version)
    output$downloadSummaryStats <- downloadHandler(
      filename = function() {
        paste0("ShinyAppSummaryStatsExport_", Sys.time(), ".csv")
      },
      content = function(file) {
        write.csv(summaryStats(), file, row.names = FALSE)
      }
    )

    ### ----- Tab 3: Box Plots ----

    # Update treatments available in pick-list
    observeEvent(input$tabsets, {
      # Run only if Timeseries Tab/Page is selected, and treatments have been assigned
      if(input$tabsets == 'boxplotsTab' & input$assignTrtCalcParam > 0) {
        tx <- unique(hot_to_r(input$metadata_hot)$treatment)
        updatePickerInput(session = session, inputId = "treatmentPickerBoxplots", choices = tx, selected = tx)
      }
    }, ignoreInit = TRUE)

    # Boxplots
    output$boxplots <- renderPlot({
      dat <- paramData()
      sumStats <- summaryStats()

      if(input$boxplotRadio == 'Statistically significant') {
        pvalCols <- grep('dunn_pval', colnames(sumStats), value = TRUE)
        if(length(pvalCols) > 0) {
          if(length(pvalCols) > 1) {
            sumStats <- subset(sumStats, `dunn_pval^^overall` < 0.05)
            dat <- subset(dat, acronym %in% sumStats$acronym)
          } else { # only 1 pvalue column
            sumStats <- subset(sumStats, pvalCols < 0.05)
            dat <- subset(dat, acronym %in% sumStats$acronym)
          }
          if(nrow(dat) == 0) showNotification("No statistically significant parameter differences.", type = 'warning', closeButton = TRUE, duration = 5)
        } else {
          showNotification("Data only exists for 1 treatment. Cannot compute statistical differences.",
                           type = 'warning', closeButton = TRUE, duration = 5)
        }

      } else {
        dat <- subset(dat, acronym %in% input$boxplotParameterPicker)
      }

      if(nrow(dat) > 0) {
        subDat <- subset(dat, treatment %in% input$treatmentPickerBoxplots)
        out <- ggplot(subDat, aes(x=treatment, y=value, color=treatment)) +
          geom_boxplot() +
          theme_classic(base_size = 16) +
          geom_jitter(width = 0.02, height = 0) +
          facet_wrap(~acronym, scales = 'free')
      } else {
        out <- NULL
      }
      return(out)
    }) %>% bindEvent(input$showBoxplots)



    ### ----- Tab 4: Timeseries Plot ----

    # Update treatments available in pick-list
    observeEvent(input$tabsets, {
      # Run only if Timeseries Tab/Page is selected, and treatments have been assigned
      if(input$tabsets == 'timeseriesTab' & input$assignTrtCalcParam > 0) {
        tx <- unique(hot_to_r(input$metadata_hot)$treatment)
        updatePickerInput(session = session, inputId = "treatmentPickerTimeseries", choices = tx, selected = tx)
      }

    }, ignoreInit = TRUE)

    timeseriesDat <- reactive({
      anaDat <- alldata()
      selected_treatments <- input$treatmentPickerTimeseries

      tsList <- lapply(selected_treatments, function(tx) {
        trt_dat <- subset(anaDat, treatment == tx)
        tsdat <- ana_to_timeseries(ana_df = trt_dat, waveform_labels = waveform_labels)$data
        tsdat$treatment <- tx
        return(tsdat)
      })

      tsDat <- do.call(rbind, tsList)
      return(tsDat)
    }) %>% bindEvent(input$plotTimeseries)

    # Timeseries Plot (for Display)
    tsPlot <- function() {
      modeDat <- timeseriesDat()
      # re-order factor, in same order as "waveform_labels"
      modeDat$activity <- factor(toupper(modeDat$activity),
                                 levels = toupper(intersect(names(waveform_labels), unique(modeDat$activity))))
      # remove 'end' for plotting
      modeDat <- subset(modeDat, modeDat$activity != 'END')

      ggplot(modeDat, aes(x=activity, y=time, color=activity)) +
        geom_point(shape=20, size=1) +
        theme_classic(base_size = 16) +
        coord_flip() +
        facet_wrap(~treatment, ncol=1)
    }

    output$timeseriesPlot <- renderPlot({
      tsPlot()
    })

    # Export Timeseries Data
    output$downloadTimeseriesData <- downloadHandler(
      filename = function() {
        paste0("ShinyAppTimeseriesDataExport_", Sys.time(), ".csv")
      },
      content = function(file) {
        write.csv(timeseriesDat(), file, row.names = FALSE)
      }
    )

    # Export Timeseries Plot (SVG)
    output$downloadTimeseriesPlot <- downloadHandler(
      filename = function() {
        paste0("ShinyAppTimeseriesPlotExport_", Sys.time(), ".svg")
      },
      content = function(file) {
        svglite(file, width = 8, height = 5)
        print(tsPlot())
        dev.off()
      }
    )


    ### ----- Tab 5: Kinetograms   ----

    # Update treatments available in pick-list
    observeEvent(input$tabsets, {
      # Run only if Kinetogram Tab/Page is selected, and treatments have been assigned
      if(input$tabsets == 'kinetogramTab' & input$assignTrtCalcParam > 0) {
        tx <- unique(hot_to_r(input$metadata_hot)$treatment)
        updatePickerInput(session = session, inputId = "treatmentPickerKinetogram", choices = tx, selected = tx)
      }

    }, ignoreInit = TRUE)

    # A list of 2 graph viz objects (one kinetogram "viz" per treatment)
    # kinetogramList <- reactive({
    #   deList <- dataAndErrorsList()
    #   anaDat <- deList$data
    #   trtKey <- hot_to_r(input$metadata_hot)
    #   anaDat <- merge(anaDat, trtKey)
    #   anaDat <- subset(anaDat, treatment %in% input$treatmentPickerKinetogram)
    #   kineDat <- ana_to_kinetogram_df(ana_df = anaDat)
    #   kinePlots <- compare_kinetograms(kineDat = kineDat)
    #   return(list(data = kineDat, plots = kinePlots))
    # }) %>% bindEvent(input$showKinetogram)

    kinetogramList <- function() {
      deList <- dataAndErrorsList()
      anaDat <- deList$data
      trtKey <- hot_to_r(input$metadata_hot)
      anaDat <- merge(anaDat, trtKey)
      anaDat <- subset(anaDat, treatment %in% input$treatmentPickerKinetogram)
      kineDat <- ana_to_kinetogram_df(ana_df = anaDat)
      kinePlots <- compare_kinetograms(kineDat = kineDat)
      return(list(data = kineDat, plots = kinePlots))
    }

    # kinetogramList <- reactive({
    #   kineList()
    # }) %>% bindEvent(input$showKinetogram)


    output$kinetogramPlot1 <- renderGrViz({
      kinetogramList()$plots[[1]]
    }) %>% bindEvent(input$showKinetogram)

    output$kinetogramPlot2 <- renderGrViz({
      kinetogramList()$plots[[2]]
    }) %>% bindEvent(input$showKinetogram)

    # Export Kinetogram Data
    output$downloadKinetogramData <- downloadHandler(
      filename = function() {
        paste0("ShinyAppKinetogramDataExport_", Sys.time(), ".csv")
      },
      content = function(file) {
        write.csv(kinetogramList()$data$summaryStats, file, row.names = FALSE)
      }
    )

    # Export Kinetogram Plots (SVG)
    output$downloadKinetogramPlot1 <- downloadHandler(
      filename = function() {
        paste0("ShinyAppKinetogramPlotExport_Treatment_", names(kinetogramList()$plots)[1], "_", Sys.time(), ".svg")
      },
      content = function(file) {
        svg <- DiagrammeRsvg::export_svg(kinetogramList()$plots[[1]])
        fileConn <- file(file)
        writeLines(svg, fileConn)
        close(fileConn)
      }
    )
    output$downloadKinetogramPlot2 <- downloadHandler(
      filename = function() {
        paste0("ShinyAppKinetogramPlotExport_Treatment_", names(kinetogramList()$plots)[2], "_", Sys.time(), ".svg")
      },
      content = function(file) {
        svg <- DiagrammeRsvg::export_svg(kinetogramList()$plots[[2]])
        fileConn <- file(file)
        writeLines(svg, fileConn)
        close(fileConn)
      }
    )


  }

)

