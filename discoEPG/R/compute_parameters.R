
# Parameter function
compute_parameters <- function(EPG_analysis, waveform_labels){

  # No end exception
  EPG_parameters = as.data.frame(matrix(nrow = 0,ncol = 2,dimnames = list(NULL,c("Value","Description"))))

  # Some precomputed values for convenience
  if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-2")])){
    pre_pdLII2 = which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-2")]) - 1
    EPG_analysis$Waveform[pre_pdLII2][EPG_analysis$Waveform[pre_pdLII2] %in% waveform_labels["pd-L"]] = waveform_labels["pd-L-II-2"]
  }

  if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-3")])){
    pre_pdLII3 = which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-3")]) - 1
    EPG_analysis$Waveform[pre_pdLII3][EPG_analysis$Waveform[pre_pdLII3] %in% waveform_labels[c("pd-L","pd-L-II-2")]] = waveform_labels["pd-L-II-3"]
  }

  np_idx = EPG_analysis$Waveform %in% waveform_labels["np"]
  EPG_analysis[["Duration"]] = c(EPG_analysis$Time[-1] - EPG_analysis$Time[-length(EPG_analysis$Time)],0)

  probe_fun = function(EPG_analysis,np_idx,parameter){
    probe_split = unname(split(EPG_analysis[[parameter]], cumsum(seq_along(EPG_analysis[[parameter]]) %in% which(np_idx))))
    probe_split = lapply(seq_along(probe_split),function(x){
      if(length(probe_split[[x]]) > 1){
        probe_split[[x]][-1]
      }
    })
    probe_split[sapply(probe_split,is.null)] = NULL
    names(probe_split) = seq_along(probe_split)
    return(probe_split)
  }

  probe_time = probe_fun(EPG_analysis = EPG_analysis,np_idx = np_idx,parameter = "Time")
  probe_duration = sapply(probe_time,function(x){
    if(any(EPG_analysis$Time[np_idx] > x[1])){
      return(EPG_analysis$Time[np_idx][EPG_analysis$Time[np_idx] > x[1]][1] - x[1])
    }else{
      return(max(EPG_analysis$Time) - x[1])
    }
  })
  probe_wave = probe_fun(EPG_analysis = EPG_analysis,np_idx = np_idx,parameter = "Waveform")
  probe_time_duration = probe_fun(EPG_analysis = EPG_analysis,np_idx = np_idx,parameter = "Duration")

  ##### TODO is G/F added here to C wave?
  C_waves = split(EPG_analysis[which(EPG_analysis$Waveform %in% waveform_labels[c("C","pd-L","pd-L-II-2","pd-L-II-3","pd-S","pd-S-II-2","pd-S-II-3")]),c("Time","Duration")],
                  cumsum(c(1, diff(which(EPG_analysis$Waveform %in% waveform_labels[c("C","pd-L","pd-L-II-2","pd-L-II-3","pd-S","pd-S-II-2","pd-S-II-3")])) != 1)))
  pre_E1 = which(EPG_analysis$Waveform %in% waveform_labels["E1"]) - 1
  E2_postE1 = EPG_analysis$Time %in% EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) + 1][EPG_analysis$Waveform[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) + 1] %in% waveform_labels["E2"]]
  E2_postE1_sus = EPG_analysis$Time %in% EPG_analysis$Time[E2_postE1][EPG_analysis$Duration[E2_postE1] > 10*60]
  E2_sus = EPG_analysis$Time %in% EPG_analysis$Time[EPG_analysis$Duration > 10*60 & EPG_analysis$Waveform %in% waveform_labels["E2"]]
  pd_E1E2sus = unlist(sapply(EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")]],function(x){
    if(any(x < EPG_analysis$Time[which(E2_postE1_sus) - 1])){
      return(x)
    }
  }))
  if(!is.null(pd_E1E2sus)){
    pd_E1E2sus = max(pd_E1E2sus)
  }

  # Calculate parameters
  EPG_parameters["t_1Pr",] = c(Value = EPG_analysis$Time[!np_idx][1],
                               Description = "Time to 1st probe from start of EPG")
  EPG_parameters["n_Pr.1E1",] = c(Value = which(sapply(names(probe_wave),function(x) any(probe_wave[[x]] %in% waveform_labels["E1"])))[1],
                                  Description = "Number of probes to the 1st E1")
  EPG_parameters["n_F",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["F"]),
                             Description = "Number of F")
  EPG_parameters["s_1Pr",] = c(Value = probe_duration[1],
                               Description = "Duration of 1st probe")
  EPG_parameters["s_2Pr",] = c(Value = probe_duration[2],
                               Description = "Duration of 2nd probe")

  ##### TODO only works with F/G added to C wave
  EPG_parameters["mnt_E1inPr",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E1"])){
    C_preE1 = unlist(sapply(C_waves,function(x) if(any(x[["Time"]] %in% EPG_analysis$Time[pre_E1])){return(sum(x[["Duration"]]))}))[1]
    if(!is.null(C_preE1)){
      C_preE1
    }else{
      NA_integer_
    }
  }else{
    NA_integer_
  },
  Description = "Duration of the shortest C wave before E1")
  #####

  EPG_parameters["s_2np",] = c(Value = EPG_analysis$Duration[which(np_idx)[2]],
                               Description = "Duration of 2nd nonprobe period")
  EPG_parameters["s_F",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["F"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["F"]])
  }else{
    0
  },Description = "Total duration of F")

  EPG_parameters["s_np.1E",] = c(Value = sum(EPG_analysis$Duration[np_idx][EPG_analysis$Time[np_idx] < EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels[c("E1e","E1","E2")]][1]]),
                                 Description = "Duration of nonprobe period before the first E")

  EPG_parameters["a_pd",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L","pd-S-II-2","pd-S-II-3","pd-L-II-2","pd-L-II-3")])){
    mean(sapply(split(which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L","pd-S-II-2","pd-S-II-3","pd-L-II-2","pd-L-II-3")]),cumsum(c(TRUE, diff(which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L","pd-S-II-2","pd-S-II-3","pd-L-II-2","pd-L-II-3")]))!=1))),function(x){
      sum(EPG_analysis$Duration[x])
    }))
  }else{
    NA_integer_
  },
  Description = "Mean duration of pd")

  EPG_parameters["a_pd-L",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-L","pd-L-II-2","pd-L-II-3")])){
    mean(sapply(split(which(EPG_analysis$Waveform %in% waveform_labels[c("pd-L","pd-L-II-2","pd-L-II-3")]),cumsum(c(TRUE, diff(which(EPG_analysis$Waveform %in% waveform_labels[c("pd-L","pd-L-II-2","pd-L-II-3")]))!=1))),function(x){
      sum(EPG_analysis$Duration[x])
    }))
  }else{
    NA_integer_
  },
  Description = "Mean duration of pd-L")

  EPG_parameters["a_pd-S",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")])){
    mean(sapply(split(which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")]),cumsum(c(TRUE, diff(which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")]))!=1))),function(x){
      sum(EPG_analysis$Duration[x])
    }))
  }else{
    NA_integer_
  },
  Description = "Mean duration of pd-S")

  EPG_parameters["n_pd/n_Pr",] = c(Value = mean(sapply(names(probe_wave),function(probe){
    if(any(probe_wave[[probe]] %in% c(8,9,10))){
      length(split(which(probe_wave[[probe]] %in% c(8,9,10)),cumsum(c(TRUE, diff(which(probe_wave[[probe]] %in% c(8,9,10)))!=1))))
    }else{
      0
    }
  })),
  Description = "Average number of pd per probe")

  EPG_parameters["a_F",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["F"])){
    mean(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["F"]])
  }else{
    0
  },Description = "Mean duration of F")

  EPG_parameters["t_1Erec",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")]][1]
  }else{
    max(EPG_analysis$Time)
  },Description = "Time to 1st E from start of EPG (total duration of EPG recording if E is missing)")

  EPG_parameters["t_1Eexp",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")]][1] - probe_time[[1]][1]
  }else{
    max(EPG_analysis$Time) - probe_time[[1]][1]
  },Description = "Time to 1st E from 1st probe (time from 1st probe to the end of EPG recording if E is missing)")

  EPG_parameters["t_1EinPr",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")]][1] -
      probe_time[sapply(probe_wave,function(x) any(x %in% waveform_labels[c("E1","E2")]))][[1]][1]
  }else{
    NA_integer_
  },Description = "Time to first E from the beginning of that probe (missing data if E is missing)")

  EPG_parameters["n_G",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["G"]),
                             Description = "Number of G")

  EPG_parameters["s_G",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["G"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["G"]])
  }else{
    0
  },Description = "Duration of G")

  EPG_parameters["a_G",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["G"])){
    mean(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["G"]])
  }else{
    0
  },Description = "Mean duration of G")

  EPG_parameters["n_Pr.after1E",] = c(Value = length(probe_wave) - which(sapply(names(probe_wave),function(x) any(probe_wave[[x]] %in% waveform_labels[c("E1","E2")])))[1],
                                      Description = "Number of probes after 1st E")

  EPG_parameters["n_bPr.after1E",] = c(Value = if(any(waveform_labels[c("E1","E1e")] %in% EPG_analysis$Waveform)){
    sum(probe_duration[names(probe_wave)[sapply(probe_wave,function(x){
      any(x %in% waveform_labels[c("E1","E2","E1e")])
    })][1]:length(probe_duration)] < 3*60)
  }else{
    NA_integer_
  },
  Description = "Number of probes (shorter than 3 minutes) after 1st E")

  EPG_parameters["n_E1",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["E1"]),
                              Description = "Number of E1")

  EPG_parameters["n_lE1followedbyE2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E1"])){
    sum(EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) + 1] - EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels["E1"]] > 10*60 &
          EPG_analysis$Waveform[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) + 1] %in% waveform_labels["E2"])
  }else{
    0
  },Description = "Number of E1 (longer than 10 minutes) followed by E2")

  EPG_parameters["n_sgE1",] = c(Value = sum(!EPG_analysis$Waveform[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) + 1] %in% waveform_labels["E2"] &
                                              !EPG_analysis$Waveform[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) - 1] %in% waveform_labels["E2"]),
                                Description = "Number of single E1")

  EPG_parameters["n_E2",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["E2"]),
                              Description = "Number of E2")

  EPG_parameters["n_sE2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    sum(EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels["E2"]) + 1] -
          EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels["E2"]] > 10*60)
  }else{
    0
  },Description = "Number of sustained E2 (longer than 10 minutes)")

  EPG_parameters["d_1E",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")])){
    EPG_analysis$Time[!EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")] & EPG_analysis$Time > EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")])[1]]][1] -
      EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")])[1]]
  }else{
    0
  },Description = "Duration of first E")

  EPG_parameters["%_E1/E12",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E1"]])/
      sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")]])*100
  }else{
    NA_integer_
  },Description = "Contribution of E1 to phloem phase (%)")

  EPG_parameters["d_E1followedby1sE2",] = c(Value = if(any(E2_postE1_sus)){
    EPG_analysis$Duration[which(E2_postE1_sus) - 1][1]
  }else{
    0
  },Description = "Duration of E1 followed by first sustained E2 (>10 min)")

  EPG_parameters["d_E1followedby1E2",] = c(Value = if(any(E2_postE1)){
    EPG_analysis$Duration[which(E2_postE1) - 1][1]
  }else{
    0
  },Description = "Duration of E1 followed by the first E2")

  EPG_parameters["E2index",] = c(Value = 100*sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E2"]])/
                                   (sum(EPG_analysis$Duration)-EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels["E2"]][1]),
                                 Description = "Potential E2 index")

  EPG_parameters["s_E",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")]])
  }else{
    0
  },Description = "Total duration of E")

  EPG_parameters["s_E1",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E1"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E1"]])
  }else{
    0
  },Description = "Total duration of E1")

  EPG_parameters["s_E1followedbysE2",] = c(Value = if(any(E2_postE1_sus)){
    sum(EPG_analysis$Duration[which(E2_postE1_sus) - 1])
  }else{
    0
  },Description = "Total duration of E1 followed by sustained E2 (>10 min)")

  EPG_parameters["s_E1followedbyE2",] = c(Value = if(!all(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")])){
    sum(EPG_analysis$Duration[which(E2_postE1) - 1])
  }else{
    0
  },Description = "Total duration of E1 followed by E2")

  EPG_parameters["s_sgE1",] = c(Value = sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E1"]][!EPG_analysis$Waveform[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) + 1] %in% waveform_labels["E2"] & !EPG_analysis$Waveform[which(EPG_analysis$Waveform %in% waveform_labels["E1"]) - 1] %in% waveform_labels["E2"]]),
                                Description = "Total duration of single E1")

  EPG_parameters["s_E12",] = c(Value = if(!all(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2")])){
    sum(EPG_analysis$Duration[c(which(E2_postE1),which(E2_postE1) - 1)])
  }else{
    0
  },Description = "Duration of E1 followed by E2 and E2")

  EPG_parameters["s_E2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E2"]])
  }else{
    0
  },Description = "Total duration of E2")

  EPG_parameters["a_E1",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E1"])){
    mean(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E1"]])
  }else{
    0
  },Description = "Mean duration of E1")

  EPG_parameters["a_E2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    mean(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E2"]])
  }else{
    0
  },Description = "Mean duration of E2")

  EPG_parameters["n_Pr",] = c(Value = length(probe_wave)
                              ,Description = "Number of probes")

  EPG_parameters["n_C",] = c(Value = length(C_waves),
                             Description = "Number of C")

  EPG_parameters["n_bPr",] = c(Value = sum(probe_duration < 3*60)
                               ,Description = "Number of short probes (<3 minutes)")

  EPG_parameters["n_Np",] = c(Value = sum(np_idx)
                              ,Description = "Number of np")

  EPG_parameters["n_pd",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L")]),
                              Description = "Number of pd")

  EPG_parameters["n_pd-L",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["pd-L"]),
                                Description = "Number of pd-L")

  EPG_parameters["n_pd-S",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["pd-S"]),
                                Description = "Number of pd-S")

  EPG_parameters["n_E1e",] = c(Value = sum(EPG_analysis$Waveform %in% waveform_labels["E1e"]),
                               Description = "Number of E1e")

  EPG_parameters["s_C",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["C"])){
    sum(unlist(sapply(C_waves,function(x) x[["Duration"]])))
  }else{
    0
  },Description = "Total duration of C")

  EPG_parameters["s_E1e",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E1e"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E1e"]])
  }else{
    0
  },Description = "Total duration of E1e")

  EPG_parameters["s_nE",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")])){
    max(EPG_analysis$Time) -
      sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("E1","E2","E1e")]])
  }else{
    max(EPG_analysis$Time)
  },Description = "Total duration of no phloematic phase")

  EPG_parameters["s_np",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["np"])){
    sum(EPG_analysis$Duration[np_idx])
  }else{
    0
  },Description = "Total duration of np")

  EPG_parameters["s_pd",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L","pd-S-II-2","pd-S-II-3","pd-L-II-2","pd-L-II-3")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L","pd-S-II-2","pd-S-II-3","pd-L-II-2","pd-L-II-3")]])
  }else{
    0
  },Description = "Total duration of pd")

  EPG_parameters["s_pd-L",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-L","pd-L-II-2","pd-L-II-3")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("pd-L","pd-L-II-2","pd-L-II-3")]])
  }else{
    0
  },Description = "Total duration of pd-L")

  EPG_parameters["s_pd-S",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")]])
  }else{
    0
  },Description = "Total duration of pd-S")

  EPG_parameters["s_Pr",] = c(Value = sum(probe_duration)
                              ,Description = "Total probing time")

  EPG_parameters["a_Np",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["np"])){
    mean(EPG_analysis$Duration[np_idx])
  }else{
    0
  },Description = "Mean duration of np")

  EPG_parameters["a_C",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["C"])){
    mean(unlist(sapply(C_waves,function(x) sum(x[["Duration"]]))))
  }else{
    0
  },Description = "Mean duration of C")

  EPG_parameters["t_1sE2.rec",] = c(Value = if(any(E2_sus)){
    EPG_analysis$Time[E2_sus][1]
  }else{
    max(EPG_analysis$Time)
  },Description = "Time to first sustained E2 (longer than 10 minutes) from start of EPG")

  EPG_parameters["t_1sE2.exp",] = c(Value = if(any(E2_sus)){
    EPG_analysis$Time[E2_sus][1] -
      probe_time[[1]][1]
  }else{
    max(EPG_analysis$Time)
  },Description = "Time to first sustained E2 (longer than 10 minutes) from the first probe")

  EPG_parameters["t_1sE2inPr",] = c(Value = if(any(E2_sus)){
    EPG_analysis$Time[E2_sus][1] -
      probe_time[sapply(probe_time,function(x) EPG_analysis$Time[E2_sus][1] %in% x)][[1]][1]
  }else{
    NA_integer_
  },Description = "Time to first sustained E2 (longer than 10 minutes) from that probe")

  EPG_parameters["t_1E2.rec",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels["E2"]][1]
  }else{
    max(EPG_analysis$Time)
  },Description = "Time to first E2 from start of EPG")

  EPG_parameters["t_1E2.exp",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels["E2"]][1] - probe_time[[1]][1]
  }else{
    max(EPG_analysis$Time)
  },Description = "Time to first E2 from the first probe")

  EPG_parameters["t_1E2inPr",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels["E2"]][1] - probe_time[sapply(probe_wave,function(x)any(x %in% waveform_labels["E2"]))][[1]][1]
  }else{
    NA_integer_
  },Description = "Time to first E2 from that probe")

  # Function for calculating duration/mean/number per hour
  hourly_duration = function(wave,hour_start,hour_end){
    res = list()
    res[["duration"]] = EPG_analysis$Duration[EPG_analysis$Time >= hour_start*60*60 &
                                                EPG_analysis$Time < hour_end*60*60 &
                                                EPG_analysis$Waveform %in% wave]

    num_waves = sum(EPG_analysis$Time >= hour_start*60*60 &
                      EPG_analysis$Time < hour_end*60*60 &
                      EPG_analysis$Waveform %in% wave[1])

    res[["pre"]] = if(hour_start > 0){
      if(EPG_analysis$Waveform[which(EPG_analysis$Time >= hour_start*60*60)[1] - 1] %in% wave & EPG_analysis$Time[EPG_analysis$Time >= hour_start*60*60][1] > hour_start*60*60){
        EPG_analysis$Time[EPG_analysis$Time >= hour_start*60*60][1] - hour_start*60*60
      }else{
        0
      }
    }else{
      0
    }
    res[["post"]] = if(any(EPG_analysis$Time > hour_end*60*60)){
      if(EPG_analysis$Waveform[max(which(EPG_analysis$Time < hour_end*60*60))] %in% wave & EPG_analysis$Time[max(which(EPG_analysis$Time < hour_end*60*60)) + 1] > hour_end*60*60){
        EPG_analysis$Time[max(which(EPG_analysis$Time < hour_end*60*60)) + 1] - hour_end*60*60
      }else{
        0
      }
    }else{
      0
    }
    res_sum = sum(res[["duration"]]) + res[["pre"]] - res[["post"]]
    return(c(sum = res_sum,
             length = num_waves,
             mean = if(num_waves > 0){
               res_sum / num_waves
             }else{
               0
             }))
  }

  EPG_parameters["s_Np.in1stH",] = c(Value = hourly_duration(wave = waveform_labels["np"],hour_start = 0,hour_end = 1)["sum"],
                                     Description = "Total duration of np during the first hour")

  EPG_parameters["s_Np.in2ndH",] = c(Value = hourly_duration(wave = waveform_labels["np"],hour_start = 1,hour_end = 2)["sum"],
                                     Description = "Total duration of np during the second hour")

  EPG_parameters["s_Np.in3rdH",] = c(Value = hourly_duration(wave = waveform_labels["np"],hour_start = 2,hour_end = 3)["sum"],
                                     Description = "Total duration of np during the third hour")

  EPG_parameters["s_Np.in4thH",] = c(Value = hourly_duration(wave = waveform_labels["np"],hour_start = 3,hour_end = 4)["sum"],
                                     Description = "Total duration of np during the fourth hour")

  EPG_parameters["s_Np.in5thH",] = c(Value = hourly_duration(wave = waveform_labels["np"],hour_start = 4,hour_end = 5)["sum"],
                                     Description = "Total duration of np during the fifth hour")

  EPG_parameters["s_Np.in6thH",] = c(Value = hourly_duration(wave = waveform_labels["np"],hour_start = 5,hour_end = 6)["sum"],
                                     Description = "Total duration of np during the sixth hour")

  EPG_parameters["n_pd-S.in1stH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 0,hour_end = 1)["length"],
                                       Description = "Number of pd-S during the first hour")

  EPG_parameters["n_pd-S.in2ndH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 1,hour_end = 2)["length"],
                                       Description = "Number of pd-S during the second hour")

  EPG_parameters["n_pd-S.in3rdH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 2,hour_end = 3)["length"],
                                       Description = "Number of pd-S during the third hour")

  EPG_parameters["n_pd-S.in4thH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 3,hour_end = 4)["length"],
                                       Description = "Number of pd-S during the fourth hour")

  EPG_parameters["n_pd-S.in5thH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 4,hour_end = 5)["length"],
                                       Description = "Number of pd-S during the fifth hour")

  EPG_parameters["n_pd-S.in6thH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 5,hour_end = 6)["length"],
                                       Description = "Number of pd-S during the sixth hour")

  EPG_parameters["s_pd-S.in1stH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 0,hour_end = 1)["mean"],
                                       Description = "Average duration of pd-S during the first hour")

  EPG_parameters["s_pd-S.in2ndH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 1,hour_end = 2)["mean"],
                                       Description = "Average duration of pd-S during the second hour")

  EPG_parameters["s_pd-S.in3ndH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 2,hour_end = 3)["mean"],
                                       Description = "Average duration of pd-S during the third hour")

  EPG_parameters["s_pd-S.in4thH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 3,hour_end = 4)["mean"],
                                       Description = "Average duration of pd-S during the fourth hour")

  EPG_parameters["s_pd-S.in5thH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 4,hour_end = 5)["mean"],
                                       Description = "Average duration of pd-S during the fifth hour")

  EPG_parameters["s_pd-S.in6thH",] = c(Value = hourly_duration(wave = waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3")],hour_start = 5,hour_end = 6)["mean"],
                                       Description = "Average duration of pd-S during the sixth hour")

  EPG_parameters["n_F.in1stH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 0,hour_end = 1)["length"],
                                    Description = "Number of F during the first hour")

  EPG_parameters["n_F.in2ndH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 1,hour_end = 2)["length"],
                                    Description = "Number of F during the second hour")

  EPG_parameters["n_F.in3rdH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 2,hour_end = 3)["length"],
                                    Description = "Number of F during the third hour")

  EPG_parameters["n_F.in4thH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 3,hour_end = 4)["length"],
                                    Description = "Number of F during the fourth hour")

  EPG_parameters["n_F.in5thH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 4,hour_end = 5)["length"],
                                    Description = "Number of F during the fifth hour")

  EPG_parameters["n_F.in6thH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 5,hour_end = 6)["length"],
                                    Description = "Number of F during the sixth hour")

  EPG_parameters["s_F.in1stH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 0,hour_end = 1)["sum"],
                                    Description = "Total duration of F during the first hour")

  EPG_parameters["s_F.in2ndH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 1,hour_end = 2)["sum"],
                                    Description = "Total duration of F during the second hour")

  EPG_parameters["s_F.in3rdH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 2,hour_end = 3)["sum"],
                                    Description = "Total duration of F during the third hour")

  EPG_parameters["s_F.in4thH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 3,hour_end = 4)["sum"],
                                    Description = "Total duration of F during the fourth hour")

  EPG_parameters["s_F.in5thH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 4,hour_end = 5)["sum"],
                                    Description = "Total duration of F during the fifth hour")

  EPG_parameters["s_F.in6thH",] = c(Value = hourly_duration(wave = waveform_labels["F"],hour_start = 5,hour_end = 6)["sum"],
                                    Description = "Total duration of F during the sixth hour")

  # Function for number of probes per hour
  hourly_probe = function(hour_start,hour_end){
    if(any(EPG_analysis$Time > hour_start*60*60)){
      probe_num = probe_time[sapply(probe_time,function(x){
        any(x > hour_start*60*60) & any(x < hour_end*60*60)
      })]
      if(length(probe_num) > 0){
        if(all(probe_num[[1]] > hour_start*60*60) & EPG_analysis$Time[match(probe_num[[1]][1],table = EPG_analysis$Time) - 1] > hour_start*60*60){
          return(length(probe_num) + 1)
        }else{
          return(length(probe_num))
        }
      }else{
        return(0)
      }
    }else{
      return(NA_integer_)
    }
  }

  EPG_parameters["n_Pr.in1stH",] = c(Value = hourly_probe(hour_start = 0,hour_end = 1),
                                     Description = "Number of probes during the first hour")

  EPG_parameters["n_Pr.in2ndH",] = c(Value = hourly_probe(hour_start = 1,hour_end = 2),
                                     Description = "Number of probes during the second hour")

  EPG_parameters["n_Pr.in3rdH",] = c(Value = hourly_probe(hour_start = 2,hour_end = 3),
                                     Description = "Number of probes during the third hour")

  EPG_parameters["n_Pr.in4thH",] = c(Value = hourly_probe(hour_start = 3,hour_end = 4),
                                     Description = "Number of probes during the fourth hour")

  EPG_parameters["n_Pr.in5thH",] = c(Value = hourly_probe(hour_start = 4,hour_end = 5),
                                     Description = "Number of probes during the fifth hour")

  EPG_parameters["n_Pr.in6thH",] = c(Value = hourly_probe(hour_start = 5,hour_end = 6),
                                     Description = "Number of probes during the sixth hour")

  EPG_parameters["t_1C.1pd",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")])){
    EPG_analysis$Time[EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")]][1] - probe_time[[1]][1]
  }else{
    NA_integer_
  },Description = "Time to first pd from the beginning of the first probe")

  ## TODO Mismatch with Excel sheet v4.4.3
  EPG_parameters["t_endLpd.endC.in1Pr",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")])){
    pd_probes = sapply(probe_wave,function(x) any(x %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")]))
    last_pd_probe = probe_time[[names(pd_probes[length(pd_probes)])]]
    names(last_pd_probe) = probe_wave[[names(pd_probes[length(pd_probes)])]]
    max(last_pd_probe) - max(last_pd_probe[which(names(last_pd_probe) %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")]) + 1])
  }else{
    NA_integer_
  },Description = "Time from the end of the last pd to the end of the probe")

  EPG_parameters["s_pdII-1",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-L")]])
  }else{
    0
  },Description = "Total duration of pd-II-1")

  EPG_parameters["s_pdII-2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-2","pd-L-II-2")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-2","pd-L-II-2")]])
  }else{
    0
  },Description = "Total duration of pd-II-2")

  EPG_parameters["s_pdII-3",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-3","pd-L-II-3")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("pd-S-II-3","pd-L-II-3")]])
  }else{
    0
  },Description = "Total duration of pd-II-3")

  EPG_parameters["t_endLpd.E1followedbysE2",] = c(Value = if(!is.null(pd_E1E2sus)){
    min(EPG_analysis$Time[which(E2_postE1_sus) - 1][EPG_analysis$Time[which(E2_postE1_sus) - 1] > pd_E1E2sus]) -
      EPG_analysis$Time[which(EPG_analysis$Time %in% pd_E1E2sus) + 1]
  }else{
    0
  },Description = "Time from the end of the last pd to the beginning of the E1 followed by the sustained E2 (>10 min)")

  EPG_parameters["t_endLpd.Z",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")])){
    max(EPG_analysis$Time) -
      max(EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels[c("pd-S","pd-S-II-2","pd-S-II-3","pd-L","pd-L-II-2","pd-L-II-3")]) + 1])
  }else{
    0
  },Description = "Time from the end of the last pd to the end of the EPG record (Z)")

  EPG_parameters["t_LE1.Z",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E1"])){
    max(EPG_analysis$Time) -
      max(EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels["E1"])])
  }else{
    0
  },Description = "Time from the beginning of the last E1 to the end of the EPG record (Z)")

  EPG_parameters["t_LE2.Z",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    max(EPG_analysis$Time) -
      max(EPG_analysis$Time[which(EPG_analysis$Waveform %in% waveform_labels["E2"])])
  }else{
    0
  },Description = "Time from the beginning of the last E2 to the end of the EPG record (Z)")

  EPG_parameters["s_longestE2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    max(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E2"]])
  }else{
    0
  },Description = "Duration of the longest E2")

  EPG_parameters["s_1npafter1sE2",] = c(Value = if(any(E2_sus) & any(np_idx) & any(EPG_analysis$Time[np_idx] > EPG_analysis$Time[E2_sus][1])){
    min(EPG_analysis$Duration[np_idx][EPG_analysis$Time[np_idx] > max(probe_time[sapply(probe_time,function(x) any(x %in% EPG_analysis$Time[E2_sus][1]))][[1]])])
  }else{
    0
  },Description = "Duration of np just after the probe of the first sustained E2")

  EPG_parameters["s_1npafter1sE2followedbyend",] = c(Value = if(any(E2_sus) & any(np_idx) & any(EPG_analysis$Time[np_idx] > EPG_analysis$Time[E2_sus][1]) & EPG_analysis$Waveform[max(which(EPG_analysis$Waveform %in% waveform_labels[names(waveform_labels) %in% "end"])) - 1] == waveform_labels["np"]){
    min(EPG_analysis$Duration[np_idx][EPG_analysis$Time[np_idx] > max(probe_time[sapply(probe_time,function(x) any(x %in% EPG_analysis$Time[E2_sus][1]))][[1]])])
  }else{
    0
  },Description = "Duration of np just after the probe of the first sustained E2 if it lasts until the end of the recording")

  EPG_parameters["%probtimeinC",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("C","pd-L","pd-L-II-2","pd-L-II-3","pd-S","pd-S-II-2","pd-S-II-3")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("C","pd-L","pd-L-II-2","pd-L-II-3","pd-S","pd-S-II-2","pd-S-II-3")]])/
      sum(probe_duration)*100
  }else{
    0
  },Description = "% of probing spent in C")


  EPG_parameters["%probtimeinE1",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels[c("E1e","E1")])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels[c("E1e","E1")]])/
      sum(probe_duration)*100
  }else{
    0
  },Description = "% of probing spent in E1")

  EPG_parameters["%probtimeinE2",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["E2"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["E2"]])/
      sum(probe_duration)*100
  }else{
    0
  },Description = "% of probing spent in E2")

  EPG_parameters["%probtimeinF",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["F"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["F"]])/
      sum(probe_duration)*100
  }else{
    0
  },Description = "% of probing spent in F")

  EPG_parameters["%probtimeinG",] = c(Value = if(any(EPG_analysis$Waveform %in% waveform_labels["G"])){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% waveform_labels["G"]])/
      sum(probe_duration)*100
  }else{
    0
  },Description = "% of probing spent in G")

  EPG_parameters["%sE2/E2",] = c(Value = if(sum(EPG_analysis$Waveform %in% waveform_labels["E2"]) > 0){
    sum(E2_sus)/
      sum(EPG_analysis$Waveform %in% waveform_labels["E2"])*100
  }else{
    0
  },
  Description = "% E2>10 min")

  # Clay added
  EPG_parameters["%timeinF+G+np",] = c(Value = if(any(EPG_analysis$Waveform %in% c(waveform_labels["np"], waveform_labels["G"], waveform_labels["F"]))){
    sum(EPG_analysis$Duration[EPG_analysis$Waveform %in% c(waveform_labels["np"], waveform_labels["G"], waveform_labels["F"])])/
      EPG_analysis$Time[EPG_analysis$Waveform == waveform_labels['end']]*100
  }else{
    0
  },Description = "% of total time spent in F, G, and NP")

  EPG_parameters$Value[!is.na(EPG_parameters$Value)] = as.numeric(round(as.numeric(EPG_parameters$Value[!is.na(EPG_parameters$Value)]),digits = 2))
  return(EPG_parameters)
}
