#' Read multiple .ANA files from a single directory.
#'
#' @param filenames Vector (character); names of .ANA files.
#' @param directory Character; directory where .ANA files are located.
#' @param validate Logical; If TRUE (defualt), performs data validation.
#' @param waveform_labels Named vector; values are waveform numbers and names are waveform labels.
#' @param filepaths_shiny String; Special argument only used in shiny app. Leave as default (NULL).
#' @return A list of lists. Each sub-list is of length 2, with 'data' and 'errors' objects (see 'read_ana' function description).
#' @examples
#'
combine_ana <- function(filenames, directory, validate=TRUE, waveform_labels, filepaths_shiny=NULL) {

  if(is.null(filepaths_shiny)) {
    filepaths <- paste0(directory, filenames)
  } else {
    filepaths <- filepaths_shiny
  }

  datlist <- lapply(1:length(filepaths), function(i) {
    out <- read_ana(filepath = filepaths[i], filename_shiny=filenames[i], validate = TRUE, waveform_labels = waveform_labels)
    return(list(dat=if(is.null(out$data)) NULL else data.frame(filename = filenames[i], out$data),
                errors=out$errors))
  })

  data <- do.call(rbind, lapply(datlist, function(x) x$dat))

  errrorFiles <- filenames[!sapply(datlist, function(x) is.null(x$errors))]
  errors <- do.call(c, lapply(datlist, function(x) x$errors))

  datlist <- list(data=data, errors=errors)
  return(datlist)
}
