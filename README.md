# Disco EPG R-package and Shiny App

<!-- badges: start align="center" -->
  [![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20659422-blue.svg)](https://doi.org/10.5281/zenodo.20659422)
<!-- badges: end -->



## Intro
This is the home page for the discoEPG R-package and shiny app. To use the Shiny App please go to https://nalamlab.shinyapps.io/discoEPG/.

## Installing the R-package Locally
To install the discoEPG R-package locally onto your computer, please make sure that you have installed R and R-studio on your computer.
Next, be sure to install the devtools package and then use the 'install.github()' command to install the package in this repository.
```
# install devtools if you you do not already have it installed.
install.packages("devtools")

# load the package into your environment.
library(devtools)

# install the discoEPG package at this github repo.
install_github("nalamvj/EPG_ANALYSIS/discoEPG")

# next we can load the discoEPG package into our environment.
library(discoEPG)
```
