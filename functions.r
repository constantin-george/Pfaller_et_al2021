## Load libraries
lib.fn <- function(){
	library(readxl)
	library(MASS)
	library(stats)
	library(tidyverse)
	library(stringr)
	library(bbmle)
	library(AER)
	library(sjPlot)
	library(glmmTMB)
	library(jtools)
	library(multcomp)
	library(ggplot2)
	library(reshape2)
	library(DHARMa)
	library(broom.mixed)
	library(AICcmodavg)
	library(MuMIn)
	library(effects)
	library(emmeans)
	library(ggthemes)
	library(patchwork)
}; lib.fn()

## Import data from Excel files
fun <- new.env() # Create environments.
fun$importExcel <- function(fileName, sheetNames, nskip=0){ # Create a function to call in various tabs in Excel
  df <- lapply(sheetNames, function(x){
    read_excel(fileName, sheet=x, skip=nskip, col_types = "text")
  })
  names(df) = sheetNames
  df
}
