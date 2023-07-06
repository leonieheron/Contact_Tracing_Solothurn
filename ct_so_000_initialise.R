#' ---
#' title: "Initialize"
#' author: "Leonie Heron"
#' date: "21/12/2022"
#' ---

#' Load packages
#' 
library(eeptools)
library(expss)
library(readxl)
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyverse)
library(arsenal)
library(naniar)
library(zoo)
require(tidyverse)
library(summarytools)
library(table1) 
library(skimr)
library(janitor)
library(rstatix)
library(Amelia)
library(vtable)
library(viridis)
library(ggpubr)
library(DiagrammeR)
library(tidygraph)
library(igraph)
library(scales)
library(tsibble)
library(reshape2)
library(officer)

#' Tag
timestamp = gsub("-| |:","",Sys.time())

#' Paths
path_function = "R/"
path_saveoutput = "output/"


#' Add custom functions to env
files_functions = list.files(file.path(path_function),full.names = TRUE, include.dirs = TRUE)
files_functions = files_functions[!grepl(pattern = "000|backup",files_functions)]
sapply(files_functions, source)
rm(files_functions)

#' Graphics
theme_set(theme_bw())
# Colourblind palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Colourblind palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



