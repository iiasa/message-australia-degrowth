try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
library(here)
here::i_am("message-australia-degrowth.Rproj")

# import functions from other files
source("utils.R")
load_pkgs()

version <- "publication" 
