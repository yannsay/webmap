# click the "Run" button above or
# highlight the next lines and press ctrl+ENTER
rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./dependencies/start.R", local = T)


