rm(list = ls())
library(readr)
setwd("~/Github/FinancialReportAnalyzer")

# if (file.exists("FinancialReportAnalyzer.Rproj")){  # init path
#   source("main_income.R")
# }else{  # set next stock file path
#   setwd("../../")
#   source("main_income.R")
# }

source("~/Github/FinancialReportAnalyzer/income_read.R")
income_read()
