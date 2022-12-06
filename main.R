rm(list = ls())
library(readr)
root <- "~/Github/FinancialReportAnalyzer"
setwd(root)
# setwd("~/Github/FinancialReportAnalyzer")

################################################################################
#
# 設置 - 股票代號、會計科目
stock <- c("1304", "1308")

mAcc_income <- c(
    "　銷貨收入淨額", "營業成本合計", "營業毛利（毛損）", "營業利益（損失）",
    "稅前淨利（淨損）", "所得稅費用（利益）合計",
    "繼續營業單位本期淨利（淨損）", "本期淨利（淨損）", "本期綜合損益總額"
)

mAcc_balances <- c(
    "　　流動資產合計", "　　非流動資產合計", "　資產總額", "　　　應付帳款",
    "　　　其他流動負債", "　　流動負債合計", "　　非流動負債合計",
    "　負債總額", "　　　　普通股股本", "　　　股本合計", "　　　資本公積合計", "　　　保留盈餘合計",
    "　　　其他權益合計", "　權益總額"
)

mAcc_cash <- c()

#
################################################################################

# define source
source("~/Github/FinancialReportAnalyzer/income_read.R")
source("~/Github/FinancialReportAnalyzer/income_draw.R")

# call
income_dta <- income_read()
income_draw()


source("~/Github/FinancialReportAnalyzer/balance_read.R")
source("~/Github/FinancialReportAnalyzer/balance_draw.R")
balance_dta <- balance_read()
balance_draw()
