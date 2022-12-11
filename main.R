rm(list = ls())
library(readr)
library(scales)

root <- "~/Github/FinancialReportAnalyzer"
setwd(root)

################################################################################
#
# 設置 - 股票代號、報表年度、會計科目
stock <- c("1304", "1308")

year <- c(2017:2021)

mAcc_income <- c(
    "　銷貨收入淨額", "　銷貨成本", "營業毛利（毛損）", "　營業費用合計", "營業利益（損失）",
    "　營業外收入及支出合計", "稅前淨利（淨損）", "所得稅費用（利益）合計",
    "本期淨利（淨損）", "　其他綜合損益（淨額）", "本期綜合損益總額"
)

mAcc_balances <- c(
    "　　流動資產合計", "　　非流動資產合計", "　　　應付帳款",
    "　　　其他流動負債", "　　流動負債合計", "　　非流動負債合計", "　　非流動負債合計",
    "　負債總額", "　　　　普通股股本", "　　　股本合計", "　　　資本公積合計", "　　　保留盈餘合計",
    "　　　其他權益合計", "　權益總額"
)

mAcc_cash <- c(
  "　繼續營業單位稅前淨利（淨損）", "　本期稅前淨利（淨損）", "　營運產生之現金流入（流出）",
  "　收取之利息", "　支付之利息", "　退還（支付）之所得稅", "營業活動之淨現金流入（流出）")

#
################################################################################

# define source
source("~/Github/FinancialReportAnalyzer/income_read.R")
source("~/Github/FinancialReportAnalyzer/balance_read.R")
source("~/Github/FinancialReportAnalyzer/cash_read.R")

source("~/Github/FinancialReportAnalyzer/income_draw.R")

source("~/Github/FinancialReportAnalyzer/analyze.R")

# call
income_dta <- income_read()
balance_dta <- balance_read()
cash_dta <- cash_read()

ratio_dta1 <- income_draw()

# analyze
analyze()
