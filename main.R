rm(list = ls())
library(readr)
library(scales)
library(showtext)
showtext_auto(enable = TRUE)

root <- "~/Github/FinancialReportAnalyzer"
setwd(root)

################################################################################
#
# 設置 - 股票代號、報表年度、會計科目
stock <- c("1304", "1308")

year <- c(2016:2021)

mAcc_income <- c(
  "　銷貨收入淨額",
  "營業收入合計",
  "　銷貨成本",
  "營業成本合計",
  "營業毛利（毛損）"
  ,
  "　營業費用合計",
  "營業利益（損失）",
  "　營業外收入及支出合計",
  "稅前淨利（淨損）"
  ,
  "所得稅費用（利益）合計",
  "繼續營業單位本期淨利（淨損）",
  "　停業單位損益合計"
  ,
  "本期淨利（淨損）",
  "　其他綜合損益（淨額）",
  "本期綜合損益總額",
  "　基本每股盈餘"
)

mAcc_balances <- c(
  "　　　應收票據淨額",
  "　　　應收帳款淨額",
  "　　　應收帳款－關係人淨額",
  "　　　其他應收款淨額",
  "　　　其他應收款－關係人淨額",
  "　　　存貨",
  "　　　預付款項",
  "　　流動資產合計",
  "　　　不動產、廠房及設備",
  "　　非流動資產合計",
  "　資產總額",
  "　　　應付帳款",
  "　　　其他流動負債",
  "　　流動負債合計",
  "　　非流動負債合計",
  "　負債總額",
  "　　　　普通股股本",
  "　　　股本合計",
  "　　　資本公積合計",
  "　　　保留盈餘合計",
  "　　　其他權益合計",
  "　　歸屬於母公司業主之權益合計",
  "　　非控制權益",
  "　權益總額"
)

mAcc_cash <- c(
  "　繼續營業單位稅前淨利（淨損）",
  "　本期稅前淨利（淨損）",
  "　　　利息費用",
  "　營運產生之現金流入（流出）",
  "　收取之利息",
  "　支付之利息",
  "　退還（支付）之所得稅",
  "營業活動之淨現金流入（流出）",
  "　發放現金股利"
)

#
################################################################################

# define source
source("~/Github/FinancialReportAnalyzer/income_read.R")
source("~/Github/FinancialReportAnalyzer/balance_read.R")
source("~/Github/FinancialReportAnalyzer/cash_read.R")

source("~/Github/FinancialReportAnalyzer/draw.R")

source("~/Github/FinancialReportAnalyzer/analyze.R")

# call
income_dta <- income_read()
balance_dta <- balance_read()
cash_dta <- cash_read()

ratio_dta1 <- draw()


# analyze
analyze()

