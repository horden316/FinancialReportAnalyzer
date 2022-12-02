rm(list = ls())
library(readr)

################################################################################
#
# 設置 - 股票代號、會計科目
stock = c("1304", "1308")

mAcc = c("營業收入合計", "營業成本合計", "營業毛利（毛損）", "營業利益（損失）"
         , "稅前淨利（淨損）", "所得稅費用（利益）合計"
         , "繼續營業單位本期淨利（淨損）", "本期淨利（淨損）", "本期綜合損益總額")
#
################################################################################
#
# define array
rnum = length(mAcc)  # Account title - row
cnum = length(dir())  # Year - column
dnum = length(stock)  # Stock - dimension
dtax3 = array(numeric(), c(rnum,cnum,dnum))  # 3D array
#
################################################################################
#
# read file
for (d in 1:dnum){  # set path by stock
  if (file.exists("FinancialReportAnalyzer.Rproj")){  # init path
    setwd(file.path(stock[d],"csv/comprehensiveincome", fsep = ""))
  }else{  # set next stock file path
    setwd("../../")
    setwd(file.path(stock[d],"csv/comprehensiveincome", fsep = ""))
  }
  
  for (c in 1:cnum){  # read csv by year
    dta = read_csv(dir()[c])
    colnames(dta) = c("Account",substr(dir()[c],6,9))
    
    for (r in 1:rnum){  # get field by account
      
      # get by main Account title
      Row = which(dta$Account==mAcc[r])  # find row where Account title in mAcc
      dtax3[r,c,d] = as.numeric(dta[Row,2])
      
      #get by sub Account title
      
      
    }
  }
}

# named array
dimnames(dtax3) = list(mAcc, substr(dir(),6,9), stock)



(dtax3)
