income_draw <- function() {

  ################################################################################
  # plot title, color
  # ratio <- c("負債佔資產比率","長期資金佔固定資產比率","流動比率","速動比率",
  #            "利息保障倍數","應收帳款週轉率","平均收現日數","存貨週轉率","平均售貨日數"
  #            ,"固定資產週轉率","總資產週轉率","資產報酬率","股東權益報酬率","純益率","每股盈餘",
  #            "現金流量比率","現金再投資比率")
  ratio <- c("負債佔資產比率","長期資金佔固定資產比率","流動比率","速動比率",
             "利息保障倍數")
  
  color <- c("orange", "blue")
  #
  ################################################################################
  #
  # define array
  setwd(root)
  setwd(file.path(stock[1], "csv/comprehensiveincome", fsep = ""))
  rnum <- length(ratio) # Ratio - row
  cnum <- length(dir()) # Year - column
  dnum <- length(stock) # Stock - dimension
  dtax <- array(numeric(), c(rnum, cnum, dnum)) # 3D array
  dimnames(dtax) <- list(ratio, substr(dir(), 6, 9), stock) # named array
  #
  ################################################################################
  #
  # ratio calculate
  
  #負債佔資產比率
  Ratio <- balance_dta[which(mAcc_balances == "　負債總額"), , ] / (
    balance_dta[which(mAcc_balances == "　資產總額"), , ] )
  Ratio <- as.data.frame(t(Ratio))
  dtax[1, , 1] <- unlist(Ratio[1, ])
  dtax[1, , 2] <- unlist(Ratio[2, ])
  
  #長期資金佔固定資產比率
  Ratio <- (balance_dta[which(mAcc_balances == "　權益總額"), , ]+balance_dta[which(mAcc_balances == "　　非流動負債合計"), , ]) /
    balance_dta[which(mAcc_balances == "　　非流動資產合計"), , ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[2, , 1] <- unlist(Ratio[1, ])
  dtax[2, , 2] <- unlist(Ratio[2, ])
  #
  #
  #流動比率
  Ratio <- (balance_dta[which(mAcc_balances == "　　流動資產合計"), , ]) /
    balance_dta[which(mAcc_balances == "　　流動負債合計"), , ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[3, , 1] <- unlist(Ratio[1, ])
  dtax[3, , 2] <- unlist(Ratio[2, ])

  #速動比率
  Ratio <- (balance_dta[which(mAcc_balances == "　　流動資產合計"), , ]-balance_dta[which(mAcc_balances == "　　　存貨"), , ]-balance_dta[which(mAcc_balances == "　　　預付款項"), , ]) /
    balance_dta[which(mAcc_balances == "　　流動負債合計"), , ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[4, , 1] <- unlist(Ratio[1, ])
  dtax[4, , 2] <- unlist(Ratio[2, ])

  #利息保障倍數
  Ratio <- (income_dta[which(mAcc_income == "稅前淨利（淨損）"), , ]+income_dta[which(mAcc_income == "所得稅費用（利益）合計"), , ]) /
    cash_dta[which(mAcc_cash == "　　　利息費用"), , ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[5, , 1] <- unlist(Ratio[1, ])
  dtax[5, , 2] <- unlist(Ratio[2, ])


  # # 純益率=本期淨利（淨損）/　銷貨收入淨額
  # Net_profit_margin <- income_dta[which(mAcc_income=="本期淨利（淨損）"), , ] /
  #   income_dta[which(mAcc_income=="　銷貨收入淨額"), , ]
  # Net_profit_margin <- as.data.frame(t(Net_profit_margin))
  # dtax[14, , 1] <- unlist(Net_profit_margin[1, ])
  # dtax[14, , 2] <- unlist(Net_profit_margin[2, ])
  
  
  # #現金流量比率
  # Ratio <- (cash_dta[which(mAcc_cash == "營業活動之淨現金流入（流出）"), , ]) /
  #   balance_dta[which(mAcc_balances == "　　流動負債合計"), , ]
  # Ratio <- as.data.frame(t(Ratio))
  # dtax[16, , 1] <- unlist(Ratio[1, ])
  # dtax[16, , 2] <- unlist(Ratio[2, ])
  # 
  # #現金再投資比率
  # Ratio <- (cash_dta[which(mAcc_cash == "營業活動之淨現金流入（流出）"), , ]-cash_dta[which(mAcc_cash == "　發放現金股利"), , ]) /
  #   (balance_dta[which(mAcc_balances == "　　非流動資產合計"), , ]+balance_dta[which(mAcc_balances == "　　流動資產合計"), , ]-balance_dta[which(mAcc_balances == "　　流動負債合計"), , ])
  # Ratio <- as.data.frame(t(Ratio))
  # dtax[17, , 1] <- unlist(Ratio[1, ])
  # dtax[17, , 2] <- unlist(Ratio[2, ])
  
  
  #
  ################################################################################
  #
  # draw
  print(dtax)
  for (r in 1:rnum) {
    Yrange <- c(min(dtax[r, , ]) / 1.1, max(dtax[r, , ]) * 1.1) # Y
    print(paste("r: ",r))
    for (d in 1:dnum) {
      print(paste("d: ",d))
      plot(unlist(dtax[r, , d]),
        type = "l", col = paste(color[d]), lwd = 2, main = paste(ratio[r]),
        ylim = Yrange, ylab = "", xlab = "年", xaxt = "n"
      )
      points(unlist(dtax[r, , d]), pch = 16, cex = 1.25, col = paste(color[d]))
      if (d != dnum) {
        par(new = T)
      }
    }
    axis(
      1, 1:length(colnames(income_dta)),
      format(as.Date(colnames(income_dta), format = "%Y"), "%Y")
    ) # X
  }
  
  return(dtax)
}
