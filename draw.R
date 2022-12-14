draw <- function() {

  ################################################################################
  # plot title, color
  ratio <- c("負債佔資產比率", "長期資金佔固定資產比率", "流動比率", "速動比率",
             "利息保障倍數", "應收帳款週轉率", "平均收現日數", "存貨週轉率", "平均售貨日數",
             "固定資產週轉率", "總資產週轉率", "資產報酬率", "股東權益報酬率", "純益率", "每股盈餘",
             "現金流量比率", "現金再投資比率")
  # ratio <- c("負債佔資產比率","長期資金佔固定資產比率","流動比率","速動比率",
  #            "利息保障倍數", "應收帳款週轉率")
  
  color <- c("orange", "blue")
  #
  ################################################################################
  #
  # define array
  rnum <- length(ratio) # Ratio - row
  cnum <- length(year)-1 # Year - column
  dnum <- length(stock) # Stock - dimension
  dtax <- array(numeric(), c(rnum, cnum, dnum)) # 3D array
  dimnames(dtax) <- list(ratio, year[2:(cnum+1)], stock) # named array
  #
  ################################################################################
  #
  # ratio calculate
  ry <- 2:length(year)  # draw year
  dy <- 1:(length(year)-1)  # data year
  
  #負債佔資產比率
  Ratio <- balance_dta[which(mAcc_balances == "　負債總額"), ry, ] / (
    balance_dta[which(mAcc_balances == "　資產總額"), ry, ] )
  Ratio <- as.data.frame(t(Ratio))
  dtax[1, , 1] <- unlist(Ratio[1, ])
  dtax[1, , 2] <- unlist(Ratio[2, ])
  
  #長期資金佔固定資產比率
  Ratio <- (balance_dta[which(mAcc_balances == "　權益總額"), ry, ]+balance_dta[which(mAcc_balances == "　　非流動負債合計"), ry, ]) /
    balance_dta[which(mAcc_balances == "　　非流動資產合計"), ry, ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[2, , 1] <- unlist(Ratio[1, ])
  dtax[2, , 2] <- unlist(Ratio[2, ])
  #
  #
  #流動比率
  Ratio <- (balance_dta[which(mAcc_balances == "　　流動資產合計"), ry, ]) /
    balance_dta[which(mAcc_balances == "　　流動負債合計"), ry, ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[3, , 1] <- unlist(Ratio[1, ])
  dtax[3, , 2] <- unlist(Ratio[2, ])

  #速動比率
  Ratio <- (balance_dta[which(mAcc_balances == "　　流動資產合計"), ry, ]-balance_dta[which(mAcc_balances == "　　　存貨"), ry, ]-balance_dta[which(mAcc_balances == "　　　預付款項"), ry, ]) /
    balance_dta[which(mAcc_balances == "　　流動負債合計"), ry, ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[4, , 1] <- unlist(Ratio[1, ])
  dtax[4, , 2] <- unlist(Ratio[2, ])

  #利息保障倍數
  Ratio <- (income_dta[which(mAcc_income == "稅前淨利（淨損）"), ry, ]+income_dta[which(mAcc_income == "所得稅費用（利益）合計"), ry, ]) /
    cash_dta[which(mAcc_cash == "　　　利息費用"), ry, ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[5, , 1] <- unlist(Ratio[1, ])
  dtax[5, , 2] <- unlist(Ratio[2, ])
  
  #應收帳款週轉率
  Ratio <- income_dta[which(mAcc_income == "　銷貨收入淨額"), ry, ] /
    ((
      balance_dta[which(mAcc_balances == "　　　應收票據淨額"), ry,] +
      balance_dta[which(mAcc_balances == "　　　應收帳款淨額"), ry, ] +
      balance_dta[which(mAcc_balances == "　　　應收帳款－關係人淨額"), ry, ] +
      balance_dta[which(mAcc_balances == "　　　應收票據淨額"), dy,] +
      balance_dta[which(mAcc_balances == "　　　應收帳款淨額"), dy, ] +
      balance_dta[which(mAcc_balances == "　　　應收帳款－關係人淨額"), dy, ]
    ) / 2
    )
  Ratio <- as.data.frame(t(Ratio))
  dtax[6, , 1] <- unlist(Ratio[1, ])
  dtax[6, , 2] <- unlist(Ratio[2, ])
  
  #平均收現日數
  dtax[7, , ] <- 365 / dtax[6, , ]
  
  #存貨週轉率
  Ratio <- income_dta[which(mAcc_income == "　銷貨成本"), ry, ] /
    ((balance_dta[which(mAcc_balances == "　　　存貨"), ry,] + balance_dta[which(mAcc_balances == "　　　存貨"), dy, ]) / 2)
  Ratio <- as.data.frame(t(Ratio))
  dtax[8, , 1] <- unlist(Ratio[1,])
  dtax[8, , 2] <- unlist(Ratio[2,])
  
  #平均售貨日數
  dtax[9, , ] <- 365 / dtax[8, , ]
  
  #固定資產週轉率
  Ratio <- income_dta[which(mAcc_income == "　銷貨收入淨額"), ry, ] /
    balance_dta[which(mAcc_balances == "　　　不動產、廠房及設備"), ry,]
  Ratio <- as.data.frame(t(Ratio))
  dtax[10, , 1] <- unlist(Ratio[1,])
  dtax[10, , 2] <- unlist(Ratio[2,])
  
  #總資產週轉率
  Ratio <- income_dta[which(mAcc_income == "　銷貨收入淨額"), ry, ] /
    balance_dta[which(mAcc_balances == "　資產總額"), ry,]
  Ratio <- as.data.frame(t(Ratio))
  dtax[11, , 1] <- unlist(Ratio[1,])
  dtax[11, , 2] <- unlist(Ratio[2,])
  
  #資產報酬率
  Ratio <- (income_dta[which(mAcc_income == "本期淨利（淨損）"), ry,] + cash_dta[which(mAcc_cash == "　　　利息費用"), ry, ] *
    (1 - (income_dta[which(mAcc_income == "所得稅費用（利益）合計"), ry,] / income_dta[which(mAcc_income == "稅前淨利（淨損）"), ry,]))) /
    ((balance_dta[which(mAcc_balances == "　資產總額"), ry, ] + balance_dta[which(mAcc_balances == "　資產總額"), dy, ]) /
       2)
  # Ratio <- income_dta[which(mAcc_income == "本期淨利（淨損）"), ry,]/
  #   ((balance_dta[which(mAcc_balances == "　資產總額"), ry, ] + balance_dta[which(mAcc_balances == "　資產總額"), dy, ]) /
  #      2)
  Ratio <- as.data.frame(t(Ratio))
  dtax[12, , 1] <- unlist(Ratio[1,])
  dtax[12, , 2] <- unlist(Ratio[2,])
  
  #股東權益報酬率
  Ratio <- income_dta[which(mAcc_income == "本期淨利（淨損）"), ry, ] /
    ((balance_dta[which(mAcc_balances == "　權益總額"), ry,] + balance_dta[which(mAcc_balances == "　權益總額"), dy,]) /
       2)
  Ratio <- as.data.frame(t(Ratio))
  dtax[13, , 1] <- unlist(Ratio[1,])
  dtax[13, , 2] <- unlist(Ratio[2,])
  
  # 純益率
  Ratio <- income_dta[which(mAcc_income == "本期淨利（淨損）"), ry,] /
    income_dta[which(mAcc_income == "　銷貨收入淨額"), ry,]
  Ratio <- as.data.frame(t(Ratio))
  dtax[14, , 1] <- unlist(Ratio[1,])
  dtax[14, , 2] <- unlist(Ratio[2,])
  
  #每股盈餘
  Ratio <- income_dta[which(mAcc_income == "　基本每股盈餘"), ry,]
  Ratio <- as.data.frame(t(Ratio))
  dtax[15, , 1] <- unlist(Ratio[1,])
  dtax[15, , 2] <- unlist(Ratio[2,])
  
  
  #現金流量比率
  Ratio <- (cash_dta[which(mAcc_cash == "營業活動之淨現金流入（流出）"), ry, ]) /
    balance_dta[which(mAcc_balances == "　　流動負債合計"), ry, ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[16, , 1] <- unlist(Ratio[1, ])
  dtax[16, , 2] <- unlist(Ratio[2, ])

  #現金再投資比率
  Ratio <- (cash_dta[which(mAcc_cash == "營業活動之淨現金流入（流出）"), ry, ]-cash_dta[which(mAcc_cash == "　發放現金股利"), ry, ]) /
    (balance_dta[which(mAcc_balances == "　　非流動資產合計"), ry, ]+balance_dta[which(mAcc_balances == "　　流動資產合計"), ry, ]-balance_dta[which(mAcc_balances == "　　流動負債合計"), ry, ])
  Ratio <- as.data.frame(t(Ratio))
  dtax[17, , 1] <- unlist(Ratio[1, ])
  dtax[17, , 2] <- unlist(Ratio[2, ])
  
  
  #
  ################################################################################
  #
  # draw
  # print(dtax)
  for (r in 1:rnum) {
    Yrange <- c(min(dtax[r, , ]) / 1.1, max(dtax[r, , ]) * 1.1) # Y
    #print(paste("r: ",r))
    for (d in 1:dnum) {
      #print(paste("d: ",d))
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
      1, 1:(length(year)-1),
      format(as.Date(as.character(year[2:6]), format = "%Y"), "%Y")
    ) # X
  }
  
  return(dtax)
}
