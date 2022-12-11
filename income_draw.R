income_draw <- function() {

  ################################################################################
  # plot title, color
  ratio <- c("純益率 (%)")
  color <- c("orange", "blue")
  #
  ################################################################################
  #
  # define array
  rnum <- length(ratio) # Ratio - row
  cnum <- length(dir()) # Year - column
  dnum <- length(stock) # Stock - dimension
  dtax <- array(numeric(), c(rnum, cnum, dnum)) # 3D array
  dimnames(dtax) <- list(ratio, year, stock) # named array
  #
  ################################################################################
  #
  # ratio calculate

  #應收款項週轉率=銷貨淨額÷各期平均應收款項餘額
  for (y in 1:cnum) {
    
  }
  Ratio <- income_dta[which(mAcc_income == "　銷貨收入淨額"), y, ] /
    (((cash_dta[which(mAcc_cash == "　　　　應收帳款（增加）減少"), y, ] +
         cash_dta[which(mAcc_cash == "　　　　應收帳款－關係人（增加）減少"), y, ] +
         cash_dta[which(mAcc_cash == "　　　　其他應收款（增加）減少"), y, ] +
         cash_dta[which(mAcc_cash == "　　　　其他應收款－關係人（增加）減少"), y, ]) +
        (cash_dta[which(mAcc_cash == "　　　　應收帳款（增加）減少"), y-1, ] +
           cash_dta[which(mAcc_cash == "　　　　應收帳款－關係人（增加）減少"), y-1, ] +
           cash_dta[which(mAcc_cash == "　　　　其他應收款（增加）減少"), y-1, ] +
           cash_dta[which(mAcc_cash == "　　　　其他應收款－關係人（增加）減少"), y-1, ])
    ) / 2)
  Ratio <- as.data.frame(t(Ratio))
  dtax[1, , 1] <- unlist(Ratio[1, ])
  dtax[1, , 2] <- unlist(Ratio[2, ])
  #
  #平均收現日數=365÷應收款項週轉率
  #
  #存貨週轉率=銷貨成本÷平均存貨額
  #
  #平均售貨日數=365÷存貨週轉率
  #
  #固定資產週轉率=銷貨淨額÷固定資產淨額
  #
  #總資產週轉率=銷貨淨額÷資產總額
  #
  #資產報酬率=[稅後損益+利息費用(1-稅率)]÷平均資產總額
  #
  #股東權益報酬率=稅後損益÷平均股東權益淨額
  #
  # 純益率=本期淨利（淨損）/　銷貨收入淨額
  Ratio <- income_dta[which(mAcc_income=="本期淨利（淨損）"), , ] / 
    income_dta[which(mAcc_income=="　銷貨收入淨額"), , ]
  Ratio <- as.data.frame(t(Ratio))
  dtax[2, , 1] <- unlist(Ratio[1, ])
  dtax[2, , 2] <- unlist(Ratio[2, ])
  #
  #每股盈餘=(稅後淨利-特別股股利)÷加權平均已發行股數
  #
  #
  ################################################################################
  #
  # draw
  for (r in 1:rnum) {
    Yrange <- c(min(dtax[r, , ]) / 1.1, max(dtax[r, , ]) * 1.1) # Y
    for (d in 1:dnum) {
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
      1, 1:length(year),
      format(as.Date(as.character(year), format = "%Y"), "%Y")
    ) # X
  }
  
  return(dtax)
}
