analyze <- function() {
  #######################
  ###### TEST DATA ######
  s <- 1  #1304 台聚
  y <- 3  #2018
  r <- 13  #ratio
  #######################
  #
  # ratio list
  ratio <- c("負債佔資產比率","長期資金佔固定資產比率","流動比率","速動比率",
             "利息保障倍數","應收帳款週轉率","平均收現日數","存貨週轉率","平均售貨日數"
             ,"固定資產週轉率","總資產週轉率","資產報酬率","股東權益報酬率","純益率","每股盈餘",
             "現金流量比率","現金再投資比率")
  # reset key value
  s <- y <- r <- ""
  #
  ################################# Input Menu #################################
  #
  ###### stock ######
  while ((stock[s] %in% stock) == FALSE) {
    li <- paste0(1:length(stock), sep = ".", stock)
    s <- readline(cat("請輸入要分析的股票代號：", li, sep = "\n"))  # input
    
    if (s == "exit")
      break
    else
      s <- as.numeric(s)
    
    ###### year ######
    while ((stock[s] %in% stock) == TRUE &
           (year[y] %in% year) == FALSE) {
      li <- paste0(1:(length(year) - 2), sep = ".", year[3:length(year)])
      y <- readline(cat("請輸入要分析的年度：", li, sep = "\n"))  # input
      
      if (y == "exit")
        break
      else
        y <- as.numeric(y) + 2
      
      ###### ratio ######
      while ((stock[s] %in% stock) == TRUE &
             (year[y] %in% year) == TRUE &
             (ratio[r] %in% ratio) == FALSE) {
        li <- paste0(1:length(ratio), sep = ".", ratio)
        r <- readline(cat("請輸入要分析的比率：", li, sep = "\n"))  # input
        
        if (r == "exit")
          break
        else
          r <- as.numeric(r)
      }
    }
  }
  ##############################################################################
  #
  ###### magic for analyze ######
  magic_black_box <- function(mAcc) {
    m1 <- m2 <- c("", 0)  # max change (account title, value)
    t2 <- ""  # result
    
    for (m in mAcc) {
      # calculate
      if (m %in% mAcc_income)  {  # check mAcc in income
        rp <- income_dta[which(mAcc_income == m), (y - 1), s]
        rc <- income_dta[which(mAcc_income == m), y, s]
      } else if (m %in% mAcc_balances) {  # check mAcc in balance
        rp <- balance_dta[which(mAcc_balances == m), (y - 1), s]
        rc <- balance_dta[which(mAcc_balances == m), y, s]
      } else if (m %in% mAcc_cash) {  # check mAcc in cash
        rp <- cash_dta[which(mAcc_cash == m), (y - 1), s]
        rc <- cash_dta[which(mAcc_cash == m), y, s]
      } else{
        rp <- rc <- 0
      }
       
        
      
      n <- rc - rp
      
      # add to t2
      if (n > 0)
        t <- paste(m, "與前年相比增加了", percent(n/rp, accuracy = 0.01), "%")
      else if (n < 0)
        t <- paste(m, "與前年相比減少了", percent( abs(n)/rp, accuracy = 0.01), "%")
      else
        t <- paste(m, "與前年相同")
      
      t2 <- paste(t2, t, sep = "\n")
      
      # find 2 max change account title & value
      if (abs(n)/rp > as.numeric(m1[2])) {
        temp <- m1
        m1 <- c(m , abs(n)/rp)
        m2 <- temp
      } else if (abs(n)/rp > as.numeric(m2[2])) {
        m2 <- c(m , abs(n)/rp)
      }
      
    }
    # add max change to t2
    t2 <- paste(t2, "\n 其中變動最大的為", m1[1], "變動第二大的為", m2[1])
    return(t2)
  }
  ##############################################################################
  #
  
  ratio_cr <-
    ratio_dta1[r, y-1, s] - ratio_dta1[r, (y - 2), s]  # ratio change rate
  
  # add to t1
  if (ratio_cr > 0)
    t1 <-
    paste(ratio[r], "增加了", percent(ratio_cr, accuracy = 0.01))
  else if (ratio_cr < 0)
    t1 <-
    paste(ratio[r], "減少了", percent(abs(ratio_cr), accuracy = 0.01))
  else
    t1 <- paste(ratio[r], "不變")
  
  ############################################################################
  #
  
  #負債佔資產比率
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  #長期資金佔固定資產比率
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  #流動比率
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  #速動比率
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  #利息保障倍數
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  #應收帳款週轉率
  if (r == 6) {
    mAcc <- c(
      # 分子
      "　銷貨收入淨額"
      # 分母  
      , "　　　應收票據淨額", "　　　應收帳款淨額")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #平均收現日數
  if (r == 7) {
    mAcc <- c(
      # 分子
      "　銷貨收入淨額"
      # 分母  
      , "　　　應收票據淨額", "　　　應收帳款淨額")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #存貨週轉率
  if (r == 8) {
    mAcc <- c(
      # 分子
      "　銷貨成本"
      # 分母  
      , "　　　存貨")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #平均售貨日數
  if (r == 9) {
    mAcc <- c(
      # 分子
      "　銷貨成本"
      # 分母  
      , "　　　存貨")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #固定資產週轉率
  if (r == 10) {
    mAcc <- c(
      # 分子
      "　銷貨收入淨額"
      # 分母  
      , "　　　不動產、廠房及設備")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #總資產週轉率
  if (r == 11) {
    mAcc <- c(
      # 分子
      "　銷貨收入淨額"
      # 分母  
      , "　　流動資產合計", "　　非流動資產合計", "　資產總額")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #資產報酬率
  if (r == 12) {
    mAcc <- c(
      # 分子
      "營業收入合計", "營業成本合計", "　營業費用合計", "營業利益（損失）"
      , "　營業外收入及支出合計", "稅前淨利（淨損）", "所得稅費用（利益）合計"
      , "繼續營業單位本期淨利（淨損）", "　停業單位損益合計", "　　　利息費用"
      # 分母        
      , "　　流動資產合計", "　　非流動資產合計", "　資產總額")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #股東權益報酬率
  if (r == 13) {
    mAcc <- c(
      # 分子
      "營業收入合計", "營業成本合計", "　營業費用合計", "營業利益（損失）"
      , "　營業外收入及支出合計", "稅前淨利（淨損）", "所得稅費用（利益）合計"
      , "繼續營業單位本期淨利（淨損）", "　停業單位損益合計"
      # 分母
      , "　　歸屬於母公司業主之權益合計", "　　非控制權益", "　權益總額")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #純益率
  if (r == 14) {
    mAcc <- c(
      # 分子
      "營業收入合計", "營業成本合計", "　營業費用合計", "營業利益（損失）"
      , "　營業外收入及支出合計", "稅前淨利（淨損）", "所得稅費用（利益）合計"
      , "繼續營業單位本期淨利（淨損）", "　停業單位損益合計"
      # 分母
      , "　銷貨收入淨額")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #每股盈餘
  if (r == 15) {
    mAcc <- c(
      # 分子
      "營業收入合計", "營業成本合計", "　營業費用合計", "營業利益（損失）"
      , "　營業外收入及支出合計", "稅前淨利（淨損）", "所得稅費用（利益）合計"
      , "繼續營業單位本期淨利（淨損）", "　停業單位損益合計"
      # 分母
      , "　　　　普通股股本")
    
    t2 <- magic_black_box(mAcc)
  }
  
  #現金流量比率
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  #現金再投資比率
  if (r == 5) {
    mAcc <- c(
      # 分子
      ""
      # 分母  
      , "")
    
    t2 <- magic_black_box(mAcc)
  }  
  
  
  ############################################################################

  # show result <- stock + year + t1 + t2
  cat(paste(stock[s], "在", year[y], "年的", t1 , "是因為", t2))
}
