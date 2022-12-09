analyze <- function() {
  #######################
  ###### TEST DATA ######
  # s <- 1  #1304 台聚
  # y <- 2  #2018
  # r <- 1  #純益率
  #######################
  #
  # ratio list
  ratio <- c("純益率")
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
      li <- paste0(1:(length(year) - 1), sep = ".", year[2:length(year)])
      y <- readline(cat("請輸入要分析的年度：", li, sep = "\n"))  # input
      
      if (y == "exit")
        break
      else
        y <- as.numeric(y) + 1
      
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
    m1 <- m2 <- c("", 0)# max change (account title, value)
    t2 <- ""  # result
    
    for (m in mAcc) {
      # calculate
      n <-
        income_dta[which(mAcc_income == m), y, s] - income_dta[which(mAcc_income == m), (y - 1), s]
      
      # add to t2
      if (n > 0)
        t <- paste(m, "與前年相比增加了", n, "元")
      else if (n < 0)
        t <- paste(m, "與前年相比減少了", abs(n), "元")
      else
        t <- paste(m, "與前年相同")
      
      t2 <- paste(t2, t, sep = "\n")
      
      # find 2 max change account title & value
      if (abs(n) > as.numeric(m1[2])) {
        temp <- m1
        m1 <- c(m , abs(n))
        m2 <- temp
      } else if (abs(n) > as.numeric(m2[2])) {
        m2 <- c(m , abs(n))
      }
      
    }
    # add max change to t2
    t2 <- paste(t2, "\n 其中變動最大的為", m1[1], "變動第二大的為", m2[1])
    return(t2)
  }
  ##############################################################################
  #
  
  ratio_cr <-
    ratio_dta1[r, y, s] - ratio_dta1[r, (y - 1), s]  # ratio change rate
  
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
  # 純益率=本期淨利（淨損）/　銷貨收入淨額
  if (r == 1) {
    mAcc <- c("所得稅費用（利益）合計", "　營業外收入及支出合計", "　營業費用合計"
              , "　銷貨成本", "　銷貨收入淨額")
    t2 <- magic_black_box(mAcc)
  }
  
  ############################################################################

  # show result <- stock + year + t1 + t2
  cat(paste(stock[s], "在", year[y], "年的", t1 , "是因為", t2))
}
