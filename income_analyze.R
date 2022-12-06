#######################
###### TEST DATA ######
s <- 1  #1304
y <- 2  #2018
r <- 1
ratio <- c("純益率")
#######################

# for (d in dim(ratio_dta1)[3]) {
#   for (r in nrow(ratio_dta1)) {
#     for (c in ncol(ratio_dta1)) {
#
#     }
#   }
#
# }

magic_black_box <- function(mAcc) {
  mn <- 0
  ma <- ""
  t2 <- ""
  for (m in mAcc) {
    n <-
      income_dta[which(mAcc_income == m), y, s] - income_dta[which(mAcc_income == m), (y - 1), s]
    
    if (n > 0)
      t <- paste(m, "與前年相比增加了", n, "元")
    else if (n < 0)
      t <- paste(m, "與前年相比減少了", abs(n), "元")
    else
      t <- paste(m, "與前年相同")
    
    if (abs(n) > mn) {
      # find max change mAcc
      mn <- abs(n)
      ma <- m
    }
    
    t2 <- paste(t2, t, collapse = "\n")
  }
  t2 <- paste(t2, "其中變動最大的為", ma)
  return(t2)
}

# 金額、佔銷貨收入淨額比例
if (y != 2017) {
  ratio_cr <-
    ratio_dta1[r, y, s] - ratio_dta1[r, (y - 1), s]  # ratio change rate
  
  # text1
  if (ratio_cr > 0)
    t1 <- paste(ratio[r], "增加了", percent(ratio_cr, accuracy = 0.01))
  else if (ratio_cr < 0)
    t1 <-
      paste(ratio[r], "減少了", percent(abs(ratio_cr), accuracy = 0.01))
  else
    t1 <- paste(ratio[r], "不變")
  
  
  # 純益率=本期淨利（淨損）/　銷貨收入淨額
  # 分子->所得稅費用（利益）合計、　營業外收入及支出合計、　營業費用合計、　銷貨成本、　銷貨收入淨額
  # 分母->　銷貨收入淨額
  if (r == 1) {
    # 純利率 分析
    mAcc <-
      c("所得稅費用（利益）合計", "　營業外收入及支出合計", "　營業費用合計", "　銷貨成本", "　銷貨收入淨額")
    t2 <- magic_black_box(mAcc)
    # magic_black_box("所得稅費用（利益）合計")
    # magic_black_box("　營業外收入及支出合計")
    # magic_black_box("　營業費用合計")
    # magic_black_box("　銷貨成本")
    # magic_black_box("　銷貨收入淨額")
    
    # 分子
    # max(
    #   magic_black_box("所得稅費用（利益）合計")[2],
    #   magic_black_box("　營業外收入及支出合計")[2],
    #   magic_black_box("　營業費用合計")[2],
    #   magic_black_box("　銷貨成本")[2],
    #   magic_black_box("　銷貨收入淨額")[2]
    # )
    # 分母
  }
  
} else{
  cat("請選擇"+y1 + "到"+y2 + "年")
}

# show text <- stock + year + t1 + t2
paste(stock[1], "在2018年的", t1 , "是因為", t2)
