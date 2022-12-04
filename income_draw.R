income_draw <- function() {

  ################################################################################
  # plot title, color
  ratio <- c("純益率 (%)")
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
  # 純益率=本期淨利（淨損）/　銷貨收入淨額
  Net_profit_margin <- income_dta[8, , ] / income_dta[1, , ]
  Net_profit_margin <- as.data.frame(t(Net_profit_margin))
  dtax[, , 1] <- unlist(Net_profit_margin[1, ])
  dtax[, , 2] <- unlist(Net_profit_margin[2, ])
  #
  ################################################################################
  #
  # draw
  for (r in 1:rnum) {
    Yrange <- c(min(dtax[r, , ]) / 1.1, max(dtax[r, , ]) * 1.1) # Y
    for (d in 1:dnum) {
      plot(unlist(dtax[, , d]),
        type = "l", col = paste(color[d]), lwd = 2, main = paste(ratio[r]),
        ylim = Yrange, ylab = "", xlab = "年", xaxt = "n"
      )
      points(unlist(dtax[, , d]), pch = 16, cex = 1.25, col = paste(color[d]))
      if (d != dnum) {
        par(new = T)
      }
    }
    axis(
      1, 1:length(colnames(income_dta)),
      format(as.Date(colnames(income_dta), format = "%Y"), "%Y")
    ) # X
  }
}
