balance_draw <- function() {
    ################################################################################
    # plot title, color
    ratio <- c("負債佔資產比率 (%)")
    color <- c("orange", "blue")
    #
    ################################################################################
    #
    # define array
    setwd(root)
    setwd(file.path(stock[1], "csv/balances", fsep = ""))
    rnum <- length(ratio) # Account title - row
    cnum <- length(dir()) # Year - column
    dnum <- length(stock) # Stock - dimension
    dtax <- array(numeric(), c(rnum, cnum, dnum)) # 3D array
    # named array
    dimnames(dtax) <- list(ratio, substr(dir(), 6, 9), stock)
    #
    ################################################################################
    #
    # ratio calculate
    # 負債佔資產比率=負債總額 / 資產總額
    debt_ratio <- balance_dta[9, , ] / balance_dta[3, , ]
    debt_ratio <- as.data.frame(t(debt_ratio))
    dtax[, , 1] <- unlist(debt_ratio[1, ])
    dtax[, , 2] <- unlist(debt_ratio[2, ])
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
            1, 1:length(colnames(balance_dta)),
            format(as.Date(colnames(balance_dta), format = "%Y"), "%Y")
        ) # X
    }
}
