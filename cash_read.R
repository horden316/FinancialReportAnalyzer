cash_read <- function() {
  ################################################################################
  #
  # define array
  setwd(root)
  setwd(file.path(stock[1], "csv/cashflow", fsep = ""))
  rnum <- length(mAcc_cash) # Account title - row
  cnum <- length(dir()) # Year - column
  dnum <- length(stock) # Stock - dimension
  dtax <- array(numeric(), c(rnum, cnum, dnum)) # 3D array
  # named array
  dimnames(dtax) <- list(mAcc_cash, substr(dir(), 6, 9), stock)
  #
  ################################################################################
  #
  # read file
  for (d in 1:dnum) { # set path by stock
    setwd(root)
    setwd(file.path(stock[d], "csv/cashflow", fsep = ""))
    
    for (c in 1:cnum) { # read csv by year
      dta <- read_csv(dir()[c])
      colnames(dta) <- c("Account", substr(dir()[c], 6, 9))
      
      for (r in 1:rnum) { # get field by account
        
        # get by main account title
        Row <- which(dta$Account == mAcc_cash[r]) # find row where account title in mAcc_cash
        dtax[r, c, d] <- as.numeric(dta[Row, 2])
      }
    }
  }
  dtax[is.na(dtax)] <- 0
  return(dtax)
}
