income_read <- function() {
  ################################################################################
  #
  # define array
  setwd(root)
  setwd(file.path(stock[1], "csv/comprehensiveincome", fsep = ""))
  rnum <- length(mAcc_income) # Account title - row
  cnum <- length(dir()) # Year - column
  dnum <- length(stock) # Stock - dimension
  dtax <- array(numeric(), c(rnum, cnum, dnum)) # 3D array
  # named array
  dimnames(dtax) <- list(mAcc_income, substr(dir(), 6, 9), stock)
  #
  ################################################################################
  #
  # read file
  for (d in 1:dnum) { # set path by stock
    # if (file.exists("FinancialReportAnalyzer.Rproj")){  # init path
    #   setwd(file.path(stock[d],"csv/comprehensiveincome", fsep = ""))
    # }else{  # set next stock file path
    #   setwd("../../")
    #   setwd(file.path(stock[d],"csv/comprehensiveincome", fsep = ""))
    # }
    setwd(root)
    setwd(file.path(stock[d], "csv/comprehensiveincome", fsep = ""))

    for (c in 1:cnum) { # read csv by year
      dta <- read_csv(dir()[c])
      colnames(dta) <- c("Account", substr(dir()[c], 6, 9))

      for (r in 1:rnum) { # get field by account

        # get by main account title
        Row <- which(dta$Account == mAcc_income[r]) # find row where account title in mAcc_income
        dtax[r, c, d] <- as.numeric(dta[Row, 2])
      }
    }
  }
  return(dtax)
}
