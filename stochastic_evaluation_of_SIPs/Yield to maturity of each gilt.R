YTM <- function(cp,fv,ac,stl, md, lc,nc){
  
  # Define the basic parameters 
  clean_price    <- cp          # quoted market price per 100 face value
  face_value     <- fv             # par value repaid at maturity
  annual_coupon  <- ac          #  annual coupon rate
  semi_coupon    <- face_value * annual_coupon / 2  # amount per semi-annual payment
  
  settlement     <- as.Date(stl)   # date investor buys the gilt
  maturity_date  <- as.Date(md)   # date gilt is repaid at par
  
  # Identify coupon dates
  # We generate all coupon dates from issue up to maturity,
  # then keep only those after the settlement date (these are
  # the payments the investor will actually receive).
  
  last_coupon  <- as.Date(lc)   # most recent coupon BEFORE settlement
  next_coupon  <- as.Date(nc)   # first coupon AFTER settlement
  
  # Build the full sequence of coupon dates after settlement
  coupon_dates <- seq(next_coupon, maturity_date, by = "6 months")
  cat("Coupon dates the investor receives:\n")
  print(coupon_dates)
  cat("Number of coupons remaining:", length(coupon_dates), "\n\n")
  
  # Compute accrued interest and dirty price 
  # Accrued interest = coupon * (days since last coupon / days in coupon period)
  days_accrued      <- as.numeric(settlement - last_coupon)  
  days_in_period    <- as.numeric(next_coupon - last_coupon) 
  accrued_interest  <- semi_coupon * (days_accrued / days_in_period)
  
  # Dirty price = what the investor actually pays (clean + accrued)
  dirty_price <- clean_price + accrued_interest
  
  cat("Days accrued since last coupon:", days_accrued, "\n")
  cat("Days in full coupon period:    ", days_in_period, "\n")
  cat("Accrued interest:               ", round(accrued_interest, 4), "\n")
  cat("Dirty price:                    ", round(dirty_price, 4), "\n\n")
  
  # Build cash flow schedule
  # For each coupon date, the cash flow is 0.625.
  # On the final maturity date, the investor also receives 100 (face value).
  cash_flows <- rep(semi_coupon, length(coupon_dates))
  cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + face_value
  
  # Time to each cash flow, measured in semi-annual periods.
  # The first coupon is NOT exactly 1 full period away - it arrives after
  # only the REMAINING fraction of the current period.
  first_fraction <- as.numeric(next_coupon - settlement) / days_in_period
  time_periods   <- first_fraction + (0:(length(coupon_dates) - 1))
  
  cat("Time to each cash flow (in semi-annual periods):\n")
  print(round(time_periods, 4))
  cat("\n")
  
  #Define the bond pricing function 
  # The fair price of the gilt equals the present value of all future cash flows,
  # discounted at the semi-annual rate r = YTM/2.
  # dirty_price = sum[ cash_flow_i / (1 + r)^t_i ]
  # We need to solve for r (and hence YTM) that makes this equation hold.
  
  bond_price <- function(ytm) {
    r  <- ytm / 2                          # convert annual YTM to semi-annual rate
    pv <- sum(cash_flows / (1 + r)^time_periods)  # present value of all cash flows
    return(pv)
  }
  
  # Solve for YTM using uniroot 
  # uniroot finds the YTM where bond_price(ytm) - dirty_price = 0
  # We search between 0.01% and 20% (a wide enough range for any realistic gilt)
  
  objective <- function(ytm) bond_price(ytm) - dirty_price
  
  solution  <- uniroot(objective, interval = c(0.0001, 0.20), tol = 1e-10)
  ytm       <- solution$root
  
  # Report results
  holding_years <- as.numeric(maturity_date - settlement) / 365.25
  total_return  <- ((1 + ytm/2)^(2 * holding_years) - 1) * 100
  
  cat("Summary of the results \n")
  cat("Clean Price :      ", clean_price, "\n")
  cat("Accrued Interest:               ", round(accrued_interest, 4), "\n")
  cat("Dirty Price:                    ", round(dirty_price, 4), "\n")
  cat("Annual YTM:                      ", round(ytm * 100, 4), "%\n")
  cat("Holding period:                  ", round(holding_years, 2), "years\n")
  cat("Total coupons received:         ", length(coupon_dates) * semi_coupon, "\n")
  cat("Total return over holding period:", round(total_return, 4), "%\n")
}

#YTM(104.35,100,0.0125 ,"2021-04-30", "2027-07-22", "2021-01-22","2021-07-22") #for Gilt 1
YTM(96.93, 100, 0.041250, "2026-04-24", "2033-03-7", "2026-03-07", "2026-09-07") #for Gilt 2
#YTM(98.56, 100, 0.041250,"2026-05-08", "2031-03-7", "2026-03-07", "2026-09-07") #for Gilt 3
#YTM(99.90, 100, 0.046250,"2026-04-10", "2034-01-31", "2026-01-31", "2026-07-31") #for Gilt 4