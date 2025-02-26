

### MALIA'S CODE STARTS HERE ###

## pairs to compare ints
panel.hist <- function(x){
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "darkgreen",)
}

pairs(x = wa_combined[, c("TotalGDP_PercentChange",
                          "DivorceRate",
                          "PersonalIncome",
                          "RealGDP",
                          "PersonalExpenditures",
                          "MarriageRate")],
      panel = panel.smooth,
      diag.panel = panel.hist)

