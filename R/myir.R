#' @export myir

myir <- function(my.ir, my.naics.code) {



  naics$NAICS <- suppressWarnings(as.numeric(naics$NAICS))
  naics <- tidyr::drop_na(naics)

  # take in my.ir
  # find my.naics.code
  my.industry <- which(naics$NAICS == my.naics.code)

  # find the row in df
  industry.ir <- naics[my.industry,3]
  print(naics[my.industry,])
  print(my.ir < industry.ir)

  # barplot
  i <- my.ir
  j <- industry.ir
  k <- c(i, j)
  barplot(k, col = c("dodgerblue", "gray"),
          main = "Our IR vs Industry IR",
          sub = naics[my.industry,1],
          ylab = "Incident Rate")
}
