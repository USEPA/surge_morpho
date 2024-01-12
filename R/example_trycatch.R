xf <- function(df){
  x <- tryCatch(
    {
      samp <- sample(0:1, 1)
      if(samp){
        df
      } else {
        stop("Sad Trombone")
      }
    },
    error = function(e){
      df$Petal.Width
    }
  )
  x
}

lapply(split(iris, 1:150), xf)
