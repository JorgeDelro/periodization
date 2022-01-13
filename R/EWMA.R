
#' Title
#'
#' @param TL
#'
#' @return
#' @export
#'
#' @examples
EWMA <- function(TL) {


  # lambda <- 2/(N + 1)

  # Initialize variables
  EWMA_chronic <- c()
  EWMA_acute <- c()
  EWMA_ACWR <- c()
  lambda_acute <- 2/(7+1)
  lambda_chronic <- 2/(28+1)

  # Loop over the TL
  for (i in seq_along(TL)) {

    # First training day: EWMA_chronic = TL / EWMA_acute = TL
    if(i == 1){
      EWMA_chronic[i] = TL[i]
      EWMA_acute[i] = TL[i]
    }

    if(i > 1){
      EWMA_chronic[i] = TL[i] * lambda_chronic + ((1- lambda_chronic)* EWMA_chronic[i-1])
      EWMA_acute[i] = TL[i] * lambda_acute + ((1- lambda_acute)* EWMA_acute[i-1])
    }

    EWMA_ACWR <- EWMA_acute / EWMA_chronic

  }

  return(list(EWMA_chronic = EWMA_chronic,
              EWMA_acute = EWMA_acute,
              EWMA_ACWR = EWMA_ACWR))

} # end EWMA
