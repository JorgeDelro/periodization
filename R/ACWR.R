
#' Title
#'
#' @param df
#' @param ID
#' @param TL
#' @param weeks
#' @param training_dates
#' @param ACWR_method
#'
#' @return
#' @export
#'
#' @examples
ACWR <- function(df,
                 ID,
                 TL,
                 weeks,
                 training_dates,
                 ACWR_method = c("EWMA", "RAC", "RAU")) {

  # Checks
  #


  # NULL checks
  if(is.null(df)){
    stop("you must provide a dataframe")
  }
  if(is.null(ID)){
    stop("you must provide the column name of ID variable")
  }
  if(is.null(TL)){
    stop("you must provide the column name of training load variable")
  }
  if(is.null(weeks)){
    stop("you must provide the column name of week variable")
  }
  if(is.null(training_dates)){
    stop("you must provide the column name of training dates variable")
  }

  # Checks methods
  #if(ACWR_method != "EWMA" || ACWR_method != "RAC" || ACWR_method != "RAU"){
  #  stop(cat(paste("Mispelling error in ACWR_method. Currently you can choose among the following methods to calculate the ACWR:",
  #                 "EWMA, RAC and RAU" ,sep="\n")))
  #}

  # Loop over the individuals
  for (i in unique(df$ID)) {

    # Create individual dfs
    df_ind <- df[ df[[ID]] == i,  c(ID, TL, weeks, training_dates) ]

    # EWMA method
    if(ACWR_method == "EWMA") {

      res_EWMA <- EWMA(TL = df_ind$TL)

      df_ind$EWMA_chronic <- res_EWMA$EWMA_chronic
      df_ind$EWMA_acute <- res_EWMA$EWMA_acute
      df_ind$EWMA_ACWR <- res_EWMA$EWMA_ACWR

    }

    # Rolling Average Coupled
    if(ACWR_method == "RAC") {

      res_RAC <- RAC(TL = df_ind$TL,
                     weeks = df_ind$weeks,
                     training_dates = df_ind$training_dates)

      df_ind$RAC_acute = res_RAC$RAC_acute
      df_ind$RAC_chronic = res_RAC$RAC_chronic
      df_ind$RAC_ACWR = res_RAC$RAC_ACWR

    }

    # Rolling Average Uncoupled
    if(ACWR_method == "RAU") {

      res_RAU <- RAU(TL = df_ind$TL,
                     weeks = df_ind$weeks,
                     training_dates = df_ind$training_dates)

      df_ind$RAU_acute = res_RAU$RAU_acute
      df_ind$RAU_chronic = res_RAU$RAU_chronic
      df_ind$RAU_ACWR = res_RAU$RAU_ACWR

    }

    if(i == 1){
      df_final <- df_ind
    } else {
      df_final <- rbind(df_final, df_ind)
    }

  } # end loop over individuals

  return(df_final)

} # end ACWR function
