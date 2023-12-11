#' @describeIn utils_ utils_
report_missing <- function(dat,
                           id_col,
                           report_col
                           ){
  report_missing_i <- function(dat,
                               id_col,
                               report_col){
    dat2 <- dat[,c(id_col,report_col),with=FALSE] |> unique()
    n_missing <- sum(is.na(dat2[[report_col]]))
    n_total <- nrow(dat2)
    messager(formatC(n_missing,big.mark = ","),
             "/",
             formatC(n_total,big.mark=","),
             paste0("(",round(n_missing/n_total*100,2),"%)"),
             report_col,"missing.")
  }
  for(rc in report_col){
    report_missing_i(dat = dat,
                     id_col = id_col,
                     report_col = rc)
  }
}
