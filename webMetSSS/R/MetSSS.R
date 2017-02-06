#' Score the MetSSS
#'
#' Function requires systolic and diastolic blood pressure,
#'   triglycerides, waist circumference, HDL cholesterol,
#'   blood glucose, and sex.
#'
#' @export
#' @importFrom pscore predictCS
#' @param input data passed on as \code{newdata} to \code{\link{predictCS}}
#' @examples
#' mydata <- data.frame(
#'   sbp = c(122, 102.5),
#'   dbp = c(76.5, 64),
#'   trigs = c(1.47, 1.27),
#'   hdl = c(2.22, 1.59),
#'   waist = c(71, 91),
#'   glucose = c(5.16, 5.82),
#'   sex = c("Female", "Male"))
#'
#' MetSSS(mydata)
MetSSS <- function(input) {
  #input can either be csv file or data
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }

  v <- c("sbp", "dbp", "trigs", "hdl", "waist", "glucose")
  if (!all(v %in% names(newdata))) {
    stop(sprintf("%s must be column names in the data",
                 paste(v[!v %in% names(newdata)], collapse = ", ")))
  }

  for (i in v) {
    newdata[, i] <- as.numeric(newdata[, i])
  }

  ##MetSSS_model is included with the package
  metsss <- predictCS(MetSSS_model,
              newdata = newdata[, v],
                      groups = as.character(newdata$sex))
  if (!identical(length(metsss), nrow(newdata))) {
    metsss <- metsss[1:nrow(newdata)]
  }

  newdata$metsss <- metsss
  return(newdata)
}
