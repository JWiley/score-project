.onLoad <- function(lib, pkg){
  #automatically loads the dataset when package is loaded
  #do not use this in combination with lazydata=true
  utils::data(MetSSS_model, package = pkg, envir = parent.env(environment()))
}
