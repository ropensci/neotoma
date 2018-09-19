#' @export
print.chroncontrol <- function(x, ...) {

  if (!is.na(x$parent$dataset.name)) {
    cat(paste0("Chronology for ", x$parent$dataset.name, 
               ", dataset ID: ", x$parent$dataset.id, "\n"))
  }
  
  if (x$meta$default == TRUE) {
    cat(paste0(x$meta$name, ": Default model, constructed using ", 
               x$meta$age.model, ".\n",
               "Model age span: ", x$meta$age.younger, " to ", x$meta$age.older, "\n",
               "Model age units: ", x$meta$age.type, "\n\n"))
    print(x$chron.control)
  } else {
    cat(paste0(x$meta$name, ": Default model, constructed using ", 
               x$meta$age.model, ".\n",
               "Model age span: ", x$meta$age.younger, " to ", x$meta$age.older, "\n",
               "Model age units: ", x$meta$age.type, "\n\n"))
    
    print(format(x$chron.control, justify = "left"))
  }
  
  if (!is.na(x$access.date)) {
    cat(paste0('\nAccessed ', format(x$access.date, "%Y-%m-%d %H:%M"), 'h. \n'))
  }
  
  NULL
}
