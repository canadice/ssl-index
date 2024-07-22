combineTriggers <- function(...){
  qs <- rlang::enquos0(...)
  vals <- lapply(qs, exec_event_safely)
  
  if(all(sapply(vals, event_is_invalid))) {
    # Return NULL if all other events are NULL or 0
    NULL
  } else {
    vals
  }
}

exec_event_safely <- function(x) {
  try(rlang::eval_tidy(x), silent = TRUE)
}

event_is_invalid <- function(x) {
  inherits(x, "try-error") || is.null(x) || x == 0
}