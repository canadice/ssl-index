box::use(
  httr,
  jsonlite,
)


#' @export
readAPI <- function(url, ...) {
  temp <- url |> httr$GET(...)
  temp$content |>
    rawToChar() |>
    jsonlite$fromJSON()
}
