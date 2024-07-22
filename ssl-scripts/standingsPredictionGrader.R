
##################################################################
##              Season Standings Prediction Grader              ##
##################################################################

require(googlesheets4)
require(dplyr)
require(stringr)
require(rvest)
require(stringi)

sheet <- "https://docs.google.com/spreadsheets/d/10F9O7mX1c52Du_avjykHt1sO8CI_7CTdeU_aiI_5WSA/edit#gid=741638837"

predictions <- read_sheet(sheet)

correct <- 
  c(
    "Asiania",
    "Baltic",
    "Americas",
    "Africa",
    "Asiania|Americas",
    "Americas|Asiania",
    "Americas",
    "Liang Kuai (Asiania)",
    "Scott Sterling (Africa)"
  )

value <- 
  c(
    1, 1, 1, 1, 0.5, 0.5, 1, 1, 1
  )

graded <- 
  ((predictions[,3:(ncol(predictions)-1)] %>% t() %>% str_detect(pattern = correct)) * value) %>% 
  matrix(ncol = 9, byrow = TRUE) %>% 
  rowSums(na.rm = TRUE)

predictions <- 
  predictions %>% 
  mutate(
    points = 2 + graded
  ) %>% 
  arrange(`Write your user name:`)

paste(
  "The following users may claim the specified TPE for WSFC Group Stage:",
  " ",
  "Correct predictions:",
  paste0(correct, collapse = "\n"),
  " ",
  paste("BoD, Grader and Sim Team may claim the AVG:", mean(predictions$points %>% ceiling()) %>% round(0)),
  "[code]",
  paste0(
    paste(predictions$`Write your user name:`,"\t", predictions$points %>% ceiling(), sep = ""),
    collapse = "\n"
  ),
  "[/code]",
  sep = "\n"
) %>% 
  cat()

