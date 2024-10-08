library(pander)
library(stringr)
library(dplyr)
library(lubridate)
library(kableExtra)
library(readxl)  

get_cv_sheet <- function(sheet) {
  return(read_excel(
    path = 'data/cv.xlsx',
    sheet = sheet
  ))
}

make_ordered_list <- function(x) {
    return(pandoc.list(x, style = 'ordered'))
}

make_bullet_list <- function(x) {
  return(pandoc.list(x, style = 'bullet'))
}

make_ordered_list_filtered <- function(df, cat) {
  return(df %>%
    filter(category == {{cat}}) %>%
        mutate(
            citation = str_replace_all(
                citation,
                "\\\\\\*(\\w+),",
                "\\\\*\\\\underline{\\1},"
            )
        ) %>%
    pull(citation) %>%
    make_ordered_list()
  )
}

na_to_space <- function(x) {
  return(ifelse(is.na(x), '', x))
}

enquote <- function(x) {
  return(paste0('"', x, '"'))
}

markdown_url <- function(url) {
  return(paste0('[', url, '](', url,')'))
}
