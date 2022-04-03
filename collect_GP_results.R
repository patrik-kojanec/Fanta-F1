# script to scrape the results from the internet
library(rvest)
library(lubridate)
library(tidyverse)

GPs <- read.csv("db/GPs.csv",  row.names = 1)

prev_gps_id <-
  c(0, which(as.Date(GPs$Date) - as.Date(today()) < 7)) + 1

fix_scraped_df <- function(df, ss) {
  if(nrow(df) < 20){
    df[(nrow(df)+1):20, ] <- NA
  }
  df <- df[, c(-1, -ncol(df))] %>%
    mutate(
      Driver = str_squish(str_split(
        Driver, "\n", n = 3, simplify = T
      )[, 3]),
      Session = ss,
      Pos = 1:nrow(df)
    ) %>%
    select(Session, Pos, Driver) %>%
    pivot_wider(
      values_from = Driver,
      names_from = Pos,
      names_prefix = "P",
      names_sep = "",
    )
  return(df)
}

sessions <- c("FP1", "FP2", "FP3", "Sprint", "Quali", "Race")
links <-
  c(
    "link_fp1",
    "link_fp2",
    "link_fp3",
    "link_result_sprint",
    "link_result_qualifying",
    "link_result_race"
  )

for (gp in prev_gps_id) {
  gp_file <- paste0("db/GP_results/", GPs$Event[gp], ".csv")
  for (i in 1:6) {
    if (!file.exists(gp_file)) {
      link <- paste0("https://www.formula1.com/en/results.html/2022/",
                     GPs$link_fp1[gp])
      df <- html_table(read_html(link))
      if (length(df) != 0) {
        df <- fix_scraped_df(df[[1]], ss = "FP1")
        write_csv(df, gp_file)
      }
    } else{
      df_o <- read.csv(gp_file)
      if(sessions[i] %in% df_o$Session){
        next
      }
      if (i == 4 & !GPs$is_sprint[gp]){
        next
      }
      if (i == 6) {
        link <- paste0("https://www.formula1.com/en/results.html/2022/",
                       GPs$link_result_race[gp])
        link2 <-
          paste0(
            "https://www.formula1.com/en/results.html/2022/",
            str_replace(
              string = GPs$link_result_race[gp],
              pattern = "race-result",
              replacement = "fastest-laps"
            )
          )
        
        df <- html_table(read_html(link))[[1]]
        df2 <- html_table(read_html(link2))[[1]]
        df <- fix_scraped_df(df, ss = "Race")
        df2 <- fix_scraped_df(df2, ss = "GV")
        write_csv(rbind(df_o, df, df2), gp_file)
      } else {
        link <- paste0("https://www.formula1.com/en/results.html/2022/",
                       GPs[gp, links[i]])
        df <- html_table(read_html(link))
        if (length(df) == 0) {
          break
        }
        df <- fix_scraped_df(df[[1]], ss = sessions[i])
        write_csv(rbind(df_o, df), gp_file)
      }
    }
  }
}
