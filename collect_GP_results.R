# script to scrape the results from the internet
library(rvest)
library(lubridate)
library(tidyverse)

GPs <- read.csv("db/GPs.csv",  row.names = 1)

prev_gps_id <- 1:which(as.Date(GPs$Date) - as.Date(today()) < 7 & 
                    as.Date(GPs$Date) - as.Date(today()) > 0)

fix_scraped_df <- function(df, ss){
  df <- df[, c(-1, -ncol(df))] %>%
    mutate(Driver = str_squish(str_split(Driver, "\n", n = 3, simplify = T)[, 3]),
           Session = ss,
           Pos = 1:20) %>%
    select(Session, Pos, Driver) %>%
    pivot_wider(values_from = Driver, names_from = Pos, names_prefix = "P", names_sep = "", )
  return(df)  
}


for (gp in prev_gps_id){
  gp_file <- paste0("db/GP_results/", GPs$Event[gp], ".csv")
  if (!file.exists(gp_file)){
    if(GPs$is_sprint[gp]){
      link <- paste0("https://www.formula1.com/en/results.html/2022/", 
                    GPs$link_result_sprint)
      df <- html_table(read_html(link))[[1]]
      df <- fix_scraped_df(df, ss = "Sprint")
      write_csv(df, gp_file)
    }else{
      if(now() > as.POSIXct(GPs$Time_Quali[gp], tz = "GMT") + hours(2)){
        link <- paste0("https://www.formula1.com/en/results.html/2022/", 
                      GPs$link_result_qualifying[gp])
        df <- html_table(read_html(link))[[1]]
        df <- fix_scraped_df(df, ss = "Quali")
        write_csv(df, gp_file)
      }
    }
  }else{
    df_o <- read.csv(gp_file)
    if(GPs$is_sprint[gp]){
      if(now() > as.POSIXct(GPs$Time_Quali[gp], tz = "GMT") + hours(2)){
        link <- paste0("https://www.formula1.com/en/results.html/2022/", 
                       GPs$link_result_qualifying)
        df <- html_table(read_html(link))[[1]]
        df <- fix_scraped_df(df, ss = "Quali")
        write_csv(rbind(df_o, df), gp_file)
      }
    }else{
      if(now() > as.POSIXct(GPs$Time_Race[gp], tz = "GMT") + hours(3)){
        link <- paste0("https://www.formula1.com/en/results.html/2022/", 
                       GPs$link_result_race[gp])
        link2 <- paste0("https://www.formula1.com/en/results.html/2022/", 
                        str_replace(string = GPs$link_result_race[gp], 
                                    pattern = "race-result",
                                    replacement = "fastest-laps"))
        
        df <- html_table(read_html(link))[[1]]
        df2 <- html_table(read_html(link2))[[1]]
        df <- fix_scraped_df(df, ss = "Race")
        df2 <- fix_scraped_df(df2, ss = "GV")
        write_csv(rbind(df_o, df, df2), gp_file)
      }
    }
  }
}

