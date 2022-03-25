library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)

source("utils.R")

GPs <- read.csv("db/GPs.csv",  row.names = 1)
teams <- read.csv("db/teams.csv", row.names = 1)
users <- read.csv("db/users.csv",  row.names = 1)

drivers <- c("","VER", "PER", "HAM", "RUS", "LEC", "SAI", "NOR", "RIC", "GAS", "TSU", "BOT", "ZHO", 
             "MSC", "MAG", "ALB", "LAT", "ALO", "OCO", "VET", "STR", "HUL")
# Define UI for application that draws a histogram
ui <- navbarPage(
  "Fantasy F1",
  theme = bs_theme(bootswatch = "slate", ),
  tabPanel("Trenutni GP", 
           h2("Naslednja session: ", textOutput("sess_act_txt")),
           h4(textOutput("time_left")),
           uiOutput("sel_GP_sess")),
  tabPanel("Klassifika - Piloti",
           uiOutput("sel_class_ind_gp"),
           fluidRow(
             column(6, plotOutput("plot_class_ind1")),
             column(6, plotOutput("plot_class_ind2"))
           )
           ),
  ############################## Klass. Costruttori ###############
  tabPanel("Klassifika - Costruttori",
           uiOutput("sel_class_sku_gp"),
           fluidRow(
             column(6, plotOutput("plot_class_sku1")),
             column(6, plotOutput("plot_class_sku2"))
           )),
  ############################## Preged formazioni ###############
  tabPanel("Formazioni",
           sidebarLayout(
             sidebarPanel(
              uiOutput("sel_shw_form_gp"),
              uiOutput("sel_shw_form_sess"),
              selectInput("shw_form_usr", label = "Username:",
                                  choices = c("Vsi", users$user))),
             mainPanel(tableOutput("formazioni_tbl"))
           )),
  #########################################Formazion#################################
  tabPanel("Oddaj Formazion",
           fluidRow(
             column(6,
                    fluidRow(
                 selectInput("usr",
                            "Username",
                            choices = users$user),
                  textInput(inputId = "pwd", label = "Password: "))),
             column(width = 5,
                    fluidRow(
                      column(5, p(""), actionButton("log_in", "Log In")),
                      column(6,
                             p("Login-Status: \n",
                               textOutput(outputId = "log_in_err_msg", container = div, inline = T )))
                      )),
             column(1,
                    downloadButton("dl_backup", "Download Backup"))
             ),
           sidebarLayout(
             
             sidebarPanel(width = 2,
                          h4("Active GP: "),
                          h4(textOutput("gp_select",inline = F)),
                          uiOutput("sess_select")),
             mainPanel(width = 10,
               titlePanel("", 
                          textOutput("gp_session_text", inline = T)),#Moja Formazion za 
               fluidRow(
               column(2, uiOutput("form_1")),
               column(2,uiOutput("form_2")),
               column(2,uiOutput("form_3")),
               column(2,uiOutput("form_4")),
               column(2,uiOutput("form_5"))),
               fluidRow(
              column(2,uiOutput("form_6")),
              column(2,uiOutput("form_7")),
              column(2,uiOutput("form_8")),
              column(2,uiOutput("form_9")),
              column(2,uiOutput("form_10")),
              column(2,uiOutput("form_GV")) ), 
              fluidRow(
                column(2,offset =9 , uiOutput("save_form_b")),
                column(1,uiOutput("save_form_m"))),)
           )),
  #########################################Regole#################################
  tabPanel("Regolamento",
    titlePanel("REGOLE FANTA F1"),
    p( "In caso d je Sprint Race u cetrtkh se posla: ", " Prvh 5 ud Sprint Race"),
              p("U petkh se posla: ", " Prvh 5 ud qualifik"),
              p("U soboth se posla: "," Prvh 10 gara +Giro veloce"),
              p(""),
              p("Punti:"),
              p("5p ce ugans prvega"),
              p("3p z druzga"),
              p("2p z tretjega"),
              p("1p z usakega posamezno"),
             p( "3p giro veloce"),
              p(""),
             p( "PRECEDENZE CLASSIFICA IN CASO DI PARITÀ :"),
              p("Classifica Qualifiche; jma precedenzo prve ki je npiso griljo"),
             p( "Classifica Gara; jma precedenzo uni ki je nredo bulse n qualifikh"),
              p("Classifica weekend (GP) ; jma precedenzo uni ki je nredo bulso garo"),
              p("Classifica complessiva ; jma precedenzo uni ki je nredo bulse zadnji GP"))
)
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  avail_drivers <- reactiveVal(NULL)
  
  V <- reactiveValues(ACCESS_ACTIVE = F,
                      active_GP_id = which(as.POSIXct(GPs$Time_Race, tz = "GMT") > now(tz = "GMT") - hours(3))[1],
                      concluded_GP_id = which(as.POSIXct(GPs$Time_Race, tz = "GMT") > now(tz = "GMT") - hours(3))[1] -1,
                      active_sprint = F,
                      active_quali = F, 
                      active_race = F, 
                      saved_formation = F,
                      active_usr = "")
  
  
  observeEvent(input$usr,{
    if (input$usr != V$active_usr){
      V$ACCESS_ACTIVE = F
    }
  })
  
  observeEvent(input$log_in,{
    a <- input$pwd == users$pwd[grep(input$usr, users$user)]
    if (a){
      V$ACCESS_ACTIVE = T
      V$active_usr = input$usr
    }else{
      V$ACCESS_ACTIVE = F
    }
    
    if(length(GPs$Time_Sprint[V$active_GP_id]) > 1 & GPs$is_sprint){
      V$active_sprint <- difftime(as.POSIXct(GPs$Time_Sprint[V$active_GP_id], tz = "GMT"), now(tz="GMT"), units = "hours") > 2
    }
    V$active_quali <- difftime(as.POSIXct(GPs$Time_Quali[V$active_GP_id], tz = "GMT"), now(tz="GMT"), units = "hours") > 2
    V$active_race <- difftime(as.POSIXct(GPs$Time_Race[V$active_GP_id],  tz = "GMT"), now(tz="GMT"), units = "hours") > 2
    
    })
  
  output$gp_select <- renderText({
    if (length(V$active_GP_id) > 0){
      GPs$Event[V$active_GP_id]
    }else{
        "No Active GP..."
      }
      })
  
  output$log_in_err_msg <- renderText({
     if (V$ACCESS_ACTIVE){
       "Access Granted!"
     }else{
       "Access Denied!"
     }
    
  })
  
  output$sess_select <- renderUI({
    #if(V$ACCESS_ACTIVE){
      active_sess <- c("None")
      if (V$active_sprint){
        active_sess <- c(active_sess, "Sprint")
      }
      if (V$active_quali){
        active_sess <- c(active_sess, "Quali")
      }
      if (V$active_race){
        active_sess <- c(active_sess, "Race")
      }
      active_sess <- active_sess[-1]
      selectInput("session", "Select the session:", active_sess)
    #}
    })
  
  output$form_1 <- renderUI({
    if(V$ACCESS_ACTIVE){
    selectInput("sel_P1", "P1", drivers)}})
  
  output$form_2 <- renderUI({
    if(V$ACCESS_ACTIVE){
    selectInput("sel_P2", "P2", drivers )}})
  
  output$form_3 <- renderUI({
    if(V$ACCESS_ACTIVE){
    selectInput("sel_P3", "P3", drivers )}})
  
  output$form_4 <- renderUI({
    if(V$ACCESS_ACTIVE){
    selectInput("sel_P4", "P4", drivers )}})
  
  output$form_5 <- renderUI({
    if(V$ACCESS_ACTIVE){
    selectInput("sel_P5", "P5", drivers )}})
  
  output$form_6 <- renderUI({
    a <- ifelse(length(input$session)== 0, "", input$session)
    if(a == "Race" & V$ACCESS_ACTIVE){
    selectInput("sel_P6", "P6", drivers)} })
  
  output$form_7 <- renderUI({
    a <- ifelse(length(input$session)== 0, "", input$session)
    if(a == "Race"){
    selectInput("sel_P7", "P7", drivers )}})
  
  output$form_8 <- renderUI({
    a <- ifelse(length(input$session)== 0, "", input$session)
    if(a == "Race" & V$ACCESS_ACTIVE){
    selectInput("sel_P8", "P8", drivers )}})
  
  output$form_9 <- renderUI({
    a <- ifelse(length(input$session)== 0, "", input$session)
    if(a == "Race" & V$ACCESS_ACTIVE){
    selectInput("sel_P9", "P9", drivers )}})
  
  output$form_10 <- renderUI({
    a <- ifelse(length(input$session)== 0, "", input$session)
    if(a == "Race" & V$ACCESS_ACTIVE){
    selectInput("sel_P10", "P10", drivers )}})
  
  output$form_GV <- renderUI({
    a <- ifelse(length(input$session)== 0, "", input$session)
    if(a == "Race" & V$ACCESS_ACTIVE){
      selectInput("sel_GV", "GV", drivers )}})

  
  output$save_form_b <- renderUI({
    if(V$ACCESS_ACTIVE){
      actionButton("save_form", "Salva")
    }
  }
  )
  
  
  observeEvent(input$save_form, {
    V$saved_formation <- T
    if(input$session == "Quali"){
      fn <- paste0("db/users/", input$usr, "/quali_form.csv")
      df <- c(as.character(now(tz="GMT")),input$sel_P1,
              input$sel_P2,input$sel_P3,input$sel_P4,input$sel_P5)
      df2 <- read.csv(fn, row.names = 1)
      df2[V$active_GP_id, ] <- df
      write.csv(df2, fn)
    }
    else if(input$session == "Sprint"){
      fn <- paste0("db/users/", input$usr, "/sprint_form.csv")
      df <- c(as.character(now(tz="GMT")),input$sel_P1,
              input$sel_P2,input$sel_P3,input$sel_P4,input$sel_P5)
      df2 <- read.csv(fn, row.names = 1)
      df2[V$active_GP_id, ] <- df
      write.csv(df2, fn)
    } 
    else if(input$session == "Race"){
      fn <- paste0("db/users/", input$usr, "/race_form.csv")
      df <- c(as.character(now(tz="GMT")),input$sel_P1,
              input$sel_P2,input$sel_P3,input$sel_P4,input$sel_P5,
              input$sel_P6,input$sel_P7,input$sel_P8,input$sel_P9,
              input$sel_P10,input$sel_GV)
      df2 <- read.csv(fn, row.names = 1)
      df2[V$active_GP_id, ] <- df
      write.csv(df2, fn)
    }
  })
  
  output$save_form_m <- renderText({
    if(V$saved_formation){
      "Saved!"
    }
  })
  
  ################################# Poglej formazioni #############
  output$sel_shw_form_gp <- renderUI({
    hist_GP_list <- GPs[1:V$active_GP_id,]
    selectInput("shw_form_gp", label =  "Zberi GP:",
                choices = hist_GP_list$Event,
                selected = hist_GP_list$Event[V$active_GP_id])
    })
  
  output$sel_shw_form_sess <- renderUI({
    gp <- which(GPs$Event == input$shw_form_gp)
    if(GPs$is_sprint[gp]){
      cc <- c("Sprint", "Quali", "Race")
    }else{
      cc <- c("Quali", "Race")
    }
    print(cc)
    radioButtons("shw_form_session",
                 label = "Session:",
                 choices = cc)
  } )
  
  output$formazioni_tbl <- renderTable({
    gp <- which(input$shw_form_gp == GPs$Event)
    sess <- input$shw_form_session
    if (input$shw_form_usr == "Vsi"){
      usrs <- users$user
    }else{
      usrs <- c(input$shw_form_usr)
    }
    
    df <- data.frame()
    for (u in usrs){
      fn <- paste0("db/users/", u, "/", tolower(sess), "_form.csv")
      df <- rbind(df, read.csv(fn, row.names = 1)[gp,-1])
    }
    df$Username <- usrs
    df[,c("Username", colnames(df)[-ncol(df)])]
  })
###########################################Rezultati individuali###########
  
  output$sel_class_ind_gp <- renderUI({
    # hist_GP_list <- 0
    # if (GPs$is_sprint[V$active_GP_id]){
    #   hist_GP_list <- ifelse(as.POSIXct(GPs$Time_Sprint[V$active_GP_id], tz = "GMT") < (now(tzone = "GMT") - hours(2)),
    #                          GPs[1:V$active_GP_id],
    #                          GPs[1:(V$active_GP_id-1)])
    # }else{
    #   hist_GP_list <- ifelse(as.POSIXct(GPs$Time_Quali[V$active_GP_id], tz = "GMT") < (now(tzone = "GMT") - hours(2)),
    #                         GPs$Event[1:V$active_GP_id],
    #                         GPs$Event[1:(V$active_GP_id-1)])
    # }
    hist_GP_list <- GPs$Event[1:V$concluded_GP_id]
    selectInput("class_ind_gp", label =  "Zberi GP:",
                choices = hist_GP_list,
                selected = hist_GP_list[length(hist_GP_list)])
  })
  
  # output$sel_class_ind_sess <- renderUI({
  #   #sel_gp <- which(GPs$Event == input$class_ind_gp)
  #   sel_gp <- V$concluded_GP_id
  #   if(GPs$is_sprint[sel_gp]){
  #     hist_sess_list <- c("Sprint")
  #     if(as.POSIXct(GPs$Time_Quali[sel_gp], tz = "GMT") < now(tz = "GMT") + hours(3)){
  #       hist_sess_list <- c(hist_sess_list,"Quali")
  #     }
  #     if(as.POSIXct(GPs$Time_Race[sel_gp], tz = "GMT") < now(tz = "GMT") + hours(3)){
  #       hist_sess_list <- c(hist_sess_list,"Race")
  #     }
  #   } else{
  #     hist_sess_list <- c("Quali")
  #     if(as.POSIXct(GPs$Time_Race[sel_gp], tz = "GMT") < now(tz = "GMT") + hours(3)){
  #       hist_sess_list <- c(hist_sess_list,"Race")
  #     }
  #   }
  #   
  #   selectInput("class_ind_sess", label =  "Session:",
  #               choices = hist_sess_list,
  #               selected = hist_sess_list[length(hist_sess_list)])
  # })

  punteggio_ind <- reactive({
    gp_names <- GPs$Event[1:V$concluded_GP_id]
    results_Q <- data.frame()
    results_S <- data.frame()
    results_R <- data.frame()
    for (g in gp_names){
      df <- read_csv(paste0("db/GP_results/", g, ".csv"))
      gv <- filter(df, Session == "GV") %>% select(P1)
      results_R <- rbind(results_R, filter(df, Session == "Race") %>%
                           mutate(GP = g, GV = as.character(gv)) %>%
                           select(GP, Session, P1:P10, GV))
      results_Q <- rbind(results_Q, filter(df, Session == "Quali") %>%
                           mutate(GP = g) %>%
                           select(GP, Session, P1:P5))
      results_S <- rbind(results_S, filter(df, Session == "Sprint") %>%
                           mutate(GP = g) %>%
                           select(GP, Session, P1:P5))
    }
    
    df <- data.frame(Username = c(),
                     time = c(),
                     GP = c(),
                     Session = c(),
                     pts = c()
                     )
    
    for (usr in users$user){
      fn_quali <- paste0("db/users/", usr, "/quali_form.csv")
      fn_race <- paste0("db/users/", usr, "/race_form.csv")
      fn_sprint <- paste0("db/users/", usr, "/sprint_form.csv")
      
      df_q <- read_csv(fn_quali, show_col_types = F)[1:V$concluded_GP_id,  -1]
      df_s <- read_csv(fn_sprint, show_col_types = F)[1:V$concluded_GP_id,  -1]
      df_r <- read_csv(fn_race, show_col_types = F)[1:V$concluded_GP_id,  -1]
      df <- rbind(df, data.frame(calculate_usr_scores(df_q, results_Q, usr, "Quali")))
      if (dim(results_S)[1] != 0){
        df <- rbind(df, data.frame(calculate_usr_scores(df_s, results_S, usr, "Sprint")))
      }
      df <- rbind(df, data.frame(calculate_usr_scores(df_r, results_R, usr, "Race")))
    }
    df
  })  
  
  output$plot_class_ind2 <- renderPlot({
    prev_gps <- 1: which(GPs$Event == input$class_ind_gp)
    df <- punteggio_ind() %>% filter(GP %in% GPs$Event[prev_gps]) 
    
    s_order <- df %>% group_by(Username) %>% 
      summarise(pts = sum(pts, na.rm = T),) %>% 
      arrange(pts) %>%
      select(Username)  %>% mutate(Username = as.character(Username))
    
    df %>% group_by(Username, Session) %>%
      mutate(Username = factor(Username, levels = as.character(s_order$Username))) %>%
      summarise(pts = sum(pts, na.rm = T),) %>%  
      ggplot(., aes(y = Username, x = pts, fill = Session)) +
      geom_bar(stat = "identity") + ggtitle(paste("Classifica totale po koncanm ", input$class_ind_gp))
    
  })
  
  output$plot_class_ind1 <- renderPlot({
    
    df <- punteggio_ind() %>% filter(GP == input$class_ind_gp)
    
    s_order <- df %>% group_by(Username) %>% 
      summarise(pts = sum(pts, na.rm = T),) %>% 
      arrange(pts) %>%
      select(Username)  %>% mutate(Username = as.character(Username))
    
    df %>% group_by(Username, Session) %>%
      mutate(Username = factor(Username, levels = as.character(s_order$Username))) %>%
      summarise(pts = sum(pts, na.rm = T),) %>%  
      ggplot(., aes(y = Username, x = pts, fill = Session)) +
      geom_bar(stat = "identity") + ggtitle(paste("Classifica za", input$class_ind_gp))
    
  })
  
  ###################################### Skuadreeee #################
  output$sel_class_sku_gp <- renderUI({
    hist_GP_list <- GPs$Event[1:V$concluded_GP_id]
    selectInput("class_sku_gp", label =  "Zberi GP:",
                choices = hist_GP_list,
                selected = hist_GP_list[length(hist_GP_list)])
  })
  
  
  
  output$plot_class_sku2 <- renderPlot({
    prev_gps <- 1: which(GPs$Event == input$class_sku_gp)
    df_t <- teams %>% pivot_longer(cols = c(driver_1, driver_2),
                                   names_to = "Driver", values_to = "driver_id")
    df_u <- users %>% left_join(df_t, by = c("user_id" = "driver_id"))
    
    df <- punteggio_ind() %>% filter(GP %in% GPs$Event[prev_gps]) %>% 
      left_join(df_u, by = c("Username" = "user" )) %>% drop_na()
    
    s_order <- df %>% group_by(team) %>% 
      summarise(pts = sum(pts, na.rm = T),) %>% 
      arrange(pts) %>%
      select(team)  %>% mutate(team = as.character(team))
    
    df %>% group_by(team, Driver, Username) %>%
      mutate(team = factor(team, levels = as.character(s_order$team))) %>%
      summarise(pts = sum(pts, na.rm = T),) %>%  
      mutate(is_d1 = as.integer(Driver == "driver_1")) %>%
      ggplot(.) +
      geom_bar( aes(y = team, x = pts, fill = Driver), stat = "identity", show.legend = F) + 
      geom_text( aes(y = team, x = pts *(1-is_d1) + is_d1*3.2, fill = Driver, label = Username),
                 position = "stack", hjust = 1) + ggtitle(paste("Classifica totale po koncanm ", input$class_sku_gp))
    
  })
  
  output$plot_class_sku1 <- renderPlot({
    df_t <- teams %>% pivot_longer(cols = c(driver_1, driver_2),
                                  names_to = "Driver", values_to = "driver_id")
    df_u <- users %>% left_join(df_t, by = c("user_id" = "driver_id"))
    
    df <- punteggio_ind() %>% filter(GP == input$class_sku_gp) %>% 
      left_join(df_u, by = c("Username" = "user" )) %>% drop_na()
    
    s_order <- df %>% group_by(team) %>% 
      summarise(pts = sum(pts, na.rm = T),) %>% 
      arrange(pts) %>%
      select(team)  %>% mutate(team = as.character(team))
    
    df %>% group_by(team, Driver, Username) %>%
      mutate(team = factor(team, levels = as.character(s_order$team))) %>%
      summarise(pts = sum(pts, na.rm = T),) %>% 
      mutate(is_d1 = as.integer(Driver == "driver_1")) %>%
      ggplot(., ) +
      geom_bar(aes(x = team, y = pts, fill = Driver),
               stat = "identity", show.legend = F) + 
      geom_text(aes(x = team, y = pts *(1-is_d1) + is_d1*3.2, fill = Driver, label = Username), position = "stack", hjust = 1.) + 
      ggtitle(paste("Classifica za", input$class_sku_gp))+
      coord_flip()
  })
  
  ############# Home ###########################
  
    output$time_left <- renderText({
    if(GPs$is_sprint[V$active_GP_id] & as.POSIXct(GPs$Time_Sprint[V$active_GP_id], tz = "GMT") > now(tz = "GMT")){
      V$active_sprint <- T
    }
    if(as.POSIXct(GPs$Time_Quali[V$active_GP_id], tz = "GMT") > now(tz = "GMT")){
      V$active_quali <- T
    }
    if(as.POSIXct(GPs$Time_Race[V$active_GP_id], tz = "GMT") > now(tz = "GMT")){
      V$active_race <- T
    }
    
    invalidateLater(1000, session)
    
    if (V$active_sprint){
      dtp <- seconds_to_period(
        as.duration(
          difftime(as.POSIXct(GPs$Time_Sprint[V$active_GP_id], tz = "GMT") - hours(2),
                   now(tz = "GMT"))))
      paste0("Za oddat formazion je še: ",
             day(dtp), " dni in ", hour(dtp), ":", minute(dtp), ":", round(second(dtp))
      )  
    }else if (V$active_quali){
      dtp <- seconds_to_period(
        as.duration(
          difftime(as.POSIXct(GPs$Time_Quali[V$active_GP_id], tz = "GMT") - hours(2),
                   now(tz = "GMT"))))
      paste0(
            "Za oddat formazion je še: ",
            day(dtp), " dni in ", hour(dtp), ":", minute(dtp), ":", round(second(dtp))
            )  
    }else if (V$active_race){
      dtp <- seconds_to_period(
        as.duration(
          difftime(as.POSIXct(GPs$Time_Race[V$active_GP_id], tz = "GMT") - hours(2),
                   now(tz = "GMT"))))
      paste0(
             "Za oddat formazion je še: ",
             day(dtp), " dni in ", hour(dtp), ":", minute(dtp), ":", round(second(dtp))
      )  
    } else{
      paste("", "")
    }
    
  })
  
  
  output$sess_act_txt <- renderText({
    if(GPs$is_sprint[V$active_GP_id] & as.POSIXct(GPs$Time_Sprint[V$active_GP_id], tz = "GMT") > now(tz = "GMT")){
      V$active_sprint <- T
    }
    if(as.POSIXct(GPs$Time_Quali[V$active_GP_id], tz = "GMT") > now(tz = "GMT")){
      V$active_quali <- T
    }
    if(as.POSIXct(GPs$Time_Race[V$active_GP_id], tz = "GMT") > now(tz = "GMT")){
      V$active_race <- T
    }
    
    if (V$active_sprint){
      paste(GPs$Event[V$active_GP_id],  " - Sprint ")
    }else if (V$active_quali){
      paste(GPs$Event[V$active_GP_id],  " - Quali ")
    } else if (V$active_race){
      paste(GPs$Event[V$active_GP_id],  " - Race ")
    } else{
      paste("-- No active session for the ", GPs$Event[V$active_GP_id])
    }
  })
 
 output$dl_backup <- downloadHandler(
   filename = function(){
     paste("backup.zip")
   },
   content = function(file){
     zip(zipfile = file, files =  "db/") 
   }
  ) 
}


# Run the application 
shinyApp(ui = ui, server = server)

