#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(bslib)
library(janitor)
library(readr)

data <- read.csv("Fall2020.csv")

data$Date <- as.Date(data$Date, "%m/%d/%y")
deg2rad <- function(deg) {(deg * pi) / (180)}


data <- data %>%
    mutate(Type = case_when(
        TaggedPitchType %in% c("Fastball", "Sinker") ~ "FB",
        TaggedPitchType == "ChangeUp" ~ "CH",
        TaggedPitchType == "Slider" ~ "SL",
        TaggedPitchType == "Curveball" ~ "CB"),
        Count = paste(Balls, Strikes, sep = "-"))

squat_jump <- read_csv("squat_jump_master.csv")
squat_jump <- squat_jump %>%
    filter(Tags == "Profiling")

squat_jump <- clean_names(squat_jump)

power_output <- squat_jump %>%
    select(name, type, peak_propulsive_power, peak_propulsive_force)

mean(power_output$peak_propulsive_power) # 4782.293 is the mean of the dataset
mean(power_output$peak_propulsive_force) # 2105.867 is the mean of the dataset

sd(power_output$peak_propulsive_power) # 837.1419 is the sd of the dataset
sd(power_output$peak_propulsive_force) # 305.2852 is the sd of the dataset

power_output <- power_output %>%
    mutate(power_z_score = (peak_propulsive_power - 4805.314) / 930.0187,
           force_z_score = (peak_propulsive_force - 2034.903) / 331.229) %>%
    filter(name != "Weston Mazey")

power_output_max <- power_output %>%
    group_by(name) %>%
    slice(which.max(peak_propulsive_power))


sj_power <- squat_jump %>%
    filter(name != "Weston Mazey") %>%
    group_by(name) %>%
    slice(which.max(peak_propulsive_power)) %>%
    select(name, type, peak_propulsive_power) %>%
    mutate(peak_propulsive_power = round(peak_propulsive_power)) %>%
    arrange(desc(peak_propulsive_power))

sj_force <- squat_jump %>%
    filter(name != "Weston Mazey") %>%
    group_by(name) %>%
    slice(which.max(peak_propulsive_force)) %>%
    select(name, type, peak_propulsive_force) %>%
    arrange(desc(peak_propulsive_force))

# Read in sim games and color code velocity 
sim_games <- read_csv("fall_sim_games.csv")
sim_games %>%
    separate(Pitcher, c("last", "first")) -> sim_games2
sim_games2$name <- paste(sim_games2$first, sim_games2$last, sep = " ")

merge_df <- sim_games2 %>%
    group_by(name) %>%
    summarize('Velo' = mean(RelSpeed, na.rm = T))

power_viz <- power_output_max %>%
    left_join(merge_df, by = "name")

# Define UI for application that draws a histogram
ui <- navbarPage(title = "WVU Player Development",
                 tabPanel(title = "Home",
                          imageOutput("wvu_img"),
                          imageOutput("home_img", height = "320px"),
                          br(),
                          hr(),
                          h4(strong("App Description")),
                          p(style="text-align: justify; font-size = 25px",
                            "This application is to centralize player development information for our program", 
                            em("demonstrating various use of shiny features."), 
                            "Everyone will recieve an account to access the app in the future",
                          tags$blockquote("WVU PD App is still under continuous development. 
           Please look forward to future updates!")),
                          hr()),
                 tabPanel(title = "Pitching",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      fluidRow(
                                          column(6, selectInput(inputId = "Pitcher", label = "Select Pitcher", choices = sort(unique(data$Pitcher)))),
                                      ),
                                      fluidRow(
                                          column(5, selectInput(inputId = "Date", label = "Select Game", choices = ""))
                                      )
                                      # fluidRow(
                                      #     column(4, selectInput(inputId = "Count", label = "Select Count", choices = sort(unique(data$Count))))
                                      #          )
                                  ),
                          mainPanel(
                              wellPanel(style = "background: white; border-color:black; border-width:2px",
                                        fluidRow(
                                            column(2, img(src = "wv_pic.png", height = 100, width = 100), align = "center"), 
                                            column(4, h2(strong(textOutput("selected_pitcher"))), hr(style="border-color: black;"), style = "padding-right:0px;"),
                                            column(6, h2("Pitching Development"), hr(style="border-color: black;"), h2(textOutput("selected_game")), align = "center", style = "padding-left:0px;"))),
                              tabsetPanel(
                                  type = "tabs",
                                  tabPanel("Post-Game Report",
                                           wellPanel(style = "background: white; border-color:black; border-width:3px",
                                                     fluidRow(
                                                         column(width = 10.5, h3(strong("Pitcher Summary Table")), dataTableOutput("pitcher_summary_table"), align = "center")
                                                     ), br(), br(),
                                                     fluidRow(column(4, plotOutput("pitch_loc"), align = "center"),
                                                              column(4, plotOutput("pitch_sm"), align = "center"),
                                                              column(4, plotOutput("pitch_hh"), align = "center")
                                                     ), br(), br(), br(), br())),
                                  tabPanel("Game Charts",
                                           wellPanel(style = "background: white; border-color:black; border-width:2px",
                                                     fluidRow(
                                                         column(6, plotOutput("pitch_velo"), align = "center"),
                                                         column(6, plotOutput("move_plot"), align = "center")),
                                                         fluidRow(
                                                         column(6, plotOutput("release_point"), align = "center")
                                                     ), br(), br())))
                          )
                          )
                          )),
                 # tabPanel(title = "Hitting",
                 #          fluidPage(
                 #              sidebarLayout(
                 #                  sidebarPanel(
                 #                      fluidRow(
                 #                          column(6, selectInput(inputId = "Batter", label = "Select Batter", choices = sort(unique(data$Batter))))
                 #                      ),
                 #                      fluidRow(
                 #                          column(5, selectInput(inputId = "Date", label = "Select Game", choices = ""))
                 #                      )),
                 #                  mainPanel(
                 #                      wellPanel(style = "background: white; border-color:black; border-width:2px",
                 #                                fluidRow(
                 #                                    column(2, img(src = "wv_pic.png", height = 100, width = 100), align = "center),
                 #                                    column(6, h2("Hitting Development"), hr(style="border-color: black;"), h2(textOutput("selected_game")), align = "center", style = "padding-left:0px;"))),
                 #                                   ))))))),
                          
                 tabPanel(title = "Sport Sciences",
                          tabsetPanel(
                                      type = "tabs",
                            tabPanel("Squat Jump",
                              fluidRow(plotOutput("force_plot"), align = "center"),
                              fluidRow(
                                  column(5, h3(strong("Power Leaderboards")), dataTableOutput("power_table"), align = "center"),
                                  column(7, h3(strong("Force Leaderboards")), dataTableOutput("force_table"), align = "center"))
                          ))),
                 inverse = T
)


   

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$wvu_img <- renderImage({
        list(src = "www/wvu_pic.png",
             width = "100%",
             height = 330)
        
    }, deleteFile = F)
    
    observeEvent(input$Pitcher,
                 updateSelectInput(session, inputId = "Date", label = "Select Game",
                                   choices = sort(unique(data$Date[data$Pitcher == input$Pitcher]))))
    
    output$selected_pitcher <- renderText({paste(input$Pitcher)})
    
    output$selected_game <- renderText({paste(input$Date)})
    output$selected_batter <- renderText({paste(input$Batter)})
    
    # output$count <- renderText({paste(input$Count)})
    # 
    # observeEvent(input$Count,
    #              updateSelectInput(session, inputId = "Count", label = "Select Count",
    #                                choices = sort(unique(data$Count[data$Pitcher == input$Pitcher]))))
    
    output$pitcher_summary_table <- renderDataTable({
        table <- data %>%
            mutate(region = ifelse((PlateLocSide >= -0.558 & PlateLocSide <= 0.558 & PlateLocHeight >= 1.83 & PlateLocHeight <= 3.167), "Heart", 
                                   ifelse((PlateLocSide >= -1.11 & PlateLocSide <= 1.11 & PlateLocHeight >= 1.167 & PlateLocHeight <= 3.833) , "Shadow" ,
                                          ifelse((PlateLocSide >= -1.67 & PlateLocSide <= 1.67 & PlateLocHeight >= 0.1667 & PlateLocHeight <= 4.5) , "Chase" , "Waste")))) %>%
            mutate('Zone' = ifelse(PlateLocSide >= -0.83 & PlateLocSide <= 0.83 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5,1,0),
                   'ChaseP' = ifelse(region == "Chase",1,0),
                   'ChaseS' = ifelse(PitchCall == "StrikeSwinging" & region == "Chase", 1,0),
                   Type = case_when(
                       TaggedPitchType %in% c("Fastball", "Sinker") ~ "FB",
                       TaggedPitchType == "ChangeUp" ~ "CH",
                       TaggedPitchType == "Slider" ~ "SL",
                       TaggedPitchType == "Curveball" ~ "CB"
                   )) %>%
            filter(Pitcher == input$Pitcher, Date == input$Date, !TaggedPitchType %in% c("Undefined", "Other")) %>%
            group_by(Pitch = TaggedPitchType) %>%
            summarise('Pitches Thrown' = length(TaggedPitchType),
                      'Velo.' = paste(round(quantile(RelSpeed, na.rm =T, c(0.25))), round(quantile(RelSpeed, na.rm =T, c(0.75))), sep = "-"),
                      'Max Velo.' = round(max(RelSpeed, na.rm = T)),
                      'Avg Spin Rate.' = round(mean(SpinRate, na.rm = T)),
                      'HB' = round(mean(HorzBreak, na.rm = T),digits = 1),
                      'VB' = round(mean(InducedVertBreak, na.rm = T),digits = 1),
                      'VAA' = round(mean(VertApprAngle, na.rm = T),digits = 1),
                      'Chase%' = 100 * round(sum(ChaseS)/sum(ChaseP),digits = 2),
                      'Whiff%' = 100 * round(sum(PitchCall %in% c("StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall"))),3),
                      'Strike%' =  100 * round(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay"))/ n(),3)) %>%
            replace_na(list(`Whiff%`=0,`Chase%`=0)) %>%
            arrange(desc(Pitch == "Fastball"))
        
        tableFilter <- reactive({table})
        datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = TRUE))), rownames = FALSE) %>%
            formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
    })
    
    output$pitch_velo <- renderPlot({
        pitch_filter <- reactive({
            data %>%
                filter(Pitcher == input$Pitcher, Date == input$Date, TaggedPitchType != "NA", Type != "NA") %>%
                mutate(pitch_count = row_number())
        })
        
        ggplot(pitch_filter(), aes(x = pitch_count, y = RelSpeed, color = Type)) +
            geom_line(size = 1) +
            theme_bw() +
            labs(x = "Pitch Count", y = "Velocity (MPH.)", color = "Pitch Type", title = "Pitch Velocity") +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    }, width = 350, height = 350)
    
    output$pitch_loc <- renderPlot({
        pitch_loc_filter <- reactive({
            data %>%
                filter(Pitcher == input$Pitcher, Date == input$Date, Type != "NA")
        })
        ggplot(pitch_loc_filter(),aes(x = PlateLocSide*-1, y = PlateLocHeight, color = TaggedPitchType)) +
            geom_point(na.rm = T, size = 3) +
            geom_rect(xmin = -0.83,
                      xmax = 0.83, 
                      ymin = 1.5, 
                      ymax = 3.5, color = "black", fill = "transparent") + # Strikezone 
            geom_rect(xmin = -1.10833333,
                      xmax = 1.10833333, 
                      ymin = 1.16666667,
                      ymax = 3.83333333, color = "black", linetype = "dashed", fill = "transparent") +
            geom_segment(aes(x = 0.275, y = 1.5, xend = 0.275, yend = 3.5), color = "black") +
            geom_segment(aes(x = -0.275, y = 1.5, xend = -0.275, yend = 3.5), color = "black") +
            geom_segment(aes(x = -0.83, y = 2.83, xend = 0.83, yend = 2.83), color = "black") +
            geom_segment(aes(x = -0.83, y = 2.16, xend = 0.83, yend = 2.16), color = "black") +
            geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 0.5, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 0.5, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black") + 
            geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black") + 
            geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black") +
            coord_equal() +
            scale_x_continuous(limits = c(-2,2)) +
            scale_y_continuous(limits = c(0,5)) +
            theme_bw() +
            labs(title = "Pitch Location", color = "Type", y = "", x = "") +
            theme(legend.position = "bottom",
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    },width = 250, height = 350)
    
    output$pitch_sm <- renderPlot({
        sm_filter <- reactive({
            data %>%
                filter(Pitcher == input$Pitcher, Date == input$Date, Type != "NA", PitchCall == "StrikeSwinging")
        })
        ggplot(sm_filter(), aes(x = PlateLocSide*-1, y = PlateLocHeight, color = TaggedPitchType)) +
            geom_point(na.rm = T, size = 3) +
            geom_rect(xmin = -0.83,
                      xmax = 0.83, 
                      ymin = 1.5, 
                      ymax = 3.5, color = "black", fill = "transparent") + # Strikezone 
            geom_rect(xmin = -1.10833333,
                      xmax = 1.10833333, 
                      ymin = 1.16666667,
                      ymax = 3.83333333, color = "black", linetype = "dashed", fill = "transparent") + #Shadow Zone +
            geom_segment(aes(x = 0.275, y = 1.5, xend = 0.275, yend = 3.5), color = "black") +
            geom_segment(aes(x = -0.275, y = 1.5, xend = -0.275, yend = 3.5), color = "black") +
            geom_segment(aes(x = -0.83, y = 2.83, xend = 0.83, yend = 2.83), color = "black") +
            geom_segment(aes(x = -0.83, y = 2.16, xend = 0.83, yend = 2.16), color = "black") +
            geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 0.5, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 0.5, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black") + 
            geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black") + 
            geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black") +
            coord_equal() +
            scale_x_continuous(limits = c(-2,2)) +
            scale_y_continuous(limits = c(0,5)) +
            theme_bw() +
            labs(title = "Swing and Miss", y = "", x = "", color = "Type") +
            theme(legend.position = "bottom",
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    },width = 250, height = 350)
    
    output$pitch_hh <- renderPlot({
        hh_filter <- reactive({ 
            data %>%
                filter(Pitcher == input$Pitcher, Date == input$Date, Type != "NA", ExitSpeed >= 95)
        })
        ggplot(hh_filter(), aes(x = PlateLocSide*-1, y = PlateLocHeight, color = TaggedPitchType)) +
            geom_point(na.rm = T, size = 3) +
            geom_rect(xmin = -0.83,
                      xmax = 0.83, 
                      ymin = 1.5, 
                      ymax = 3.5, color = "black", fill = "transparent") + # Strikezone 
            geom_rect(xmin = -1.10833333,
                      xmax = 1.10833333, 
                      ymin = 1.16666667,
                      ymax = 3.83333333, color = "black", linetype = "dashed", fill = "transparent") +
            geom_segment(aes(x = 0.275, y = 1.5, xend = 0.275, yend = 3.5), color = "black") +
            geom_segment(aes(x = -0.275, y = 1.5, xend = -0.275, yend = 3.5), color = "black") +
            geom_segment(aes(x = -0.83, y = 2.83, xend = 0.83, yend = 2.83), color = "black") +
            geom_segment(aes(x = -0.83, y = 2.16, xend = 0.83, yend = 2.16), color = "black") +
            geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 0.5, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 0.5, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black") + 
            geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black") + 
            geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black") +
            coord_equal() +
            scale_x_continuous(limits = c(-2,2)) +
            scale_y_continuous(limits = c(0,5)) +
            theme_bw() +
            labs(title = "Hard Hit", y = "", x = "", color = "Type") +
            theme(legend.position = "bottom",
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    }, width = 250, height = 350)
    
    output$move_plot <- renderPlot({
        move_filter <- reactive({
            data %>%
                filter(Pitcher == input$Pitcher, Date == input$Date, TaggedPitchType != "NA", Type != "NA") %>%
                mutate(pitch_count = row_number())
        })
        ggplot(move_filter(), aes(x = HorzBreak, y = InducedVertBreak, color = Type)) +
            geom_point() +
            theme_bw() +
            scale_x_continuous(limits = c(-30,30), breaks = seq(-30,30, by = 10)) +
            scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30, by = 10)) +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0) +
            labs(x = "", y = "", title = "Pitch Movement") +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    },width = 350, height = 350)
    
    output$release_point <- renderPlot({
        release_filter <- reactive({
            data %>%
                filter(Pitcher == input$Pitcher, Date == input$Date, Type != "NA")
        })
        ggplot(release_filter(), aes(x = RelSide*-1, y = RelHeight, color = Type)) +
            geom_point() +
            scale_x_continuous(limits = c(-3,3), breaks = seq(-4,4,by=1)) +
            scale_y_continuous(limits = c(0,8), breaks = seq(0,8,by=2)) +
            geom_vline(xintercept = 0) +
            geom_rect(xmin = -0.83,
                      xmax = 0.83,
                      ymin = 1.5,
                      ymax = 3.5, color = "black", fill = "transparent") +
            theme_bw() +
            labs(x = "", y = "", color = "Pitch Type", title = "Release Point") +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    },width = 350, height = 350)
    
    output$power_table <- renderDataTable(sj_power, options = list(pagelength=5))
    output$force_table <- renderDataTable(sj_force, options = list(pagelength=5))
    
    output$force_plot <- renderPlot(
        ggplot(power_viz, aes(power_z_score, force_z_score, color = Velo)) +
            geom_point() +
            scale_x_continuous("Z-Score Squat Jump Peak Power", limits = c(-4,4), breaks = seq(-4,4, by = 1)) +
            scale_y_continuous("Z-Score Peak Force", limits = c(-4,4), breaks = seq(-4,4,by = 1)) +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0) +
            annotate(x=-3, y = 3, geom = "text", label = "High Force Low Power", color = "red") +
            annotate(geom = "text", label = "High Force High Power", color = "red", x = 2, y = 3) +
            annotate(geom = "text", label = "Low Force Low Power", x = -3, y = -1, color = "red") +
            annotate(geom = "text", label = "Low Force High Power", color = "red", x = 3, y = -2) +
            geom_text(aes(label= ifelse(power_z_score > quantile(power_z_score, 0.90),
                                        as.character(name),'')),hjust=0,vjust=0) +
            geom_text(aes(label= ifelse(power_z_score < quantile(power_z_score, 0.10),
                                        as.character(name),'')),hjust=0,vjust=0) +
            labs(title = "Net Peak Force vs Squat Jump Peak Power") +
            theme_bw() 
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
