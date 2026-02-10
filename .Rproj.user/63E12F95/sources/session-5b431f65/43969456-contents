library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(Hmisc)
library(scales)
library(shinyjs)
library(patchwork)

sol1 <- read.csv("riddler-castles-data/castle-solutions.csv")
sol1 <- sol1[1:10]
sol1_long <- gather(sol1, key = "Castle", value = "Value")
sol2 <- read.csv("riddler-castles-data/castle-solutions-2.csv")
sol2 <- sol2[1:10]
sol2_long <- gather(sol2, key = "Castle", value = "Value")
sol3 <- read.csv("riddler-castles-data/castle-solutions-3.csv")
sol3 <- sol3[1:10]
sol3_long <- gather(sol3, key = "Castle", value = "Value")
sol4 <- read.csv("riddler-castles-data/castle-solutions-4.csv")
sol4_long <- gather(sol4, key = "Castle", value = "Value")
temp_solA <- rbind(sol1, sol2)
temp_solB <- rbind(sol3, sol4)
sol5 <- rbind(temp_solA, temp_solB)
sol5_long <- gather(sol5, key = "Castle", value = "Value")
troopCommitMax <- 100

# Checks who sent more troops to each castle
castle_win <- function(player1, player2) {
  result_vector <- c(length = length(player1))
  for (i in seq_along(player1)) {
    if (player1[i] > player2[i]) {
      result_vector[i] <- "Player 1"
    } else if(player1[i] < player2[i]){
      result_vector[i] <- "Player 2"
    } else {
      result_vector[i] <- "Tie"
    }
  }
  
  return(result_vector)
}

#Gets scores from vector of castles won
score_calc <- function(result_vector) {
  unique_names <- unique(result_vector)
  scores <- numeric(length = length(unique_names))
  for (i in seq_along(unique_names)) {
    name <- unique_names[i]
    positions <- which(result_vector == name)
    scores[i] <- sum(positions)
  }
  result_scores <- setNames(scores, unique_names)
  return(result_scores)
}

# Allows riddle description to be posted on the sidebar
sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}

# UI 
ui <- fluidPage(theme = shinytheme("united"),
  useShinyjs(),

    # Application title
    titlePanel("The Battle for Riddler Nation!"),

    # Sidebar with a slider input for data set number
    sidebarLayout(
        sidebarPanel2(
          sliderInput(
              "set",
              "Dataset (for comparing historical game results)",
              value = 1,
              min = 1,
              max = 5,
              step = 1
           ),
        p("1=Game 1 \n 2=Game 2 \n 3=Game 3 \n 4=Game 4\n 5=All Games"),
        out = p("The riddle: \n In a distant, war-torn land, there are 10 castles. There are 
                 two warlords: you and your archenemy, with whom you’re 
                 competing to collect the most victory points. Each castle has 
                 its own strategic value for a would-be conqueror. Specifically,
                 the castles are worth 1, 2, 3, 4, 5, 6, 7, 8, 9, and 10 victory points. 
                 You and your enemy each have 100 soldiers to distribute, any 
                 way you like, to fight at any of the 10 castles. Whoever sends
                 more soldiers to a given castle conquers that castle and wins 
                 its victory points. If you each send the same number of troops,
                 neither player gets the points. You don’t know what distribution of 
                 forces your enemy has chosen until the battles begin. 
                 Whoever wins the most points wins the war. \n ")
        ),

        # Shows slider bars for each castle
        mainPanel(
           h4("Player 1: Input the number of troops to send to each castle"),
           sliderInput(
             "troop[1]",
             "Troops to send to Castle 1",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[2]",
             "Troops to send to Castle 2",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[3]",
             "Troops to send to Castle 3",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[4]",
             "Troops to send to Castle 4",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[5]",
             "Troops to send to Castle 5",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[6]",
             "Troops to send to Castle 6",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[7]",
             "Troops to send to Castle 7",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[8]",
             "Troops to send to Castle 8",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[9]",
             "Troops to send to Castle 9",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[10]",
             "Troops to send to Castle 10",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           verbatimTextOutput("troopCommittedDisplay"),
           verbatimTextOutput("troopCommittedAmount"),  
           conditionalPanel(
             condition = "output.troopCommittedAmount === 'At troop limit, press Next Player to continue'",
             actionButton("nextBtn", "Next Player")
             ),
           h4("Player 2: Input the number of troops to send to each castle"),
           sliderInput(
             "troop[11]",
             "Troops to send to Castle 1",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[12]",
             "Troops to send to Castle 2",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[13]",
             "Troops to send to Castle 3",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[14]",
             "Troops to send to Castle 4",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[15]",
             "Troops to send to Castle 5",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[16]",
             "Troops to send to Castle 6",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[17]",
             "Troops to send to Castle 7",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[18]",
             "Troops to send to Castle 8",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[19]",
             "Troops to send to Castle 9",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           sliderInput(
             "troop[20]",
             "Troops to send to Castle 10",
             value = 0,
             min = 0,
             max = troopCommitMax,
             step = 1
           ),
           verbatimTextOutput("troopCommittedDisplay2"),
           verbatimTextOutput("troopCommittedAmount2"),  
           conditionalPanel(
             condition = "output.troopCommittedAmount2 === 'At troop limit, press Submit to continue'",
             actionButton("submitBtn", "Submit"),
             plotOutput("distPlot", height = '1000px', width = '600px'
             ),
             verbatimTextOutput("winnertext")
           ),
        )
    )
)



# Server
server <- function(input, output, session) {
  nextClicked <- reactiveVal(FALSE)
  submitClicked <- reactiveVal(FALSE) 
  
  # Create reactiveValues to store troop values
  troopValues <- reactiveValues(
    troop = numeric(20)
  )
  
  updateTroopValues <- function() {
    troopValues$troop <- c(
      input[["troop[1]"]],
      input[["troop[2]"]],
      input[["troop[3]"]],
      input[["troop[4]"]],
      input[["troop[5]"]],
      input[["troop[6]"]],
      input[["troop[7]"]],
      input[["troop[8]"]],
      input[["troop[9]"]],
      input[["troop[10]"]],
      input[["troop[11]"]],
      input[["troop[12]"]],
      input[["troop[13]"]],
      input[["troop[14]"]],
      input[["troop[15]"]],
      input[["troop[16]"]],
      input[["troop[17]"]],
      input[["troop[18]"]],
      input[["troop[19]"]],
      input[["troop[20]"]]
    )
  }
  
  #Changes plot data based on value of dataset slider
  dataToUse <- reactive({
    if (input$set == 1) {
      return(sol1_long)
    } else if (input$set == 2) {
      return(sol2_long)
    } else if (input$set == 3) {
      return(sol3_long)
    } else if (input$set == 4) {
      return(sol4_long)
    } else {
      return(sol5_long)
    }
  })
  
  troop <- reactive({
    c(
      input[["troop[1]"]],
      input[["troop[2]"]],
      input[["troop[3]"]],
      input[["troop[4]"]],
      input[["troop[5]"]],
      input[["troop[6]"]],
      input[["troop[7]"]],
      input[["troop[8]"]],
      input[["troop[9]"]],
      input[["troop[10]"]],
      input[["troop[11]"]],
      input[["troop[12]"]],
      input[["troop[13]"]],
      input[["troop[14]"]],
      input[["troop[15]"]],
      input[["troop[16]"]],
      input[["troop[17]"]],
      input[["troop[18]"]],
      input[["troop[19]"]],
      input[["troop[20]"]]
    )
  })
  
  toggle("troop[11]")
  toggle("troop[12]")
  toggle("troop[13]")
  toggle("troop[14]")
  toggle("troop[15]")
  toggle("troop[16]")
  toggle("troop[17]")
  toggle("troop[18]")
  toggle("troop[19]")
  toggle("troop[20]")
  
  observe({
    # Calculate the sum of all values in the troop vector
    troopCommitted <- sum(troop())
  })
  
  #Player 1 troop logic
  output$troopCommittedDisplay <- renderText({
    paste("Total Troops Committed: ", sum(troop()[1:10]))
  })

  output$troopCommittedAmount <- renderText({
      if(sum(troop()[1:10]) == 100) { 
        paste("At troop limit, press Next Player to continue")
      } else if (sum(troop()[1:10]) > 100) {
        paste("Over troop limit!")
      } else if (sum(troop()[1:10]) < 100) {
        paste("Under troop limit, assign more troops")
      }
  })
  
  #Player 2 troop logic
  output$troopCommittedDisplay2 <- renderText({
    paste("Total Troops Committed: ", sum(troop()[11:20]))
  })
  
  output$troopCommittedAmount2 <- renderText({
    if(sum(troop()[11:20]) == 100) { 
      paste("At troop limit, press Submit to continue")
    } else if (sum(troop()[11:20]) > 100) {
      paste("Over troop limit!")
    } else if (sum(troop()[11:20]) < 100) {
      paste("Under troop limit, assign more troops")
    }
  })
  
  # Toggles hiding Player 1 sliders and showing Player 2 sliders
  observeEvent(input$nextBtn, {
    disable("nextBtn") 
    toggle("troop[1]")
    toggle("troop[2]")
    toggle("troop[3]")
    toggle("troop[4]")
    toggle("troop[5]")
    toggle("troop[6]")
    toggle("troop[7]")
    toggle("troop[8]")
    toggle("troop[9]")
    toggle("troop[10]")
    toggle("troop[11]")
    toggle("troop[12]")
    toggle("troop[13]")
    toggle("troop[14]")
    toggle("troop[15]")
    toggle("troop[16]")
    toggle("troop[17]")
    toggle("troop[18]")
    toggle("troop[19]")
    toggle("troop[20]")
    nextClicked(TRUE)
    updateTroopValues()
  })
  
  # Toggles hiding Player 2 sliders and showing final distribution
  observeEvent(input$submitBtn, {
    disable("submitBtn") 
    enable("distPlot")
    toggle("troop[11]")
    toggle("troop[12]")
    toggle("troop[13]")
    toggle("troop[14]")
    toggle("troop[15]")
    toggle("troop[16]")
    toggle("troop[17]")
    toggle("troop[18]")
    toggle("troop[19]")
    toggle("troop[20]")
    submitClicked(TRUE)
    updateTroopValues()
  })

  # Plots of troops sent to each castle
  output$distPlot <- renderPlot({
    if (submitClicked()) {
      castle_1_data <- dataToUse()[dataToUse()$Castle == "Castle.1", ]
      castle_2_data <- dataToUse()[dataToUse()$Castle == "Castle.2", ]
      castle_3_data <- dataToUse()[dataToUse()$Castle == "Castle.3", ]
      castle_4_data <- dataToUse()[dataToUse()$Castle == "Castle.4", ]
      castle_5_data <- dataToUse()[dataToUse()$Castle == "Castle.5", ]
      castle_6_data <- dataToUse()[dataToUse()$Castle == "Castle.6", ]
      castle_7_data <- dataToUse()[dataToUse()$Castle == "Castle.7", ]
      castle_8_data <- dataToUse()[dataToUse()$Castle == "Castle.8", ]
      castle_9_data <- dataToUse()[dataToUse()$Castle == "Castle.9", ]
      castle_10_data <- dataToUse()[dataToUse()$Castle == "Castle.10", ]
      
      xlims <- c(0,100)
      ylims <- c(0,0.30)
      
      p1 <- ggplot(castle_1_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 1") +
        geom_vline(aes(xintercept = troopValues$troop[1]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[11]), color = "blue", linewidth = 1.5) 
      
      p2 <- ggplot(castle_2_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 2") +
        geom_vline(aes(xintercept = troopValues$troop[2]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[12]), color = "blue", linewidth = 1.5)
      
      p3 <- ggplot(castle_3_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 3") +
        geom_vline(aes(xintercept = troopValues$troop[3]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[13]), color = "blue", linewidth = 1.5)
      
      p4 <- ggplot(castle_4_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 4") +
        geom_vline(aes(xintercept = troopValues$troop[4]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[14]), color = "blue", linewidth = 1.5)
      
      p5 <- ggplot(castle_5_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 5") +
        geom_vline(aes(xintercept = troopValues$troop[5]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[15]), color = "blue", linewidth = 1.5)
      
      p6 <- ggplot(castle_6_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 6") +
        geom_vline(aes(xintercept = troopValues$troop[6]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[16]), color = "blue", linewidth = 1.5)
      
      p7 <- ggplot(castle_7_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 7") +
        geom_vline(aes(xintercept = troopValues$troop[7]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[17]), color = "blue", linewidth = 1.5)
      
      p8 <- ggplot(castle_8_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 8") +
        geom_vline(aes(xintercept = troopValues$troop[8]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[18]), color = "blue", linewidth = 1.5)
      
      p9 <- ggplot(castle_9_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        scale_x_continuous(labels = NULL) +
        labs(subtitle = "Castle 9") +
        geom_vline(aes(xintercept = troopValues$troop[9]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[19]), color = "blue", linewidth = 1.5)
      
      p10 <- ggplot(castle_10_data, aes(x = Value, y = ..density..)) +
        geom_histogram(binwidth = 1, position = "dodge", fill = "orange", alpha = 0.7) +
        labs(subtitle = "Castle 10") +
        geom_vline(aes(xintercept = troopValues$troop[10]), color = "red", linewidth = 1.5) +
        geom_vline(aes(xintercept = troopValues$troop[20]), color = "blue", linewidth = 1.5)
      
      patchwork <- wrap_plots(p1 / p2 / p3 / p4 / p5 / p6 / p7 / p8 / p9 / p10) & 
        coord_cartesian(xlim = xlims, ylim = ylims) &
        theme_minimal() &
        scale_y_continuous(labels = percent) &
        xlab("") & ylab("") 
      
      patchwork + plot_annotation(
        title = '',
        subtitle = 'Proportion of Readers Choosing Soldiers For Each Castle',
        caption = 'Number of Soldiers Sent       Red = Player 1    Blue = Player 2    Orange Bars = Distribution of 538 Readers\' Submissions                                 '
      )
    }
  })
  
  # Uses created functions to find the winner
  wincheck <- reactive({
    score_calc(castle_win(troop()[1:10],troop()[11:20]))
    })

  # Outputs winner text
  output$winnertext <- renderText({
    if(wincheck()["Player 1"] > wincheck()["Player 2"]) { 
      paste("Player 1 wins with a score of", wincheck()["Player 1"], 
            "compared to Player 2's score of", wincheck()["Player 2"])
    } else if (wincheck()["Player 1"] < wincheck()["Player 2"]) {
      paste("Player 2 wins with a score of", wincheck()["Player 2"], 
            " compared to Player 1's score of ", wincheck()["Player 1"])
    } else {
      paste("Both players tied.")
    }
  })
  
  # Stops app to prevent crashes
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application 
  shinyApp(ui = ui, server = server)
  
  
