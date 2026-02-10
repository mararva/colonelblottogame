library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinyjs)

# ---------- Config ----------
troopCommitMax <- 100
difficulty <- reactiveVal(25)
solutions_path <- "riddler-castles-data-5p/castle-solutions-5.csv"

# ---------- Helpers ----------
castle_win <- function(player1, player2) {
  # returns vector: "Player 1", "Colonel", or "Tie" for each castle
  out <- character(length(player1))
  for (i in seq_along(player1)) {
    if (player1[i] > player2[i]) out[i] <- "Player 1"
    else if (player1[i] < player2[i]) out[i] <- "Colonel"
    else out[i] <- "Tie"
  }
  out
}

score_calc_full <- function(result_vector) {
  # Always return all keys so comparisons never become NA
  scores <- c("Player 1" = 0, "Colonel" = 0, "Tie" = 0)
  for (nm in names(scores)) {
    positions <- which(result_vector == nm)
    scores[nm] <- sum(positions)
  }
  scores
}

# Thompson Sampling Colonel (non-cheat)
pick_colonel_thompson <- function(solutions_df, post_a, post_b, K = 25) {
  n <- nrow(solutions_df)
  K <- max(1, min(K, n))
  
  # Candidate pool (keeps compute cheap if n is large)
  candidates <- sample.int(n, K, replace = FALSE)
  
  # Thompson samples for candidates
  theta <- rbeta(K, post_a[candidates], post_b[candidates])
  
  # Choose the arm with highest sampled win-prob
  best_idx <- candidates[which.max(theta)]
  
  list(
    index = best_idx,
    colonel_vec = as.numeric(solutions_df[best_idx, ]),
    theta_sample = max(theta)
  )
}



# ---------- Load historical solutions (5 castles) ----------
solutions_raw <- read.csv(solutions_path)

# Take the first 5 numeric columns as the 5-castle strategy
solutions_5 <- solutions_raw %>%
  select(where(is.numeric)) %>%
  select(1:5)

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("The Colonel Blotto Game"),
  
  div(
    style = "
    background-color: #f8f9fa;
    border: 1px solid #ddd;
    border-radius: 6px;
    padding: 12px 16px;
    margin-bottom: 15px;
  ",
    tags$p(
      strong("How the game works"),
      br(),
      "You are given ", strong("100 troops"), " to distribute across ",
      strong("5 castles"), ". After you submit your allocation, ",
      "the Colonel Blotto will respond by consulting the records of past officers ",
      "and choosing a split of soldiers to send. ",
      "Each castle is awarded to the side that commits more troops; ",
      "ties give no points to either party. ",
      "Castles are worth increasing points from top to bottom.",
      br(), br(),
      "Your objective is to find the strategy that consistently beats the Colonel and his record-book. ",
      "The win/loss record updates after every round."
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      tags$p("Allocate exactly ", strong(troopCommitMax), " troops across 5 castles."),
      numericInput("c1", "Castle 1", value = 0, min = 0, max = troopCommitMax, step = 1),
      numericInput("c2", "Castle 2", value = 0, min = 0, max = troopCommitMax, step = 1),
      numericInput("c3", "Castle 3", value = 0, min = 0, max = troopCommitMax, step = 1),
      numericInput("c4", "Castle 4", value = 0, min = 0, max = troopCommitMax, step = 1),
      numericInput("c5", "Castle 5", value = 0, min = 0, max = troopCommitMax, step = 1),
      
      tags$hr(),
      verbatimTextOutput("sumCheck"),
      actionButton("submit", "Submit allocation", class = "btn-primary")
    ),
    
    mainPanel(
      h4("Record"),
      verbatimTextOutput("recordText"),
      tags$hr(),
      h4("Result"),
      uiOutput("resultText"),
      tags$hr(),
      h4("Allocations"),
      tableOutput("allocTable"),
      tags$hr(),
      h4("Castle outcomes"),
      tableOutput("castleTable")
    )
  ),  
  
  tags$audio(id = "colonelWinSound", src = "colonelwin.mp3", type = "audio/mpeg"),
      tags$script(HTML(" 
      Shiny.addCustomMessageHandler('play_colonel_win', function(msg) {
      var a = document.getElementById('colonelWinSound');
      if (a) { a.currentTime = 0; a.play(); }});")),
    
    tags$audio(id = "colonelLoseSound", src = "colonellose.mp3", type = "audio/mpeg"),
    tags$script(HTML(" 
      Shiny.addCustomMessageHandler('play_colonel_lose', function(msg) {
      var a = document.getElementById('colonelLoseSound');
      if (a) { a.currentTime = 0; a.play(); }});")),
  
  #Debugging
  verbatimTextOutput("tsDebug")
  
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  scoreboard <- reactiveValues(wins = 0, losses = 0, ties = 0, games = 0)
  
  # Beta(1,1) prior for each arm
  post <- reactiveValues(
    a = rep(1, nrow(solutions_5)),
    b = rep(1, nrow(solutions_5))
  )
  
  
  player_vec <- reactive({
    c(input$c1, input$c2, input$c3, input$c4, input$c5)
  })
  
  output$sumCheck <- renderText({
    paste0("Current total troops: ", sum(player_vec()), " / ", troopCommitMax)
  })
  
  outcome <- eventReactive(input$submit, {
    pv <- player_vec()
    
    validate(
      need(all(is.finite(pv)), "All castle inputs must be valid numbers."),
      need(all(pv >= 0), "Troops must be non-negative."),
      need(sum(pv) == troopCommitMax, paste0("Total must equal ", troopCommitMax, "."))
    )
    
    # Map difficulty() to how many arms to sample
    K_ts <- min(nrow(solutions_5), max(200, round(400 * difficulty())))
    
    pick <- pick_colonel_thompson(solutions_5, post$a, post$b, K = K_ts)
    colonel_vec <- pick$colonel_vec
    
    res_vec <- castle_win(pv, colonel_vec)
    scores  <- score_calc_full(res_vec)
    
    # Reward = 1 if Colonel wins this round, else 0 
    reward <- as.integer(scores["Colonel"] > scores["Player 1"])
    
    # Update Beta posterior for the chosen arm
    chosen <- pick$index
    post$a[chosen] <- post$a[chosen] + reward
    post$b[chosen] <- post$b[chosen] + (1 - reward)
    
    
    
    # Update cumulative record
    is_win  <- scores["Player 1"] > scores["Colonel"]
    is_loss <- scores["Colonel"] > scores["Player 1"]
    
    scoreboard$games <- scoreboard$games + 1
    if (is_win)  scoreboard$wins   <- scoreboard$wins + 1
    if (is_loss) scoreboard$losses <- scoreboard$losses + 1
    if (!is_win && !is_loss) scoreboard$ties <- scoreboard$ties + 1
    if (is_loss) {
      session$sendCustomMessage("play_colonel_win", list())
    }
    else if (is_win) {
      session$sendCustomMessage("play_colonel_lose", list())
    } 
    
    
    # Update difficulty based on win rate 
    played <- scoreboard$wins + scoreboard$losses
    if (played > 0) {
      winrate <- scoreboard$wins / played
      if (winrate > 0.5) {
        difficulty(difficulty() + 0.2)
      }
    }
    
    
    list(
      player = pv,
      colonel = colonel_vec,
      result_vec = res_vec,
      scores = scores,
      picked_index = pick$index,
      advantage = pick$advantage
    )
    
  })
  
  
  output$resultText <- renderUI({
    req(outcome())
    oc <- outcome()
    p1 <- oc$scores["Player 1"]
    p2 <- oc$scores["Colonel"]
    
    winner <- if (p1 > p2) "You win."
    else if (p2 > p1) "The Colonel wins."
    else "Tie game."
    
    tagList(
      tags$p(strong(winner)),
      tags$p(paste0("Your score: ", p1, " | Colonel score: ", p2))
    )
  })
  
  output$allocTable <- renderTable({
    req(outcome())
    oc <- outcome()
    tibble(
      Castle = 1:5,
      Player = oc$player,
      Colonel = oc$colonel
    )
  })
  
  output$castleTable <- renderTable({
    req(outcome())
    oc <- outcome()
    tibble(
      Castle = 1:5,
      Outcome = oc$result_vec,
      CastlePoints = 1:5
    )
  })
  
  output$recordText <- renderText({
    paste0(
      "Games: ", scoreboard$games,
      " | Wins: ", scoreboard$wins,
      " | Losses: ", scoreboard$losses,
      " | Ties: ", scoreboard$ties,
      "\nWin rate (excl. ties): ",
      ifelse((scoreboard$wins + scoreboard$losses) == 0,
             "NA",
             round(100 * scoreboard$wins / (scoreboard$wins + scoreboard$losses), 1)),
      "%"
    )
  })
  
  
  #Debug - comment out
  output$tsDebug <- renderText({
    top <- order(post$a / (post$a + post$b), decreasing = TRUE)[1:5]
    paste(
      "Top arms (mean win prob):",
      paste0(
        top, ": ",
        round(post$a[top] / (post$a[top] + post$b[top]), 3),
        collapse = " | "
      )
    )
  })
  
}

shinyApp(ui, server)

