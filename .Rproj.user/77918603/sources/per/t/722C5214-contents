# setwd("path/to/your/project")   # where singleplayer.R lives
source("singleplayer.R")

simulate_ts <- function(solutions_5, rounds = 200) {
  post_a <- rep(1, nrow(solutions_5))
  post_b <- rep(1, nrow(solutions_5))
  
  pv <- c(0, 5, 15, 25, 55)
  
  picks <- integer(rounds)
  rewards <- integer(rounds)
  
  for (t in 1:rounds) {
    pick <- pick_colonel_thompson(
      solutions_5, post_a, post_b,
      K = min(100, nrow(solutions_5))
    )
    colonel_vec <- pick$colonel_vec
    
    res <- castle_win(pv, colonel_vec)
    scores <- score_calc_full(res)
    
    reward <- as.integer(scores["Player 2"] > scores["Player 1"])
    chosen <- pick$index
    
    post_a[chosen] <- post_a[chosen] + reward
    post_b[chosen] <- post_b[chosen] + (1 - reward)
    
    picks[t] <- chosen
    rewards[t] <- reward
  }
  
  posterior_mean <- post_a / (post_a + post_b)
  
  list(
    picks = picks,
    rewards = rewards,
    posterior_mean = posterior_mean,
    top_arms = order(posterior_mean, decreasing = TRUE)[1:10]
  )
}


set.seed(1)
out <- simulate_ts(solutions_5, rounds = 500)

# how often each arm got picked (top 10)
sort(table(out$picks), decreasing = TRUE)[1:10]

# top posterior arms
out$top_arms
out$posterior_mean[out$top_arms]


pv <- c(0, 5, 15, 25, 55)  # fixed player strategy

X <- as.matrix(solutions_5)
wins <- logical(nrow(X))

for (i in 1:nrow(X)) {
  sc <- score_calc_full(castle_win(pv, X[i, ]))
  wins[i] <- sc["Player 2"] > sc["Player 1"]
}

max(mean(wins), max(wins))



pv <- c(0, .5, .15, .25, .55)

X <- as.matrix(solutions_5)
n <- nrow(X)

pwin <- logical(n)
margin <- numeric(n)

for (i in 1:n) {
  sc <- score_calc_full(castle_win(pv, X[i, ]))
  pwin[i] <- sc["Player 2"] > sc["Player 1"]
  margin[i] <- as.numeric(sc["Player 2"] - sc["Player 1"])
}

cat("Fraction of arms that beat pv:", mean(pwin), "\n")
cat("Any arm beats pv?:", any(pwin), "\n")
cat("Best margin:", max(margin), "\n")
cat("Index of best arm:", which.max(margin), "\n")
table(sign(margin))  # -1 loss, 0 tie, +1 win




