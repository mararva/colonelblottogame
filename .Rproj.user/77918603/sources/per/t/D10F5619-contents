library(tidyverse)

castle_solutions <- read.csv("riddler-castles-data/castle-solutions-5.csv")

# Identify the 10 castle columns (adjust if needed)
castle_cols <- castle_solutions %>%
  select(where(is.numeric)) %>%
  select(1:10)

sol1 <- read.csv("riddler-castles-data-5p/castle-solutions.csv")
sol1 <- sol1[1:5]
sol1_long <- gather(sol1, key = "Castle", value = "Value")
sol2 <- read.csv("riddler-castles-data-5p/castle-solutions-2.csv")
sol2 <- sol2[1:5]
sol2_long <- gather(sol2, key = "Castle", value = "Value")
sol3 <- read.csv("riddler-castles-data-5p/castle-solutions-3.csv")
sol3 <- sol3[1:5]
sol3_long <- gather(sol3, key = "Castle", value = "Value")
sol4 <- read.csv("riddler-castles-data-5p/castle-solutions-4.csv")
sol4_long <- gather(sol4, key = "Castle", value = "Value")
temp_solA <- rbind(sol1, sol2)
temp_solB <- rbind(sol3, sol4)
sol5 <- rbind(temp_solA, temp_solB)




# Bin castles pairwise
binned_castles <- castle_cols %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "castle",
    values_to = "score"
  ) %>%
  mutate(
    castle_num = as.integer(str_extract(castle, "\\d+")),
    bin = ceiling(castle_num / 2)
  ) %>%
  group_by(row_id, bin) %>%
  summarise(score = sum(score), .groups = "drop") %>%
  pivot_wider(
    names_from = bin,
    values_from = score,
    names_prefix = "castle_"
  ) %>%
  select(-row_id)

write.csv(
  sol5,
  "riddler-castles-data-5p/castle-solutions-5.csv",
  row.names = FALSE
)
