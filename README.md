# The Colonel Blotto Game

## Overview
This project is a single-player strategy game inspired by the **Colonel Blotto** game.  
A human player (“Player”) allocates a fixed number of troops across multiple castles, while an AI opponent (“Colonel”) chooses its own allocation in response. Each round is scored castle-by-castle, and the side that wins the most points wins the round.

The game is implemented in **R using Shiny**.

## Core Gameplay
- There are **5 castles**.
- Both Player and Colonel must allocate **all 100 troops** across the castles.
- Each castle is won by the side allocating more troops to that castle.
- The round outcome is:
  - **Player win** (Player wins more castles)
  - **Colonel win**
  - **Tie** (neither win)


