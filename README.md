# cem-tetris

### How To Run?
1. Build the project
    ```
    stack build
    ```
2. Run the project
    ```
    stack exec cem-tetris-exe
    ```

### Stages

**1.** Implement basics of Tetris
- [X] Game state rendering
- [X] Keyboard controller
- [X] Generation of random blocks

**2.** Auxiliary
- [X] UI:
  - [X] Game Score
  - [X] Show next tetromino
  - [X] Game over menu
- [X] Bot
  - [X] The value function (basis functions as defined in [1])
  - [X] Action selection (as defined in [2])
  - [X] Cross-Entropy Method (as defined in [2])
  
  
### References
[1] Bertsekas and Tsitsiklis, Neuro-Dynamic Programming, page 436, 1996

[2] István Szita and András Lőrincz, Learning Tetris using the noisy cross-entropy method, 2006
