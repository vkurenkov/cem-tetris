# genetic-tetris

### How To Run?
1. Build the project
    ```
    stack build
    ```
2. Run the project
    ```
    stack exec genetic-tetris-exe
    ```

### Stages:

**1.** Implement basics of Tetris
- [X] Game state rendering
- [X] Keyboard controller
- [X] Generation of random blocks

**2.** Auxiliary
- [ ] UI:
  - [X] Game Score
  - [ ] Main menu (you play / bot plays)
  - [ ] Credits
  - [ ] Show next tetromino
  - [ ] Game over menu
- [ ] Bot (based on Noisy Cross-Entropy Method)
  - [ ] The value function (basis functions as defined in Bertsekas and Tsitsiklis, 1996)
  - [ ] Action selection (as defined Bertsekas and Tsitsiklis, 1996)
  - [ ] Noisy Cross-Entropy Method
