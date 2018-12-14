# cem-tetris

Implementation of the Cross-Entropy Method (CEM) for solving Tetris using Haskell Programming Language.

## Results

- Human-controllable Tetris
- Cross-Entropy Method without noise
    - Training is defined as in [2], except the noise part.
    - Mean Points: 25k (20 epochs)

## Usage Scenarios
This section describes how to set up the project for different usage scenarions: human play, bot play, and bot training.

### Bot Play
This is a default mode. It is a real-time simulation of the bot playing Tetris. The weights for the bot are specified in ```src/Tetris/GameLogic.hs``` file, you can replace them with your own weights if you wish.

### Human Play
This mode allows you to play Tetris yourself. To control a tetromino use ```Up Arrow - rotate```, ```Left Arrow - move left```, ```Right Arrow - move right```. To enable this mode, uncomment ```getUserInteraction``` and comment out ```getBotInteraction``` in ```src/Tetris.hs``` file.

### Train the Bot
This mode allows you to train the bot from scratch. In order to do that, go to the ```app/Main.hs``` file and replace ```import Tetris``` with ```import CrossEntropyMethod```.

Also, you can change training hyperparameters.
- Initial mean values can be changed at ```Line 21; src/CrossEntropyMethod.hs```
- Initial standard deviation values can be changed at ``Line 22; src/CrossEntropyMethod.hs``
- Number of simulations per epoch, selection ratio, and number of epochs can be modified at ```Line 129; src/CrossEntropyMethod.hs```

## How to Run?

1. Build the project
    ```
    stack build
    ```
2. Run the project
    ```
    stack exec cem-tetris-exe
    ```

## Development Stages

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
  
  
## References
[1] Bertsekas and Tsitsiklis, Neuro-Dynamic Programming, page 436, 1996

[2] István Szita and András Lőrincz, Learning Tetris using the noisy cross-entropy method, 2006
