## Death Stacks Bot in Haskell

![image](https://github.com/johntaraj/Death-Stacks-Haskell-Board-game-/assets/134852121/3a89c0be-c3a8-48f5-99fa-c2fc0321d0be)


### Overview
This project involves developing a bot for the game Death Stacks using Haskell. The project is part of a course assignment from the Technische Universität Berlin's Software and Embedded Systems Engineering Group. The assignment includes functional implementation in Haskell, testing, and version control using Git.

### Table of Contents
1. [Introduction](#introduction)
2. [Game Rules](#game-rules)
3. [Implementation Details](#implementation-details)
4. [Testing](#testing)
5. [Project Structure](#project-structure)
6. [How to Run](#how-to-run)
7. [Contributing](#contributing)
8. [License](#license)

### Introduction
Death Stacks is an abstract strategy game where players move and stack pieces until all stacks belong to one player. This project focuses on creating a bot to play this game, utilizing Haskell's functional programming capabilities.

### Game Rules
- The game is played on a 6x6 board.
- Each player has 12 pieces, initially placed in two rows: Red in the top row, Blue in the bottom row.
- Players take turns moving stacks of their pieces.
- Movement can be horizontal, vertical, or diagonal, and the number of spaces moved equals the number of pieces in the stack.
- Stacks can capture opposing stacks by landing on them.
- The game ends when a player cannot make a move.

### Implementation Details
The project is divided into two main modules:
1. **Board Module**: Handles the game board's state and validation.
   - `validateFEN`: Validates a board state string.
   - `buildBoard`: Constructs the board from a valid string.
   - `path`: Generates the path for a move.
   
2. **Deathstacks Module**: Contains the game logic and bot functionality.
   - `playerWon`: Determines if a player has won.
   - `possibleMoves`: Lists all possible moves for a given position.
   - `isValidMove`: Checks if a move is valid.
   - `listMoves`: Lists all possible moves for a player.

### Testing
- Unit tests are implemented using the HSpec framework.
- Test coverage is measured using Haskell Program Coverage (HPC).
- Tests are located in the `test` directory and must be executable via `stack test deathstacks:units`.

### Project Structure
```
├── src
│   ├── Board.hs
│   ├── Deathstacks.hs
├── test
│   ├── Spec.hs
├── stack.yaml
├── README.md
└── .gitlab-ci.yml
```

### How to Run
1. Clone the repository:
   ```
   git clone <repository_url>
   ```
2. Navigate to the project directory:
   ```
   cd deathstacks-haskell
   ```
3. Build the project:
   ```
   stack build
   ```
4. Run the tests:
   ```
   stack test deathstacks:units
   ```
5. Execute the bot:
   ```
   stack exec deathstacks
   ```

### Contributing
- Fork the repository.
- Create a new branch for your feature or bugfix.
- Commit your changes and push to your branch.
- Open a pull request with a detailed description of your changes.

### License
This project is licensed under the MIT License.



### Test Commands:
- Run all tests: stack test
- Run validation tests: stack test deathstacks:validate
- Run unit tests: stack test deathstacks:units
- Run grading tests: stack test deathstacks:grading
- Run tests with coverage: stack test --coverage ...

### Building Commands
- stack build
- stack clean
