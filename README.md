![Scala CI](https://github.com/itu-square/symsim/workflows/Scala%20CI/badge.svg)

## Add a new agent (A new example)

1. `Git clone` the repo or `git pull` to have the fresh version
2. Create a new branch (the repo is configured not to allow to push to main).  Let our example be tic-tac-toe

   ```sh
   git checkout -b tic-tac-toe
   ```
3. Create a new package in `src/main/scala/symsim/examples/concrete/`. The existing one is called `breaking`, let's call the new one `tictactoe`
   
   ```sh
   mkdir -pv src/main/scala/symsim/examples/concrete/tictactoe
   ```
   The package goes under `examples` (quite self-explanatory) and `concrete` for "concrete execution RL".  This might change over time, but we have an initial
   organization
4. Inside the new directory create a file `package.scala` which defines the basic types for state space, action space, discretization (abstraction), etc.
   ```
   cp -iv src/main/scala/symsim/examples/concrete/breaking/package.scala src/main/scala/symsim/examples/concrete/tictactoe/package.scala
   vim/emacs src/main/scala/symsim/examples/concrete/tictactoe/package.scala
   ```
   Adjust the name of the package object from breaking to `tictactoe`. Then change the four types (both names and definitions) to whatever makes sense for TicTacToe. For instances create
   `TicState` - to represent the state of the game
   `TicFiniteState` - this might be just a renaming because the Tic Tac Toe state space is finite
   `TicAction` - possible moves
   
5. Create the file with the implementation of the TicTacToe Agent.  You might want to copy:
   ```
   cp -iv src/main/scala/symsim/examples/concrete/breaking/Car.scala src/main/scala/symsim/examples/concrete/tictactoe/Tic.scala
   vim/emacs src/main/scala/symsim/examples/concrete/package.scala
   ```
   And edit this file from top eliminating the Car example and introducing the TicTacToe example. There are two parts: in the class in the top we give all the logics of the agent, and in the instances/constraints part in the bottom we use the type system to prove that our types have all the necessary properties for the machinery to work.  It might be useful to consult the interface definition (which also has comments at plenty): `src/main/scala/symsim/Agent.scala`.

7. Throughout the process you can commit as normally.  The first time you try to push, observe what git tells you to do, to push to the remote branch. Follow the instruction, and then read the message from git again after the succesful push, to find the link to create a pull request.  Open that link and create a pull request `Adding Tic Tac Toe`.  You can mark it as work in progress (create a 'draft pull request' instead of `pull request`) if you are not done.  After this you can continue pushing as normally from your branch, if you make new commits, and others in the project, will be able to track and discuss your progress easily.

8. To compile your code you can open `sbt` in the root directory (`sbt` is the only tool you have to install, you do not need to install `scala`):

   ```bash
   sbt
   ...>compile
   ```

9. TODO: describe how to run and test it 
