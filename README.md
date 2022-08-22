![Scala CI](https://github.com/itu-square/symsim/workflows/Scala%20CI/badge.svg)

SQUARE Symsim is a test-bed for implementing reinforcement learning
algorithms, formalizing their correctness properties, and testing
them.  It is implemented in Scala 3, in purely functional style, and
uses property-based testing.

## Installation & Requirements

There is no installation or release yet.  See below in _Adding a new
agent_ how to clone, branch, and run the code.

The implementation is quite memory hungry right now, so we recommend
the following sbt setup __if__ you run out of memory:

   ```sh
   export SBT_OPTS="-Xmx3G -XX:+UseG1GC -Xss2M"
   ```

Place this in your `.bashrc` or execute in the current shell, just
before starting `sbt`.

## Scope

So far discrete (exact) Q-Learning and SARSA are implemented, along
with a bunch of simple examples.

## Adding a new agent (A new example)

1. `Git clone` the repo or `git pull` to have the fresh version
2. Create a new branch (the repo is configured not to allow to push to main).  Let our example be tic-tac-toe

   ```sh
   git checkout -b tic-tac-toe
   ```
3. Create a new package in `src/main/scala/symsim/examples/concrete/`. The existing one is called `breaking`, let's call the new one `tictactoe`

   ```sh
   mkdir -pv src/main/scala/symsim/examples/concrete/tictactoe
   ```
   The package goes under `examples` and `concrete` for "concrete execution RL".

4. Inside the new directory create a file `TicTacToe.scala`.
   ```
   cp -iv src/main/scala/symsim/examples/concrete/breaking/Car.scala src/main/scala/symsim/examples/concrete/tictactoe/TicTacToe.scala
   edit src/main/scala/symsim/examples/concrete/tictactoe/TicTacToe.scala
   ```
   Adjust the name of the package object from breaking to `tictactoe`. Then change the four types (both names and definitions) to whatever makes sense for TicTacToe. For instances create
   `TicState` - to represent the state of the game
   `TicObservableState` - this might be just a renaming because the Tic Tac Toe state space is finite
   `TicAction` - possible moves

5. Implement the TicTacToe agent.

   Edit this file from top eliminating the Car example and introducing the TicTacToe example. There are two parts: in the class in the top we give all the logics of the agent, and in the instances/constraints part in the bottom we use the type system to prove that our types have all the necessary properties for the machinery to work.  It might be useful to consult the interface definition (which also has comments at plenty): `src/main/scala/symsim/Agent.scala`.

7. Working with git and PRs.

   Throughout the process you can commit as normally.  The first time you try to push, observe what git tells you to do, to push to the remote branch. Follow the instruction, and then read the message from git again after the succesful push, to find the link to create a pull request.  Open that link and create a pull request `Adding Tic Tac Toe`.  You can mark it as work in progress (create a 'draft pull request' instead of `pull request`) if you are not done.  After this you can continue pushing as normally from your branch, if you make new commits, and others in the project, will be able to track and discuss your progress easily.

8. Compiling

   To compile your code you can open `sbt` in the root directory (`sbt` is the only tool you have to install, you do not need to install `scala`):

   ```bash
   sbt
   ...>compile
   ```

9. Running the learning

   There is a corresponding test tree (to the `main` source tree).  Under `concrete/examples/breaking/` you will find the file `Experiments.scala` that shows how the breaking car learning is executed.  So far, we disguise it as a test.  You can copy this file to the corresponding directory for `tictactoe` and adjust it to instantiate the tic-tac-toe learning.

## Credits

Symsim is developed at the [SQUARE](https://square.itu.dk) group at [IT
University of Copenhagen](https://www.itu.dk), and at the [SIRIUS](https://sirius-labs.no/) Centre of [University of Oslo](https://www.uio.no).  The work is financially supported by the Danish [DIREC](https://direc.dk) initiative, under a bridge project [Verifiable and Safe AI for Autonomous Systems](https://direc.dk/verifiable-and-safe-ai-for-autonomous-systems/).
