# Haskell Implementation of Norvig's [Spell Checker](http://norvig.com/spell-correct.html)

Implemented the simple spell checking algorithm presented on Peter Norvig's
website, accompanied by an excellent step-by-step
[explanation](http://norvig.com/spell-correct.html).

## Running the code
 1. Install Haskell tools (I used the Haskell Platform)
 2. Download text and test files from Norvig's page
  - [`big.txt`](http://norvig.com/big.txt)
  - [`spell-testset1.txt`](http://norvig.com/spell-testset1.txt)
  - [`spell-testset2.txt`](http://norvig.com/spell-testset2.txt)
 3. Compile the code `ghc Main.hs`
 4. Run!
  - `./Main interactive big.txt` for interactive mode
  - `./Main test big.txt spell-testset1.txt` run testset1

