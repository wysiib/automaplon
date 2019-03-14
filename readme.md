# Automaplon

Port of the [dk.brics.automaton](https://github.com/cs-au-dk/dk.brics.automaton) library to Prolog

## Todo

Overview of ported classes

* [ ] Automaton
* [ ] AutomatonMatcher
* [X] ~~*AutomatonProvider*~~ [2019-03-14]
  * Note: In the original Java library, this is an interface.
    Hence, there is no code to be ported.
* [ ] BasicAutomata
* [ ] BasicOperations
* [ ] Datatypes
* [ ] DatatypesAutomatonProvider
* [ ] MinimizationOperations
* [ ] RegExp
* [ ] RunAutomaton
* [ ] ShuffleOperations
* [ ] SpecialOperations
* [X] ~~*State*~~ [2019-03-08]
* [ ] StatePair
* [ ] StringUnionOperations
* [X] ~~*Transition*~~ [2019-03-08]
  * Note: Original Java implementation held a Unicode interval and a destination
    state.
    Our implementation realises transitions as maps having the literals as keys
    and destination states as values.
    Thus, there is no explicit port of the Transition class.
