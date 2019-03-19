# Automaplon

Port of the [dk.brics.automaton](https://github.com/cs-au-dk/dk.brics.automaton) library to Prolog

## Todo

Overview of ported classes

* [X] Automaton [2019-03-17]
  * Note: The original Java implementation held a Unicode interval and a destination for transitions which could be 
    reduced by combining overlapping intervals of adjacent states. Since we unfold ranges (see ~~*Transition*~~), 
    Automaton#reduce() is not ported.
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
* [X] ~~*StatePair*~~ [2019-03-14]
  * Note: Can be expressed as tuple or two-element list or pair/2 term.
    Does not need porting.
* [ ] StringUnionOperations
* [X] ~~*Transition*~~ [2019-03-08]
  * Note: The original Java implementation held a Unicode interval and a destination
    state.
    Our implementation realises transitions as maps having the literals as keys
    and destination states as values.
    Thus, there is no explicit port of the Transition class.
