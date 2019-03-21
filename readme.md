# Automaplon

Port of the [dk.brics.automaton](https://github.com/cs-au-dk/dk.brics.automaton) library to Prolog

## Todo

Overview of ported classes

* [X] Automaton [2019-03-17]
  * Note: The original Java implementation held a Unicode interval and a
    destination for transitions which could be reduced by combining overlapping
    intervals of adjacent states. Since we unfold ranges (see ~~*Transition*~~),
    Automaton#reduce() is not ported.
* [ ] AutomatonMatcher
* [X] ~~*AutomatonProvider*~~ [2019-03-14]
  * Note: In the original Java library, this is an interface.
    Hence, there is no code to be ported.
* [ ] BasicAutomata
* [ ] BasicOperations
  * [ ] concatenate(Automaton a1, Automaton a2)
  * [ ] concatenate(List<Automaton> l)
  * [ ] optional(Automaton a)
  * [ ] repeat(Automaton a)
  * [ ] repeat(Automaton a, int min)
  * [ ] repeat(Automaton a, int min, int max)
  * [ ] complement(Automaton a)
  * [ ] minus(Automaton a1, Automaton a2)
  * [ ] intersection(Automaton a1, Automaton a2)
  * [ ] subsetOf(Automaton a1, Automaton a2)
  * [ ] union(Automaton a1, Automaton a2)
  * [ ] union(Collection<Automaton> l)
  * [ ] determinize(Automaton a)
  * [ ] determinize(Automaton a, Set<State> initialset)
  * [ ] addEpsilons(Automaton a, Collection<StatePair> pairs)
  * [ ] isEmptyString(Automaton a)
  * [ ] isEmpty(Automaton a)
  * [ ] isTotal(Automaton a)
  * [ ] getShortestExample(Automaton a, boolean accepted)
  * [ ] getShortestExample(State s, boolean accepted)
  * [ ] run(Automaton a, String s)
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
