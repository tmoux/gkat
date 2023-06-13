This is an implementation of the decision procedure from in the paper [Guarded Kleene algebra with tests](https://dl.acm.org/doi/10.1145/3371129).
Guarded Kleene Algebra with Tests (GKAT) is a restriction of KAT that can be used to model simple imperative programs.
We can take advantage of this restriction to represent GKAT programs as automata with a linear number of states, thus admitting an efficient decision procedure for equivalence between GKAT programs.

## Installing

You will need an up-to-date `opam` and `dune`.
Clone the repo and run `dune [build|test]`.
