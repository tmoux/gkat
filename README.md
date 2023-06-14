This is an implementation of the decision procedure from the paper [Guarded Kleene algebra with tests](https://dl.acm.org/doi/10.1145/3371129).
Guarded Kleene Algebra with Tests (GKAT) is a restriction of KAT that can be used to model simple imperative programs.
We can take advantage of this restriction to represent GKAT programs as automata with a linear number of states, thus admitting an efficient decision procedure for equivalence between GKAT programs.

## Installing

You will need an up-to-date `opam` and `dune`.
Clone the repo and run `dune [build|exec|test]`.

The executable `main.exe` (`dune exec main.exe [file]`) takes a filename as an argument, where two GKAT expressions are expected (see the grammar below, or the `examples/` directory).
It will then print out whether the two expressions are equivalent.

### Grammar

Note that actions are represented by strings.

For technical reasons, the primitive tests are represented by the three strings "b", "c", and "d".[^1]

[^1]: Currently, the automaton creates a transition for each possible atom (or truth assignment of primitive tests).
As the number of tests is assumed to be finite, this is still constant in the asymptotic sense, but makes it impractical to work with a large set of primitive tests.

```
b ::= 1 | 0 | "b" | "c" | "d"
    | b && b
    | b || b
    | !b
```

```
e ::= p
    | assert(b)
    | (e; e)
    | if b then e else e
    | while b do e
```
