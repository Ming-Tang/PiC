# PiC

Compiles from code from reversible language Pi (Π) to RMov instructions.

The intermediate language between Pi and RMov is IPi, which is a Pi function except some functions are reduced in terms of simpler
functions (`unite2` and `distrib1`) that can be directly expressed in terms of RMov.

```
 Pi --> IPi --> RMov
```

See [Theseus: A High Level Language for Reversible Computing][1] and [The Two Dualities of Computation: Negative and Fractional Types][2]
for more details about the Pi language.

In our syntax, function composition is denoted by `;` or `|>`. In function names, the `+` and `*` suffixes are `S` and `P` in our syntax.

The `i :: 1 <-> 1` function is used to contrain the input type, so that the IPi conversion is possible.

# Running REPL

```
$ runhaskell Main.hs
```

Press tab to autocomplete command or function name.

# Commands

 - `:t <expr>`: Get type of a Pi function
 - `:tp <iexpr>`: Get type of an IPi function
 - `:ce <expr>`: Convert a Pi function to IPi function
 - `:p <expr>`: Confirm the property that the converted IPi has the same type as the original Pi function

  [1]: https://www.cs.indiana.edu/~sabry/papers/theseus.pdf
  [2]: https://www.cs.indiana.edu/~sabry/papers/rational.pdf
  
