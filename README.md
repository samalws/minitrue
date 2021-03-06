# Minitrue
Minitrue is a minimalist [calculus of constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions) typechecker (i.e., theorem prover) written in Haskell.
It contains the bare minimum syntax necessary to prove everything that can be proven in mathematics.
Its name comes both from the fact that it is a minimalist (mini) proof checker (true), and also from the shortened name of the
[Ministry of Truth](https://en.wikipedia.org/wiki/Ministries_of_Nineteen_Eighty-Four#Ministry_of_Truth),
the ministry in *1984* tasked with deciding what is true and what isn't.
Examples can be found in `examples.minitrue`.
To compile the typechecker, run the command `ghc minitrue.hs`.
To run it, type `./minitrue example.minitrue`, replacing `example.minitrue` with the file you want to typecheck.
Note that minitrue doesn't work on Windows due to a lack of support for the `λ` and `∀` symbols.
