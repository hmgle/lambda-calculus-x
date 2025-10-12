# lambda-calculus-x

A playground for exploring lambda-calculus implementations. The repository currently provides a featureful Haskell interpreter with both a reusable library and CLI tools for evaluating lambda terms, experimenting with reduction strategies, and running property-based tests.

## Project Layout

- `haskell/lambda-calculus/` – Haskell implementation
  - `src/Lambda/Calculus/*.hs` – AST, parser, combinator expansions, and evaluation engines
  - `app/Main.hs` – CLI entry point and interactive REPL
  - `test/` – Combined Hspec and QuickCheck suite (`Spec.hs`, `Generators.hs`)

## Prerequisites

Install [Stack](https://docs.haskellstack.org/en/stable/README/) (≥ 2.1 recommended). Optional but helpful tools:

- `rlwrap` for line-editing in the REPL
- `hlint` to enforce style (`stack exec -- hlint src app test`)

All commands below assume the working directory `haskell/lambda-calculus`.

## Build & Test

```bash
stack build             # Compile library, executable, and tests
stack test              # Run Hspec scenario tests and QuickCheck properties
stack test --fast       # Faster iteration build (skips optimisations)
stack test --coverage   # Generate coverage reports when needed
```

## CLI Usage

Evaluate a term directly:

```bash
stack exec lambda-calculus-exe -- "(\\x. x) y"
# => y  -- 1 step
```

Interact with the REPL:

```bash
stack exec lambda-calculus-exe
> S K K z
z  -- 5 steps
> :set strategy applicative
> :quit
```

Key flags: `--strategy normal|applicative` toggles reduction order, `--max-steps N` prevents runaway evaluation, and combinators like `S`, `K`, `Y` are desugared automatically.

## Library Entry Points

```haskell
import Lambda.Calculus

evaluateText "S K K z"
-- Right (EvalResult {stepsUsed = 5, resultExpr = Var "z"})

normalizeWithStrategyLimit ApplicativeOrder 20 expr
```

The `Lambda.Calculus` module re-exports the AST, parser (`parseExpr`), pretty printer, and evaluation helpers to embed the interpreter in other Haskell projects.

## License

Released under the BSD-3-Clause license. See `haskell/lambda-calculus/LICENSE` for details.
