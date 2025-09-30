# lambda-calculus

A featureful, educational lambda-calculus playground implemented in Haskell. The project provides:

- A small core library for representing lambda terms, parsing surface syntax, and normalising expressions using configurable evaluation strategies.
- Command-line tooling for one-off evaluation runs or an interactive REPL with configurable beta-reduction limits and strategies.
- Batteries-included combinator shorthands (e.g., `S`, `K`, `Y`) that are automatically desugared to their lambda encodings.
- An automated test suite combining example-based specifications and property-based QuickCheck checks covering the evaluator and substitution machinery.

## Project Layout

```
├── app/                     # CLI entry point and REPL plumbing
│   └── Main.hs
├── src/
│   ├── Lambda/
│   │   ├── Calculus.hs      # Public library façade
│   │   ├── Calculus/AST.hs  # Core Expr type, pretty printer, utilities
│   │   ├── Calculus/Eval.hs # Normal/applicative reducers with limits
│   │   ├── Calculus/Parser.hs
│   │   └── Calculus/Combinators.hs
│   └── Lib.hs               # Convenience wrappers re-exporting the API
├── test/
│   ├── Spec.hs              # Hspec + QuickCheck suite
│   └── Generators.hs        # Arbitrary expression generators
├── stack.yaml               # Resolver configuration
└── package.yaml             # Project metadata & dependency spec
```

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) ≥ 2.1 (project tested with GHC 8.8.4 via the resolver in `stack.yaml`).
- Optional: `rlwrap` for a nicer REPL experience.

## Building & Running

```bash
stack build           # Compile library, executable, and tests
stack exec lambda-calculus-exe -- --help
```

### One-Off Evaluation

```bash
stack exec lambda-calculus-exe -- "(\\x. x) y"
# => y  -- 1 step

stack exec lambda-calculus-exe -- --strategy applicative "S K K z"
# => z  -- 5 steps

stack exec lambda-calculus-exe -- --max-steps 25 --strategy normal "Y (\\f. \\x. x)"
# => \x. x  -- 8 steps
```

Use `--strategy normal|applicative` to toggle between call-by-name and call-by-value style reduction. `--max-steps N` prevents runaway evaluation; exceeding the limit yields an informative error.

### Interactive REPL

```bash
stack exec lambda-calculus-exe            # start REPL
> S K K z
z  -- 5 steps
> :set steps 50
Max steps set to 50.
> :set strategy applicative
Strategy set to ApplicativeOrder.
> Y (\g. \x. x)
\x. x  -- 8 steps
> :show
Current strategy: ApplicativeOrder, max steps: 50
> :quit
Bye.
```

REPL commands:

- `:let name = expr` – store a reusable definition in the session.
- `:set steps N` – adjust the beta-reduction limit.
- `:set strategy normal|applicative` – switch evaluation strategy.
- `:show` – display current configuration.
- `:quit` – exit the session.

### GHCi / Library Usage

```bash
stack ghci
> import Lambda.Calculus
> evaluateText "S K K z"
Right (EvalResult {stepsUsed = 5, resultExpr = Var "z"})
> evaluateTextWithStrategyLimit ApplicativeOrder 20 "Y (\\f. \\x. f x)"
Right (EvalResult {stepsUsed = 12, resultExpr = Lam "x" (App (Var "f") (Var "x"))})
```

The library exposes:

- `Expr` AST constructors (`Var`, `Lam`, `App`).
- Parsers (`parseExpr`) and pretty printers (`prettyExpr`).
- Strategy-aware normalisation (`normalizeWithStrategyLimit`).
- Friendly wrappers (`evaluateText*`) returning `EvalResult` with step counts.

## Surface Syntax

- Lambda abstractions: `\x. body` or Unicode `λx. body`. Multiple binders can be chained (`\x y. ...`).
- Application: juxtaposition, left-associative (`f x y` == `((f x) y)`).
- Numeric literals translate to Church numerals (`2` ⇢ `λf. λx. f (f x)`).
- Comments and whitespace follow Megaparsec defaults (`--` line comments, `{- -}` block comments).
- Named combinators (`I`, `K`, `S`, `B`, `C`, `W`, `Y`) in free positions are expanded automatically to their lambda encodings. Bound variables shadow these expansions.

## Evaluation Strategies

- **NormalOrder** (default): Outer-most, left-most reduction. Terminates on any weakly normalising term and can ignore diverging arguments.
- **ApplicativeOrder**: Similar to call-by-value; evaluates arguments eagerly. Suitable for exploring strict behaviour and divergence.

Switch strategies via CLI flags, REPL commands, or the library API.

## Session Prelude & `:let` Bindings

- The REPL ships with Church-arithmetic helpers: `add`, `succ`, and `mul`. Combine them with numeric literals (`add 2 3`) for convenience.
- `:let name = expr` stores a definition in the current session; free occurrences of existing bindings or combinators are expanded automatically.
- Definitions are hygienic—lambda-bound variables shadow session names, and cyclic `:let` chains gracefully fall back to the original identifier.

## Combinator Reference

| Name | Definition                        | Behaviour               |
| ---- | --------------------------------- | ----------------------- |
| `I`  | `\x. x`                           | Identity                |
| `K`  | `\x. \y. x`                       | Constant                |
| `S`  | `\x. \y. \z. x z (y z)`           | Distributes application |
| `B`  | `\f. \g. \x. f (g x)`             | Function composition    |
| `C`  | `\f. \x. \y. f y x`               | Argument flip           |
| `W`  | `\f. \x. f x x`                   | Duplication             |
| `Y`  | `\f. (\x. f (x x)) (\x. f (x x))` | Fixed-point combinator  |

Use them directly in surface syntax; the parser desugars them before evaluation.

## Testing

```bash
stack test
```

The suite combines:

- **Hspec examples** testing parsing, substitution, combinator expansion, and evaluation strategies.
- **QuickCheck properties** validating that normalisation produces true normal forms, is idempotent on normal forms, and leaves no free combinator names post-expansion.

Property generators live in `test/Generators.hs` and produce both raw lambda terms and pre-expanded combinator structures, ensuring broad coverage.

## Development Tips

- Run `stack ghci` for a live development loop; modules hot-reload on save.
- `stack clean` clears build artefacts if GHCi or builds behave unexpectedly.
- `stack exec -- hlint src app test` helps keep style consistent (treat warnings as actionable).

## Roadmap Ideas

- Support alternative reduction strategies (e.g., call-by-need) or instrumentation (trace each beta-reduction step).
- Support loading user prelude files (e.g., `:load`) to seed larger definition sets.
- Emit evaluation traces/graphs for visualising reductions.

## License

Released under the BSD-3-Clause license. See [`LICENSE`](LICENSE) for details.
