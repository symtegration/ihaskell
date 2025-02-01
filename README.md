# Symtegration in IHaskell

IHaskell extension for making the use of [Symtegration] more seamless with [IHaskell].

[Symtegration]: https://symtegration.dev/

[IHaskell]: https://github.com/IHaskell/IHaskell

For example, the integral from the following will be rendered as a mathematical expression.

![Example of integral rendered by IHaskell](docs/integration-example.png)

[![Build](https://github.com/symtegration/ihaskell/actions/workflows/build.yaml/badge.svg)](https://github.com/symtegration/ihaskell/actions/workflows/build.yaml)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/symtegration/ihaskell/badge)](https://scorecard.dev/viewer/?uri=github.com/symtegration/ihaskell)

## Installation

To use with a local IHaskell installation, include extra dependencies in `stack.yaml`.
For example,

```yaml
extra-deps:
- symtegration-0.6.1
- ihaskell-symtegration-0.1.0
```

Once the extra dependencies are built and installed,
they should be available for use within IHaskell.

```bash
$ stack install symtegration ihaskell-symtegration
```

## Changes

See [`CHANGELOG.md`] for what has changed.

[`CHANGELOG.md`]: CHANGELOG.md

## Code of conduct

Be nice; see [`CODE_OF_CONDUCT.md`] for details.

[`CODE_OF_CONDUCT.md`]: docs/CODE_OF_CONDUCT.md

## Security policy

See [`SECURITY.md`] for details.

[`SECURITY.md`]: docs/SECURITY.md

## Contributing

See [`CONTRIBUTING.md`] for details.

[`CONTRIBUTING.md`]: docs/CONTRIBUTING.md

## License

Apache 2.0; see [`LICENSE`] for details.

[`LICENSE`]: LICENSE
