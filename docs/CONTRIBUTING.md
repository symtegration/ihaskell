# How to contribute

## Before you begin

### Review community guidelines

This project follows the [Contributor Covenant Code of Conduct].

[Contributor Covenant Code of Conduct]: CODE_OF_CONDUCT.md

### Review license

Any contributions are to be licensed under the [Apache-2.0 license].
Review the license to determine whether you are willing to license
any contributions under the same license.

[Apache-2.0 license]: ../LICENSE

## Contribution process

### Code reviews

All external contributions require review.
[GitHub pull requests] are used for this purpose.

[GitHub pull requests]: https://docs.github.com/en/pull-requests

### Coding standards

User-visible entities should be documented with [Haddock], including examples if feasible.
[HLint] should report no issues, and formatting should be according to [Ormolu].

All warnings are enabled for builds.
If a certain warning is unavoidable, it should only be disabled on a per file basis.
While the warnings are not errors by default, code with compiler warnings will not
be merged, and the continuous build upgrades these to errors.
To upgrade compiler warnings to errors locally, use the `--pedantic` flag.

```bash
$ stack build --pedantic
$ stack test --pedantic
```

### Releases

When releasing, these files should be updated:

*   [`CHANGELOG.md`] with user-visible changes.

*   [`package.yaml`] with the new version.  There should be at least one
    subsequent `stack build` to update [`ihaskell-symtegration.cabal`] as well.

Versioning is based on [semantic versioning] and the [Haskell package versioning policy].
When there are differences between the two policies, the latter takes precedence.

Lower version bounds for dependencies should be verified by setting the versions
to the lowest minor versions in the Cabal configuration and checking that
builds and tests are still successful.  These changes to the Cabal configuration
are only for confirming that the lower bounds are still valid, and should not
be submitted to the repository.

[`CHANGELOG.md`]: ../CHANGELOG.md
[`package.yaml`]: ../package.yaml
[`ihaskell-symtegration.cabal`]: ../ihaskell-symtegration.cabal
[semantic versioning]: https://semver.org/
[Haskell package versioning policy]: https://pvp.haskell.org/
