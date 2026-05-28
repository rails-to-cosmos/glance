# Glance — Test Suite Review

## Current state

- **TestLexer** (6 tests, active): tokenization of plain text — spaces, multiple tokens. Solid for its scope.
- **TestParser** (11 test cases, commented out): headlines, tags, properties, pragmas, timestamps, multiline. This is the bulk of coverage and it's not running.

## Gaps identified

### Structural problems

1. **TestParser is disabled.** `Spec.hs` comments out `Parser.spec`. Everything below the lexer is untested in CI.

2. **No roundtrip tests.** `Parse` (text→AST) and `TextShow` (AST→text) should satisfy `showt (parse input) == input` for well-formed inputs. Highest-value test category for a parser.

3. **No negative tests.** Every test expects success. Malformed input should produce errors or correct fallback behavior.

4. **Context threading barely tested.** IAS registration, context accumulation across multiple `orgParse` calls, and `resolveHeadline` are untested.

5. **Timestamp coverage is thin.** Only `<2024-01-01>` and `<2024-01-01 Mon>`. Missing: inactive timestamps, time-of-day, repeaters, timestamps inside titles.

6. **No TextShow tests.** Serialization format is untested.

7. **No Display tests.** Human-readable output is untested.

### Best practices missing

- **Property-based testing** with `tasty-quickcheck` and `Arbitrary` instances.
- **Test organization** mirroring module structure, with unit tests for individual `Parse` instances.
- **Negative/error tests** to prevent silent mis-parses.

## Implementation plan

1. Enable TestParser
2. Add roundtrip tests (parse . showt == id)
3. Add negative/error tests
4. Add timestamp variant tests
5. Add IAS/context-threading tests
6. Add property-based tests

## Priority

Roundtrip tests catch the most bugs per line of test code. Enabling TestParser is zero-effort maximum-gain. Property-based tests are the long-term correctness net.
