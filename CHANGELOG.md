# Changelog

## [Unreleased]
### Added
- `toDigits`, `toIntDigits` and `UInt64.Digits` for converting `UInt64` to `String`/`List`
  via `Digits`, with options like different bases, digits padding and digits grouping.

### Changed
- `limitSmallInt` takes additional `Bool` parameter
  and can now limit also to range `1 <= x <= 2 ^ bitSize`.

## [1.1.0] - 2020-06-27
### Added
- `fromString` - convert decimal/hex/octal/binary `String` to `UInt64`
- `div`, `mod`
- `isEven`, `isOdd`, `isZero`, `shiftRightZfBy1`, `square`
- Mersenne Twister example

### Optimized
- Many internal optimizations, which have increased speed of nearly all functions.
- Speed of `pow` with large exponents has increased 400% in Firefox.

## 1.0.0 - 2020-06-20
### Added
- Everything. First public version.

[Unreleased]: https://github.com/malaire/elm-uint64/compare/1.1.0...HEAD
[1.1.0]: https://github.com/malaire/elm-uint64/compare/1.0.0...1.1.0
