# Calculator

Simple `UInt64` calculator.

## Using

[Live version](https://www.markuslaire.com/github/elm-uint64/Calculator)🢅

Enter the expression. Whenever the expression is valid, result is immediately shown in decimal, hexadecimal, octal and binary.

## Syntax

### Literals

- decimal: `123`
- hexadecimal with `0x` prefix: `0xFF`
- octal with `0o` prefix: `0x777`
- binary with `0b` prefix: `0b10101010`

### Operators

- `(`, `)` - parentheses
- `~` - bitwise complement (`UInt64.complement`)
- `^` - exponentiation (`UInt64.pow`)
- `*` - multiplication (`UInt64.mul`)
- `/` - division (`UInt64.div`)
- `%` - modulo (`UInt64.mod`)
- `+` - addition (`UInt64.add`)
- `-` - subtraction (`UInt64.sub`)
- `<<` - bitwise shift left (`UInt64.shiftLeftBy`)
- `>>` - bitwise shift right (`UInt64.shiftRightZfBy`)
- `rol` - bitwise rotate left (`UInt64.rotateLeftBy`)
- `ror` - bitwise rotate right (`UInt64.rotateRightBy`)
- `and` - bitwise and (`UInt64.and`)
- `xor` - bitwise xor (`UInt64.xor`)
- `or` - bitwise or (`UInt64.or`)

### Constants

- `max` - `UInt64.maxValue`
- `maxsafe` - `UInt64.maxSafe`
- `maxfloat` - `UInt64.maxFloat`

## Examples

- `123 + 0xAB - 0b1010`
- `max % (10^10)`
- `0x11223344AABBCCDD and 0x0000FFFFFFFF0000`
- `0x11223344AABBCCDD rol 20`
