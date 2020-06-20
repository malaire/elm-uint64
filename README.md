# elm-uint64

64-bit unsigned integer using wrapping overflow.

```elm
import UInt64

-- `0 - 0xFF` wraps around to `0xFFFFFFFFFFFFFF01`
UInt64.sub UInt64.zero (UInt64.fromInt 0xFF)
    |> UInt64.toHexString
    --> "FFFFFFFFFFFFFF01"

-- ( 0xFFFFFFFFFFFFFFFF / 1e10, 0xFFFFFFFFFFFFFFFF % 1e10 )
UInt64.divMod UInt64.maxValue (UInt64.floor 1e10)
    |> Tuple.mapBoth UInt64.toFloat UInt64.toFloat
    --> ( 1844674407, 3709551615 )

UInt64.fromInt32s 0x11223344 0xAABBCCDD
    |> UInt64.rotateRightBy 20
    |> UInt64.toHexString
    --> "BCCDD11223344AAB"
```

## Design Goals

### **Low-level**

Includes relatively low-level functions.
High-level functions using these low-level functions
would then belong to other packages which depend on [`UInt64`](UInt64#UInt64).

For example `isPrime` wouldn't belong to this package,
but if someone needs add-with-carry or [multiply-accumulate][MA]🢅 , those can be considered.

If you need a function which you think should be included,
please [create an issue](https://github.com/malaire/elm-uint64/issues)🢅 mentioning use case for the function.

[MA]: https://en.wikipedia.org/wiki/Multiply%E2%80%93accumulate_operation

### **Fast**

As fast as feasible, given the chosen implementation of three 24-bit unsigned integers.
This goal is balanced with code complexity.

### **Safe Enough**

#### **Invalid arguments**

Handling of invalid arguments is balanced with goal of speed.
See [Argument handling](UInt64#argument-handling).

#### **Testing**

Every function, no matter how trivial, has at least one full-range fuzz test.

More tests are added as seemed necessary.

## Performance

Here are some quick benchmark results.
As usual I'm not interested in proper benchmarking, so take these with `2^64` grains of salt:

```text
                                      runs / second
pow   : 64-bit number ^ 64-bit number: ~     40 000
pow   : 64-bit number ^ 251          : ~    300 000
divMod: 64-bit number / 40-bit number: ~  2 000 000
pow   : 64-bit number ^ 16           : ~  3 000 000
divMod: 64-bit number / 29-bit number: ~  5 000 000
add, sub, mul                        : > 10 000 000
bitwise shift/rotate by 1 bit        : ~ 20 000 000
```
Benchmarking was done 2020-06-19 with version `1.0.0`, `elm make --optimize`,
Elm 0.19.1, Firefox 68, Debian Linux and Core i5 3570K 3.4 GHz.

Some example benchmarks are included in `benchmarks` directory of source.
