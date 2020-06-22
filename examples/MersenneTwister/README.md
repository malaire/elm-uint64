# Mersenne Twister

[Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)🢅
pseudorandom number generator implemented using `UInt64`.

## Using

To run tests:
```text
$ cd examples/MersenneTwister
$ elm-test
```

To run benchmarks:
```text
$ cd examples/MersenneTwister/benchmarks
$ elm make --optimize Benchmarks.elm
```
Then open generated `index.html` in browser.

## Testing

**This code has not been tested rigorously.**

One test is included which compares this implementation
against testdata generated using original C code.
Testdata mirrors the example output provided with the original C source.

This test passes, but one test is not enough to know for sure that there are no bugs.

## Performance

```text
                        runs / second
initByArray                       800
uint64 (with twist)             2 000
init                            8 000
uint64 (without twist)      1 400 000
```
Benchmarking was done 2020-06-22 with `elm make --optimize`,
Elm 0.19.1, Firefox 68, Debian Linux and Core i5 3570K 3.4 GHz.

Code used is included in `benchmarks` directory.

## Original C source

Files in directory `mt19937-64` are unmodified files downloaded from
http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html as `mt19937-64stdint.tgz` on 2020-06-22.

## TestData

File `TestData.elm` has been generated with `generate-test-data.c`.

```text
$ gcc generate-test-data.c mt19937-64/mt19937-64.c -o generate-test-data
$ ./generate-test-data > tests/TestData.elm
```
