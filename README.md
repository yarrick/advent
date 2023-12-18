## Solutions in haskell to adventofcode.com puzzles

Solutions with a main method expect the input file to be piped to it,
otherwise the input should be given to the suitable function inside ghci.

### Required cabal libraries

* arithmoi
* cryptonite
* matrix
* sequence
* vector

### Benchmarks

Stats from single threaded runs on Ryzen 5 Pro 4650U (from 2020).

| Day |   2018   |   2019   |   2020   |   2021   |   2022   |   2023   |
|-----|----------|----------|----------|----------|----------|----------|
| 1  | 1m2.288s | 0m0.015s | 0m0.015s | 0m0.024s | 0m0.015s | 0m0.014s |
| 2  | 0m0.016s | 0m4.366s | 0m0.026s | 0m0.015s | 0m0.015s | 0m0.014s |
| 3  | 0m4.779s | 0m0.421s | 0m0.014s | 0m0.026s | 0m0.014s | 0m0.015s |
| 4  | 0m0.024s | 0m0.046s | 0m0.015s | 0m0.047s | 0m0.014s | 0m0.025s |
| 5  | 0m55.668s | 0m0.014s | 0m0.014s | 0m0.230s | 0m0.014s | 0m0.015s |
| 6  | 0m51.604s | 0m0.035s | 0m0.026s | 0m0.015s | 0m0.014s | 0m0.054s |
| 7  | 0m0.015s | 0m0.286s | 0m0.067s | 0m7.886s | 0m0.025s | 0m0.024s |
| 8  | 0m0.047s | 0m0.014s | 0m0.246s | 0m0.015s | 0m0.015s | 0m0.476s |
| 9  | 0m52.683s | 0m9.795s | 0m0.026s | 0m0.025s | 0m0.260s | 0m0.024s |
| 10  | 0m0.285s | 0m0.376s | 0m0.015s | 0m0.015s | 0m0.014s | 0m2.858s |
| 11  | 0m4.206s | 0m1.335s | 0m1.115s | 0m0.036s | 0m0.794s | 0m0.026s |
| 12  | 0m0.036s | 0m1.265s | 0m0.014s | 0m1.195s | 0m5.136s | 0m0.717s |
| 13  | 0m0.025s | 0m23.348s | 0m0.016s | 0m0.024s | 0m0.086s | 0m0.016s |
| 14  | 0m19.908s | 0m0.105s | 6m26.231s | 1m15.306s | 0m5.489s | 0m1.459s |
| 15  | 2m38.826s | 0m15.066s | 0m45.265s | 1m22.825s | 0m1.756s | 0m0.048s |
| 16  | 0m0.046s | 0m3.360s | 0m0.024s | 0m0.014s | 0m38.534s | 0m1.757s |
| 17  | 3m48.727s | 0m8.653s | 0m2.326s | 0m16.926s | 0m0.239s | 0m49.744s |
| 18  | 0m1.260s | 0m5.589s | 0m0.026s | 0m1.475s | 0m0.096s | 0m0.014s |
| 19  | 0m0.014s | 0m22.135s | 1m14.275s | 0m13.706s | 92m0.525s |       |
| 20  | 0m9.058s | 1m12.927s | 0m0.057s | 0m1.618s | 2m48.227s |       |
| 21  | 6m1.167s | 0m29.006s | 0m0.026s | 2m15.927s | 0m0.077s |       |
| 22  | 1m32.276s | 0m0.015s | 0m9.457s | 0m0.147s | 0m0.044s |       |
| 23  | 0m1.076s | 1m47.046s | 0m1.308s | 0m26.565s | 0m7.710s |       |
| 24  | 0m0.204s | 0m5.709s | 1m15.925s | 5m24.491s | 0m3.965s |       |
| 25  | 0m0.076s | Interactive | 0m0.601s | 0m2.735s | 0m0.015s |       |

