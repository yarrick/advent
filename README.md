## Solutions in haskell to adventofcode.com puzzles

To build, run `make` followed by the two-digit number for the day.
To get the answer, execute the binary and send the input to stdin.

### Required cabal libraries

* arithmoi
* cryptonite
* matrix
* sequence
* vector

### Benchmarks

Stats from single threaded runs on Ryzen 5 Pro 4650U (from 2020).

| Day |   2015   |   2016   |   2017   |   2018   |   2019   |   2020   |   2021   |   2022   |   2023   |
|-----|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
| 1  |    0.015s |    0.013s |   0.093s | 1m 5.174s |   0.014s |   0.014s |   0.023s |   0.014s |   0.026s |
| 2  |    0.026s |    0.015s |   0.015s |   0.015s |   4.374s |   0.025s |   0.016s |   0.015s |   0.014s |
| 3  |    0.116s |    0.034s |   0.024s |   4.986s |   0.480s |   0.013s |   0.013s |   0.014s |   0.014s |
| 4  |    18.526s |    0.026s |   0.013s |   0.025s |   0.044s |   0.014s |   0.044s |   0.024s |   0.024s |
| 5  |    0.016s |    50.554s | 4m 33.935s |   59.928s |   0.013s |   0.014s |   0.239s |   0.016s |   0.015s |
| 6  |    6.744s |    0.015s |   0.226s |   55.267s |   0.033s |   0.026s |   0.014s |   0.014s |   0.042s |
| 7  |    0.238s |    0.057s |   0.056s |   0.014s |   0.295s |   0.076s |   7.585s |   0.024s |   0.026s |
| 8  |    0.014s |    0.015s |   0.026s |   0.046s |   0.024s |   0.254s |   0.015s |   0.015s |   0.524s |
| 9  |    0.075s |  8m 9.665s |   0.014s |   59.477s |   9.536s |   0.024s |   0.024s |   0.267s |   0.024s |
| 10  |    1.332s |    0.016s |   0.074s |   0.306s |   0.374s |   0.013s |   0.013s |   0.014s |   3.015s |
| 11  |    0.106s |    52.631s |   0.024s |   4.386s |   1.373s |   1.194s |   0.044s |   0.823s |   0.026s |
| 12  |    0.025s |    7.128s |   0.145s |   0.034s |   1.195s |   0.014s |   1.214s |   6.196s |   0.725s |
| 13  |    0.087s |    0.165s | 8m 2.284s |   0.024s |   24.630s |   0.014s |   0.025s |   0.095s |   0.013s |
| 14  |    0.016s |  2m 49.504s |   4.546s |   21.111s |   0.093s | 6m 50.131s | 1m 23.754s |   6.107s |   1.524s |
| 15  |    14.487s |    0.244s |   2.583s | 2m 55.315s |   15.408s |   45.335s |   0.109s |   1.756s |   0.047s |
| 16  |    0.015s |    7.703s |   1.483s |   0.046s |   3.410s |   0.025s |   0.014s |   39.152s |   1.746s |
| 17  |    0.026s |    0.250s |   0.274s | 3m 49.644s |   8.349s |   2.315s |   16.904s |   0.247s |   49.644s |
| 18  |    0.502s |    1.045s |   0.177s |   1.289s |   5.639s |   0.025s |   1.474s |   0.095s |   0.015s |
| 19  |    0.557s |    1.835s |   0.025s |   0.015s |   22.586s | 1m 14.758s |   14.096s | 92m 18.285s |   0.025s |
| 20  |    12.546s |    0.026s |   0.544s |   9.158s | 1m 13.057s |   0.054s |   1.596s | 2m 48.071s |   0.176s |
| 21  |    0.015s |    0.015s |   5.358s | 6m 3.716s |   29.306s |   0.015s | 2m 13.765s |   0.035s |       |
| 22  |    0.648s |    0.056s |   20.048s | 1m 32.670s |   0.015s |   9.456s |   0.115s |   0.035s |   1.975s |
| 23  |    0.015s |    0.025s |   0.045s |   1.075s | 1m 46.898s |   1.309s |   28.076s |   7.621s | 1m 12.966s |
| 24  |    6.275s |    16.758s |   0.942s |   0.205s |   5.670s | 1m 16.065s | 5m 24.479s |   3.984s |       |
| 25  |    2.675s |    13.356s | 7m 35.147s |   0.077s | Interactive |   0.615s |   2.676s |   0.015s | 2m 4.796s |
