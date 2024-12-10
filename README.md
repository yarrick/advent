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

| Day |   2015 |   2016 |   2017 |   2018 |   2019 |   2020 |   2021 |   2022 |   2023 |   2024 |
|-----|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| 1  |  0.015s | 0.014s | 0.145s | 0.097s | 0.015s | 0.014s | 0.046s | 0.025s | 0.024s | 0.025s |
| 2  |  0.024s | 0.014s | 0.015s | 0.025s | 4.186s | 0.064s | 0.025s | 0.014s | 0.025s | 0.034s |
| 3  |  0.094s | 0.054s | 0.025s | 4.747s | 0.292s | 0.025s | 0.025s | 0.025s | 0.025s | 0.024s |
| 4  |  17.316s | 0.045s | 0.025s | 0.035s | 0.055s | 0.036s | 0.045s | 0.024s | 0.035s | 0.025s |
| 5  |  0.025s | 53.526s | 7.276s | 0.187s | 0.035s | 0.015s | 0.231s | 0.035s | 0.025s | 0.064s |
| 6  |  6.896s | 0.036s | 0.226s | 50.658s | 0.084s | 0.025s | 0.014s | 0.026s | 0.055s | 1.495s |
| 7  |  0.279s | 0.068s | 0.066s | 0.014s | 0.295s | 0.086s | 6.936s | 0.076s | 0.034s | 0.304s |
| 8  |  0.015s | 0.025s | 0.035s | 0.045s | 0.036s | 0.265s | 0.035s | 0.024s | 0.475s | 0.025s |
| 9  |  0.107s | 0.035s | 0.014s | 51.345s | 7.955s | 0.076s | 0.036s | 0.270s | 0.046s | 4.746s |
| 10  |  1.338s | 0.036s | 0.106s | 0.355s | 0.384s | 0.014s | 0.025s | 0.014s | 2.866s | 0.035s |
| 11  |  0.115s | 47.821s | 0.066s | 4.237s | 1.436s | 1.143s | 0.034s | 0.763s | 0.045s |       |
| 12  |  0.025s | 6.824s | 0.186s | 0.044s | 1.186s | 0.015s | 1.125s | 4.896s | 0.756s |       |
| 13  |  0.126s | 0.156s | 0.024s | 0.045s | 22.590s | 0.015s | 0.065s | 0.085s | 0.035s |       |
| 14  |  0.025s | 2m55.9s | 4.365s | 19.235s | 0.115s | 0.751s | 1m13.5s | 6.362s | 1.455s |       |
| 15  |  14.456s | 0.266s | 2.364s | 2m35.8s | 14.098s | 42.711s | 0.127s | 1.705s | 0.070s |       |
| 16  |  0.025s | 7.609s | 1.461s | 0.066s | 3.242s | 0.045s | 0.015s | 36.554s | 1.756s |       |
| 17  |  0.035s | 0.257s | 0.284s | 0.167s | 7.834s | 2.715s | 18.725s | 0.288s | 50.609s |       |
| 18  |  0.673s | 1.056s | 0.175s | 1.355s | 5.757s | 0.045s | 1.445s | 0.106s | 0.035s |       |
| 19  |  0.607s | 1.808s | 0.066s | 0.014s | 21.065s | 1m10.7s | 13.195s | 11m43.1s | 0.037s |       |
| 20  |  13.461s | 0.039s | 0.552s | 9.650s | 1m6.6s | 0.106s | 1.773s | 2m41.2s | 0.225s |       |
| 21  |  0.028s | 0.020s | 5.405s | 6m37.4s | 28.285s | 0.037s | 2m20.7s | 0.047s | 2.301s |       |
| 22  |  0.685s | 0.090s | 19.776s | 1m36.2s | 0.060s | 12.623s | 0.164s | 0.066s | 1.780s |       |
| 23  |  0.029s | 0.039s | 0.071s | 1.091s | 1m44.3s | 1.269s | 25.107s | 8.771s | 1m18.7s |       |
| 24  |  6.200s | 17.017s | 0.953s | 0.230s | 5.515s | 2m20.9s | 5m26.9s | 4.031s | 0.514s |       |
| 25  |  2.663s | 13.169s | 7m28.9s | 0.126s | 3m41.4s | 0.718s | 2.558s | 0.056s | 1m57.6s |       |
