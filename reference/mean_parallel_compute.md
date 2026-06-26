# Call an Rcpp function within a doParallel call

Constructs an example showing how to use `foreach`, `iterators`, and
`doParallel` to perform a parallel computation with a C++ function
written using Rcpp.

## Usage

``` r
mean_parallel_compute(
  n,
  mean = 0,
  sd = 1,
  n_sim = 1000,
  n_cores = parallel::detectCores()
)
```

## Arguments

- n:

  Number of Observations

- mean:

  Center of Normal Distribution

- sd:

  Standard Deviation of Normal Distribution

- n_sim:

  Number of Simulations to Run

- n_cores:

  Number of CPU cores to use in parallelization task.

## Value

A `vector` of length `n_sim` containing the mean for each distribution.

## Details

The `mean_parallel_compute()` function performs a bootstrap computation
in parallel of a mean value from the normal distribution.

## Examples

``` r
# Compute the mean on 1000 observations with 50 replications across
# 2 CPUs.
mean_parallel_compute(1000, n_sim = 50, n_cores = 2)
#>                   [,1]
#> result.1  -0.049377668
#> result.2  -0.024685727
#> result.3  -0.028447515
#> result.4   0.007411866
#> result.5  -0.016741424
#> result.6   0.001685332
#> result.7  -0.018131838
#> result.8   0.032931713
#> result.9  -0.071466469
#> result.10 -0.002682792
#> result.11 -0.002345633
#> result.12 -0.009217979
#> result.13 -0.002288086
#> result.14  0.007737320
#> result.15  0.059615696
#> result.16  0.020396757
#> result.17 -0.047423281
#> result.18  0.029150284
#> result.19 -0.012757521
#> result.20 -0.001444628
#> result.21  0.024403440
#> result.22  0.019155758
#> result.23 -0.015964924
#> result.24  0.035099036
#> result.25 -0.005356551
#> result.26  0.030102881
#> result.27  0.056233586
#> result.28 -0.028438526
#> result.29  0.012548952
#> result.30  0.014062601
#> result.31  0.012710110
#> result.32 -0.023017832
#> result.33 -0.014497426
#> result.34 -0.011179594
#> result.35 -0.016803026
#> result.36  0.026393652
#> result.37  0.028693280
#> result.38  0.032712412
#> result.39 -0.011184778
#> result.40 -0.013165387
#> result.41 -0.027785105
#> result.42  0.003788694
#> result.43 -0.006250784
#> result.44 -0.008191873
#> result.45 -0.049230931
#> result.46  0.010568215
#> result.47 -0.011196814
#> result.48  0.031574080
#> result.49 -0.014916197
#> result.50  0.023289528
```
