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
#>                    [,1]
#> result.1   0.0150045919
#> result.2  -0.0171252609
#> result.3   0.0131322171
#> result.4   0.0284949978
#> result.5  -0.0072779336
#> result.6  -0.0456233404
#> result.7  -0.0114650138
#> result.8  -0.0309747446
#> result.9  -0.0163521463
#> result.10  0.0371556658
#> result.11 -0.0138538836
#> result.12 -0.0094418344
#> result.13  0.0066034032
#> result.14  0.0439363013
#> result.15 -0.0437081775
#> result.16 -0.0250913663
#> result.17 -0.0258391376
#> result.18 -0.0117568199
#> result.19 -0.0508091776
#> result.20 -0.0243636797
#> result.21  0.0426642179
#> result.22  0.0138032727
#> result.23 -0.0726409953
#> result.24 -0.0066152770
#> result.25 -0.0159812499
#> result.26  0.0133501588
#> result.27 -0.0009888053
#> result.28  0.0181051650
#> result.29 -0.0429865858
#> result.30 -0.0187104521
#> result.31 -0.0069762687
#> result.32  0.0169555946
#> result.33  0.0184515324
#> result.34  0.0406557821
#> result.35 -0.0056584112
#> result.36 -0.0250035907
#> result.37  0.0118896699
#> result.38  0.0308388036
#> result.39 -0.0500019369
#> result.40 -0.0189606556
#> result.41 -0.0184563972
#> result.42 -0.0233212471
#> result.43 -0.0522513100
#> result.44 -0.0339195849
#> result.45 -0.0291680945
#> result.46 -0.0308662648
#> result.47 -0.0162136534
#> result.48  0.0134435547
#> result.49  0.0119273715
#> result.50 -0.0340562885
```
