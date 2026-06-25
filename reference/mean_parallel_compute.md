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
#> result.1   0.0123386187
#> result.2   0.0049329560
#> result.3  -0.0341451077
#> result.4  -0.0201411192
#> result.5  -0.0563541363
#> result.6  -0.0551041044
#> result.7  -0.0107631244
#> result.8  -0.0132416010
#> result.9  -0.0263944468
#> result.10 -0.0635325296
#> result.11 -0.0249883816
#> result.12 -0.0216547532
#> result.13 -0.0097207679
#> result.14  0.0297965332
#> result.15 -0.0339187821
#> result.16  0.0035960101
#> result.17  0.0161247585
#> result.18 -0.0186492059
#> result.19  0.0251893787
#> result.20 -0.0054787369
#> result.21 -0.0382085654
#> result.22 -0.0100205918
#> result.23 -0.0318253916
#> result.24 -0.0293945302
#> result.25 -0.0307940830
#> result.26  0.0416149730
#> result.27  0.0293303133
#> result.28 -0.0076254705
#> result.29 -0.0199092889
#> result.30 -0.0362450178
#> result.31  0.0322865992
#> result.32 -0.0028413112
#> result.33  0.0094890592
#> result.34  0.0264870678
#> result.35 -0.0305359287
#> result.36  0.0307940826
#> result.37  0.0010846616
#> result.38 -0.0634106701
#> result.39 -0.0125929284
#> result.40  0.0310210591
#> result.41 -0.0002788326
#> result.42  0.0365372921
#> result.43 -0.0235938773
#> result.44  0.0032748743
#> result.45  0.0087551547
#> result.46  0.0687027708
#> result.47  0.0108358207
#> result.48  0.0393021398
#> result.49  0.0073322009
#> result.50  0.0079927042
```
