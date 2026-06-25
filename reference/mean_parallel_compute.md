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
#> result.1   0.0574489614
#> result.2  -0.0085457141
#> result.3  -0.0124399948
#> result.4  -0.0186350378
#> result.5  -0.0219970855
#> result.6  -0.0327342183
#> result.7   0.0221617574
#> result.8   0.0538504919
#> result.9   0.0208757394
#> result.10  0.0387122891
#> result.11  0.0007381406
#> result.12  0.0162826556
#> result.13 -0.0051351726
#> result.14 -0.0777783633
#> result.15 -0.0103536998
#> result.16 -0.0244270718
#> result.17  0.0166567582
#> result.18  0.0108895745
#> result.19  0.0506462234
#> result.20  0.0291277098
#> result.21  0.0253467826
#> result.22 -0.0167628204
#> result.23  0.0245242326
#> result.24  0.0131598673
#> result.25 -0.0285811496
#> result.26  0.0327155884
#> result.27  0.0268645655
#> result.28  0.0068999275
#> result.29  0.0067645757
#> result.30 -0.0239418263
#> result.31  0.0331205698
#> result.32  0.0185114476
#> result.33 -0.0352440821
#> result.34 -0.0473383560
#> result.35  0.0204550439
#> result.36  0.0341769573
#> result.37 -0.0206942218
#> result.38 -0.0177663513
#> result.39  0.0122684514
#> result.40  0.0043092879
#> result.41  0.0042242430
#> result.42 -0.0545322893
#> result.43 -0.0203906459
#> result.44 -0.0208091167
#> result.45 -0.0251555905
#> result.46 -0.0128145858
#> result.47  0.0215102076
#> result.48  0.0281530510
#> result.49  0.0429583389
#> result.50 -0.0111076655
```
