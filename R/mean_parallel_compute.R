#' Call an Rcpp function within a doParallel call
#'
#' Constructs an example showing how to use `foreach`, `iterators`, and
#' `doParallel` to perform a parallel computation with a C++ function written
#' using Rcpp.
#'
#' @param n       Number of Observations
#' @param mean    Center of Normal Distribution
#' @param sd      Standard Deviation of Normal Distribution
#' @param n_sim   Number of Simulations to Run
#' @param n_cores Number of CPU cores to use in parallelization task.
#'
#' @return
#' A `vector` of length `n_sim` containing the mean for each distribution.
#'
#' @export
#'
#' @importFrom foreach %dopar% foreach
#' @importFrom iterators icount
#' @importFrom doParallel registerDoParallel
#' @importFrom stats rnorm
#'
#' @details
#' The `mean_parallel_compute()` function performs a bootstrap computation in
#' parallel of a mean value from the normal distribution.
#'
#' @examples
#' # Compute the mean on 1000 observations with 50 replications across
#' # 2 CPUs.
#' mean_parallel_compute(1000, n_sim = 50, n_cores = 2)
mean_parallel_compute = function(n, mean = 0, sd = 1,
                                 n_sim = 1000,
                                 n_cores = parallel::detectCores()) {

  # Construct cluster
  cl = parallel::makeCluster(n_cores)

  # After the function is run, close the cluster.
  on.exit(parallel::stopCluster(cl))

  # Register parallel backend
  doParallel::registerDoParallel(cl)


  # Compute estimates
  estimates = foreach::foreach(i = iterators::icount(n_sim), # Perform n simulations
                               .combine = "rbind",           # Combine results
                                                             # Self-load
                               .packages = "Rcpp2doParallel") %dopar% {
    random_data = rnorm(n, mean, sd)

    result = mean_rcpp(random_data) # or use Rcpp2doParallel::mean_rcpp()

    result
  }

  estimates
}
