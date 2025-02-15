#' Parametric Bootstrap for the
#' State Space Model
#' (Fixed Parameters)
#'
#' This function simulates data from
#' a state-space model
#' and fits the model using the `dynr` package.
#' The process is repeated `R` times.
#' It assumes that the parameters remain constant
#' across individuals and over time.
#' At the moment, the function only supports
#' `type = 0`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams simStateSpace::SimSSMFixed
#' @inheritParams dynr::dynr.cook
#' @inherit simStateSpace::SimSSMFixed details
#' @param R Positive integer.
#'   Number of bootstrap samples.
#' @param path Path to a directory
#'   to store bootstrap samples and estimates.
#' @param prefix Character string.
#'   Prefix used for the file names
#'   for the bootstrap samples and estimates.
#' @param mu0_fixed Logical.
#'   If `mu0_fixed = TRUE`,
#'   fix the initial mean vector
#'   to `mu0`.
#'   If `mu0_fixed = FALSE`,
#'   `mu0` is estimated.
#' @param sigma0_fixed Logical.
#'   If `sigma0_fixed = TRUE`,
#'   fix the initial covariance matrix
#'   to `tcrossprod(sigma0_l)`.
#'   If `sigma0_fixed = FALSE`,
#'   `sigma0` is estimated.
#' @param alpha_level Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param ncores Positive integer.
#'   Number of cores to use.
#'   If `ncores = NULL`,
#'   use a single core.
#'   Consider using multiple cores
#'   when number of bootstrap samples `R`
#'   is a large value.
#' @param seed Random seed.
#' @param clean Logical.
#'   If `clean = TRUE`,
#'   delete intermediate files generated by the function.
#' @param xtol_rel Stopping criteria option
#'   for parameter optimization.
#'   See [dynr::dynr.model()] for more details.
#' @param stopval Stopping criteria option
#'   for parameter optimization.
#'   See [dynr::dynr.model()] for more details.
#' @param ftol_rel Stopping criteria option
#'   for parameter optimization.
#'   See [dynr::dynr.model()] for more details.
#' @param ftol_abs Stopping criteria option
#'   for parameter optimization.
#'   See [dynr::dynr.model()] for more details.
#' @param maxeval Stopping criteria option
#'   for parameter optimization.
#'   See [dynr::dynr.model()] for more details.
#' @param maxtime Stopping criteria option
#'   for parameter optimization.
#'   See [dynr::dynr.model()] for more details.
#'
#' @references
#'   Chow, S.-M., Ho, M. R., Hamaker, E. L., & Dolan, C. V. (2010).
#'   Equivalence and differences between structural equation modeling
#'   and state-space modeling techniques.
#'   *Structural Equation Modeling: A Multidisciplinary Journal*,
#'   17(2), 303–332.
#'   \doi{10.1080/10705511003661553}
#'
#' @return Returns an object
#'   of class `bootstatespace` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{thetahatstar}{Sampling distribution of
#'       \eqn{\boldsymbol{\hat{\theta}}}.}
#'     \item{vcov}{Sampling variance-covariance matrix of
#'       \eqn{\boldsymbol{\hat{\theta}}}.}
#'     \item{est}{Vector of estimated
#'       \eqn{\boldsymbol{\hat{\theta}}}.}
#'     \item{fun}{Function used ("PBSSMFixed").}
#'     \item{method}{Bootstrap method used ("parametric").}
#'   }
#'
#' @examples
#' \donttest{
#' # prepare parameters
#' set.seed(42)
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' delta_t <- 1
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#' ## measurement model
#' k <- 3
#' nu <- rep(x = 0, times = k)
#' lambda <- diag(k)
#' theta <- 0.001 * diag(k)
#' theta_l <- t(chol(theta))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "ssm",
#'   n = n,
#'   time = time,
#'   delta_t = delta_t,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   nu = nu,
#'   lambda = lambda,
#'   theta_l = theta_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' print(pb)
#' summary(pb)
#' confint(pb)
#' vcov(pb)
#' coef(pb)
#' print(pb, type = "bc") # bias-corrected
#' summary(pb, type = "bc")
#' confint(pb, type = "bc")
#' }
#'
#' @family Bootstrap for State Space Models Functions
#' @keywords bootStateSpace boot pb ssm
#' @export
PBSSMFixed <- function(R,
                       path,
                       prefix,
                       n, time, delta_t = 1,
                       mu0, sigma0_l,
                       alpha, beta, psi_l,
                       nu, lambda, theta_l,
                       type = 0,
                       x = NULL, gamma = NULL, kappa = NULL,
                       mu0_fixed = FALSE,
                       sigma0_fixed = FALSE,
                       alpha_level = 0.05,
                       optimization_flag = TRUE,
                       hessian_flag = FALSE,
                       verbose = FALSE,
                       weight_flag = FALSE,
                       debug_flag = FALSE,
                       perturb_flag = FALSE,
                       xtol_rel = 1e-7,
                       stopval = -9999,
                       ftol_rel = -1,
                       ftol_abs = -1,
                       maxeval = as.integer(-1),
                       maxtime = -1,
                       ncores = NULL,
                       seed = NULL,
                       clean = TRUE) {
  R <- as.integer(R)
  stopifnot(R > 0)
  if (!type == 0) {
    stop(
      paste0(
        "The function currently supports",
        "`type = 0`.",
        "\n"
      )
    )
  }
  covariates <- x
  args <- list(
    R = R,
    path = path,
    prefix = prefix,
    n = n,
    time = time,
    delta_t = delta_t,
    mu0 = mu0,
    sigma0_l = sigma0_l,
    alpha = alpha,
    beta = beta,
    psi_l = psi_l,
    nu = nu,
    lambda = lambda,
    theta_l = theta_l,
    type = type,
    x = x,
    gamma = gamma,
    kappa = kappa,
    mu0_fixed = mu0_fixed,
    sigma0_fixed = sigma0_fixed,
    alpha_level = alpha_level,
    optimization_flag = optimization_flag,
    hessian_flag = hessian_flag,
    verbose = verbose,
    weight_flag = weight_flag,
    debug_flag = debug_flag,
    perturb_flag = perturb_flag,
    xtol_rel = xtol_rel,
    stopval = stopval,
    ftol_rel = ftol_rel,
    ftol_abs = ftol_abs,
    maxeval = maxeval,
    maxtime = maxtime,
    ncores = ncores,
    seed = seed,
    clean = clean
  )
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    os_type <- Sys.info()["sysname"]
    if (os_type == "Darwin") {
      fork <- TRUE
    } else if (os_type == "Linux") {
      fork <- TRUE
    } else {
      fork <- FALSE
    }
    if (fork) {
      output <- .PBSSMFixedFork(
        R = R,
        path = path,
        prefix = prefix,
        n = n,
        time = time,
        delta_t = delta_t,
        mu0 = mu0,
        sigma0_l = sigma0_l,
        alpha = alpha,
        beta = beta,
        psi_l = psi_l,
        nu = nu,
        lambda = lambda,
        theta_l = theta_l,
        type = type,
        covariates = covariates,
        gamma = gamma,
        kappa = kappa,
        mu0_fixed = mu0_fixed,
        sigma0_fixed = sigma0_fixed,
        optimization_flag = optimization_flag,
        hessian_flag = hessian_flag,
        verbose = verbose,
        weight_flag = weight_flag,
        debug_flag = debug_flag,
        perturb_flag = perturb_flag,
        xtol_rel = xtol_rel,
        stopval = stopval,
        ftol_rel = ftol_rel,
        ftol_abs = ftol_abs,
        maxeval = maxeval,
        maxtime = maxtime,
        ncores = ncores,
        seed = seed
      )
    } else {
      output <- .PBSSMFixedSocket(
        R = R,
        path = path,
        prefix = prefix,
        n = n,
        time = time,
        delta_t = delta_t,
        mu0 = mu0,
        sigma0_l = sigma0_l,
        alpha = alpha,
        beta = beta,
        psi_l = psi_l,
        nu = nu,
        lambda = lambda,
        theta_l = theta_l,
        type = type,
        covariates = covariates,
        gamma = gamma,
        kappa = kappa,
        mu0_fixed = mu0_fixed,
        sigma0_fixed = sigma0_fixed,
        optimization_flag = optimization_flag,
        hessian_flag = hessian_flag,
        verbose = verbose,
        weight_flag = weight_flag,
        debug_flag = debug_flag,
        perturb_flag = perturb_flag,
        xtol_rel = xtol_rel,
        stopval = stopval,
        ftol_rel = ftol_rel,
        ftol_abs = ftol_abs,
        maxeval = maxeval,
        maxtime = maxtime,
        ncores = ncores,
        seed = seed
      )
    }
  } else {
    output <- .PBSSMFixedSerial(
      R = R,
      path = path,
      prefix = prefix,
      n = n,
      time = time,
      delta_t = delta_t,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      alpha = alpha,
      beta = beta,
      psi_l = psi_l,
      nu = nu,
      lambda = lambda,
      theta_l = theta_l,
      type = type,
      covariates = covariates,
      gamma = gamma,
      kappa = kappa,
      mu0_fixed = mu0_fixed,
      sigma0_fixed = sigma0_fixed,
      optimization_flag = optimization_flag,
      hessian_flag = hessian_flag,
      verbose = verbose,
      weight_flag = weight_flag,
      debug_flag = debug_flag,
      perturb_flag = perturb_flag,
      xtol_rel = xtol_rel,
      stopval = stopval,
      ftol_rel = ftol_rel,
      ftol_abs = ftol_abs,
      maxeval = maxeval,
      maxtime = maxtime,
      seed = seed
    )
  }
  out <- list(
    call = match.call(),
    args = args,
    thetahatstar = output$thetahatstar,
    vcov = stats::var(
      do.call(
        what = "rbind",
        args = output$thetahatstar
      )
    ),
    est = output$prep$est[names(output$thetahatstar[[1]])],
    fun = "PBSSMFixed",
    method = "parametric"
  )
  class(out) <- c(
    "bootstatespace",
    class(out)
  )
  if (clean) {
    unlink(
      file.path(
        path,
        paste0(
          prefix,
          "*.Rds"
        )
      )
    )
  }
  out
}
