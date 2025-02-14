#' Parametric Bootstrap for the
#' Vector Autoregressive Model
#' (Fixed Parameters)
#'
#' This function simulates data from
#' a vector autoregressive model
#' using a state-space model parameterization
#' and fits the model using the `dynr` package.
#' The process is repeated `R` times.
#' It assumes that the parameters remain constant
#' across individuals and over time.
#' At the moment, the function only supports
#' `type = 0`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams simStateSpace::SimSSMVARFixed
#' @inheritParams PBSSMFixed
#' @inheritParams dynr::dynr.cook
#' @inherit simStateSpace::SimSSMVARFixed details
#' @inherit PBSSMFixed references
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
#'     \item{fun}{Function used ("PBSSMVARFixed").}
#'     \item{method}{Bootstrap method used ("parametric").}
#'   }
#'
#' @examples
#' \donttest{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
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
#' @keywords bootStateSpace boot pb var
#' @export
PBSSMVARFixed <- function(R,
                          path,
                          prefix,
                          n, time,
                          mu0, sigma0_l,
                          alpha, beta, psi_l,
                          type = 0,
                          x = NULL, gamma = NULL,
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
    mu0 = mu0,
    sigma0_l = sigma0_l,
    alpha = alpha,
    beta = beta,
    psi_l = psi_l,
    type = type,
    x = x,
    gamma = gamma,
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
      output <- .PBSSMVARFixedFork(
        R = R,
        path = path,
        prefix = prefix,
        n = n,
        time = time,
        mu0 = mu0,
        sigma0_l = sigma0_l,
        alpha = alpha,
        beta = beta,
        psi_l = psi_l,
        type = type,
        covariates = covariates,
        gamma = gamma,
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
      output <- .PBSSMVARFixedSocket(
        R = R,
        path = path,
        prefix = prefix,
        n = n,
        time = time,
        mu0 = mu0,
        sigma0_l = sigma0_l,
        alpha = alpha,
        beta = beta,
        psi_l = psi_l,
        type = type,
        covariates = covariates,
        gamma = gamma,
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
    output <- .PBSSMVARFixedSerial(
      R = R,
      path = path,
      prefix = prefix,
      n = n,
      time = time,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      alpha = alpha,
      beta = beta,
      psi_l = psi_l,
      type = type,
      covariates = covariates,
      gamma = gamma,
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
    fun = "PBSSMVARFixed",
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
