.PBCoefDynr <- function(i,
                        path,
                        prefix) {
  .CoefFitDynr(
    readRDS(
      file = file.path(
        path,
        paste0(
          prefix,
          "_",
          "fit",
          "_",
          i,
          ".Rds"
        )
      )
    )
  )
}
