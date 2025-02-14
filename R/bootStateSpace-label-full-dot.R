.LabelFull <- function(p,
                       label) {
  chars <- seq_len(p)
  outer(
    X = chars,
    Y = chars,
    FUN = function(x, y) {
      paste0(
        label,
        "_",
        x,
        "_",
        y
      )
    }
  )
}
