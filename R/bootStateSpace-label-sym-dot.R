.LabelSym <- function(p,
                      label) {
  chars <- seq_len(p)
  outer(
    X = chars,
    Y = chars,
    FUN = function(x, y) {
      ifelse(
        test = x <= y,
        yes = paste0(
          label,
          "_",
          y,
          "_",
          x
        ),
        no = paste0(
          label,
          "_",
          x,
          "_",
          y
        )
      )
    }
  )
}
