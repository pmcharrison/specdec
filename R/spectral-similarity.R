#' Spectral similarity
#'
#' Computes the spectral similarity of successive chords in a chord sequence
#' to an integrated acoustic image of their recent context,
#' as computed by \code{\link{spectral_decay}}.
#'
#' @param x
#' Chord sequence as created by \code{\link[hrep]{vec}}.
#' This chord sequence will be coerced to a
#' \code{\link[hrep]{milne_pc_spectrum}} representation.
#'
#' @param half_life
#' (Numeric scalar)
#' Half-life of the exponential decay, in units of chords.
#'
#' @param ...
#' Arguments passed to \code{\link[hrep]{milne_pc_spectrum}}.
#'
#' @return Numeric vector, where the ith element corresponds to the spectral similarity
#' of chord i to its preceding acoustic context.
#' The first element is always NA.
#'
#' @export
spectral_similarity <- function(x, half_life = 3, ...) {
  spectra <- hrep::represent(x, "milne_pc_spectrum", ...)
  decay_spectra <- spectral_decay(spectra,
                                  half_life = half_life,
                                  offset = TRUE)
  res <- rep(as.numeric(NA), length = length(x))
  if (length(x) > 1L) {
    for (i in seq(from = 2L, to = length(x))) {
      res[i] <- hrep::cosine_similarity(decay_spectra[[i - 1L]],
                                        spectra[[i]])
    }
  }
  res
}
