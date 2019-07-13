#' Spectral decay
#'
#' Given a chord sequence, this function simulates the accumulation
#' of acoustic information in sensory memory.
#'
#' @details
#' Chords are represented as objects of class
#' \code{\link[hrep]{milne_pc_spectrum}}.
#' The weights of these pitch-class spectra decay
#' exponentially over time.
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
#' @param offset
#' (Logical scalar)
#' If \code{FALSE}, element i of the output vector
#' corresponds to the accumulated weight
#' at the timepoint immediately after the onset of chord i.
#' If \code{TRUE}, element i corresponds to
#' the accumulated weight at the end of chord i
#' (i.e. just before chord i + 1).
#'
#' @param ...
#' Arguments passed to \code{\link[hrep]{milne_pc_spectrum}}.
#'
#' @return
#' A vector (\code{\link[hrep]{vec}}) of Milne pitch-class spectra
#' (\code{\link[hrep]{milne_pc_spectrum}})
#' produced by accumulating pitch-class spectra over time
#' with exponential decay.
#'
#' @export
#' @rdname spectral_decay
spectral_decay <- function(x, half_life = 3, offset = FALSE, ...) {
  UseMethod("spectral_decay")
}

#' @export
#' @rdname spectral_decay
spectral_decay.vec <- function(x, half_life = 3, offset = FALSE, ...) {
  spectral_decay(hrep::represent(x, "milne_pc_spectrum", ...),
                 half_life = half_life,
                 offset = offset)
}

#' @export
#' @rdname spectral_decay
spectral_decay.coded_vec <- function(x, ...) {
  spectral_decay(hrep::decode(x), ...)
}

#' @export
#' @rdname spectral_decay
spectral_decay.vec_milne_pc_spectrum <- function(x, half_life = 3, offset = FALSE, ...) {
  checkmate::qassert(half_life, "N1(0,]")
  checkmate::qassert(offset, "B1")
  if (length(x) == 0L) {
    return(x)
  } else {
    spec_dim <- purrr::map_int(x, length) %>% unique()
    if (length(spec_dim) > 1L)
      stop("all input pitch-class spectra must have the same dimensionality")
    for (i in seq_along(x)) {
      if (offset) {
        x[[i]] <- decay(if (i == 1) x[[i]] else x[[i]] + x[[i - 1L]],
                        half_lives = 1 / half_life)

      } else {
        x[[i]] <- x[[i]] + if (i == 1) 0 else decay(x[[i - 1L]],
                                                    half_lives = 1 / half_life)

      }
    }
    return(x)
  }
}

#' Decay
#'
#' Takes a numeric vector and applies exponential decay.
#'
#' @param x Numeric vector to decay.
#'
#' @param half_lives (Numeric scalar) The number of half lifes that have elapsed.
#'
#' @return A numeric vector.
#'
decay <- function(x, half_lives) {
  checkmate::qassert(x, "N")
  checkmate::qassert(half_lives, "N1[0,]")
  x * 2 ^ (- half_lives)
}
