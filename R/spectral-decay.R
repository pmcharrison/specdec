#' @export
#' @rdname spectral_decay
spectral_decay <- function(x, half_life = 3, offset = FALSE, ...) {
  UseMethod("spectral_decay")
}

#' @export
#' @rdname spectral_decay
spectral_decay.default <- function(x, half_life = 3, offset = FALSE, ...) {
  spectral_decay(hrep::represent(x, "milne_pc_spectrum"),
                 half_life = half_life,
                 offset = offset,
                 ...)
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
                        half_lifes = 1 / half_life)

      } else {
        x[[i]] <- x[[i]] + if (i == 1) 0 else decay(x[[i - 1L]],
                                                    half_lifes = 1 / half_life)

      }
    }
    return(x)
  }
}

decay <- function(x, half_lifes) {
  checkmate::qassert(x, "N")
  checkmate::qassert(half_lifes, "N[0,]")
  x * 2 ^ (- half_lifes)
}
