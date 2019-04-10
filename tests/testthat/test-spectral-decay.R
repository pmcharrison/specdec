context("test-spectral-decay")

test_that("regression test", {
  chords <- list(
    hrep::pi_chord(c(60, 64, 67)),
    hrep::pi_chord(c(59, 62, 67)),
    hrep::pi_chord(c(60, 64, 67))
  ) %>%
    hrep::vec("pi_chord")

  decayed_onset <- chords %>% spectral_decay(half_life = 1)

  manual_onset_1 <- hrep::pi_chord(c(60, 64, 67)) %>% hrep::milne_pc_spectrum()
  manual_onset_2 <- hrep::pi_chord(c(59, 62, 67)) %>% hrep::milne_pc_spectrum() + manual_onset_1 / 2
  manual_onset_3 <- hrep::pi_chord(c(60, 64, 67)) %>% hrep::milne_pc_spectrum() + manual_onset_2 / 2

  expect_equal(decayed_onset[[1]], manual_onset_1)
  expect_equal(decayed_onset[[2]], manual_onset_2)
  expect_equal(decayed_onset[[3]], manual_onset_3)

  decayed_offset <- chords %>% spectral_decay(half_life = 1, offset = TRUE)

  manual_offset_1 <- (hrep::pi_chord(c(60, 64, 67)) %>% hrep::milne_pc_spectrum()) / 2
  manual_offset_2 <- (hrep::pi_chord(c(59, 62, 67)) %>% hrep::milne_pc_spectrum() + manual_offset_1) / 2
  manual_offset_3 <- (hrep::pi_chord(c(60, 64, 67)) %>% hrep::milne_pc_spectrum() + manual_offset_2) / 2

  expect_equal(decayed_offset[[1]], manual_offset_1)
  expect_equal(decayed_offset[[2]], manual_offset_2)
  expect_equal(decayed_offset[[3]], manual_offset_3)
})
