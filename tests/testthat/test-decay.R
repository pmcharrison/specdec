context("test-decay")

test_that("examples", {
  decay(1, half_lives = 1) %>%
    expect_equal(0.5)

  decay(1, half_lives = 2) %>%
    expect_equal(0.25)

  decay(1:3, half_lives = 2) %>%
    expect_equal(c(0.25, 0.5, 0.75))
})
