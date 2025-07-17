debugging <- TRUE

test_that("agg_webp_anim produces animated WebP output", {
  tmp <- tempfile(fileext = ".webp")
  nframes <- 8
  agg_webp_anim(tmp, width = 400, height = 300, delay = 300, loop = 0)
  for (i in seq_len(nframes)) {
    plot.new()
    x <- i / nframes
    y <- 0.5
    symbols(x, y, circles = 0.1, inches = FALSE, add = TRUE, bg = "red")
    dev.flush()
  }
  dev.off()

  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 1000)
  if (debugging) cat("Animated WebP created at:", tmp, "\n") else unlink(tmp)
})