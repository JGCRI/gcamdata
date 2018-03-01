# Test the dstrace utility

context("file_encoding")

test_that("inputs are encoded correctly", {

  root <- system.file("extdata", package = "gcamdata")

  # Get a list of all input files: CSV files that may or may not be already compressed
  files <- list.files(root, pattern = "\\.csv(\\.gz|\\.zip)?$", full.names = TRUE, recursive = TRUE)
  tf <- tempfile()
  td <- tempdir()

  for(f in files) {
    # Decompress the file if necessary
    if(grepl("\\.gz$", f)) {
      R.utils::gunzip(f, remove = FALSE, destname = tf, overwrite = TRUE)
    } else if(grepl("\\.zip$", f)) {
      zipfiles <- unzip(f, exdir = tempdir())
      file.copy(zipfiles[1], tf, overwrite = TRUE)
    } else {
      file.copy(f, tf, overwrite = TRUE)
    }

    # Read first few bytes of file to check line endings
    bytes <- paste(readBin(tf, "raw", n = 1000), collapse = "")
    expect_false(grepl("0d0a", bytes), info = paste("Windows line endings found in", f))

    # Read file - shouldn't produce warning
    expect_silent(readLines(tf)) #, info = paste("Embedded nuls or no final EOL in", f))
  }
})
