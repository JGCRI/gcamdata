# Test that files are encoded correctly (line endings, blank final line)

context("file_encoding")

test_that("inputs are encoded correctly", {

  root <- "../../inst/extdata"

  # Get a list of all input files: CSV files that may or may not be already compressed
  files <- list.files(root, pattern = "\\.csv(\\.gz|\\.zip)?$", full.names = TRUE, recursive = TRUE)
  td <- tempdir()
  # Copy all data to temp and perform normalizations there
  expect_true(file.copy(root, td, recursive = TRUE))
  normalize_files(paste(td, "extdata", sep="/"))

  for(f in files) {
    # we expect files to be the same and unmodified
    new_f = paste(td, sub(root, 'extdata', f), sep = "/")
    expect_true(file.exists(new_f),
                info = paste("Normalized file not found for ", f, ", it could have gotten compressed"))
    expect_equal(as.vector(tools::md5sum(f)), as.vector(tools::md5sum(new_f)),
                 info = paste("Normalized file does not match for ", f))
  }
})
