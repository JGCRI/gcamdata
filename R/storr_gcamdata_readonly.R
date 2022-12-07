#' storr_gcamdata_readonly
#'
#' Object cache driver that builds on the cache drivers in storr to
#' read from a cache without writing anything new. Instead it writes to
#' a temporary environment that is not saved.
#'
#' FINISH DOCUMENTATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#' For example, we can use the drake cache as a the read-only driver and then when running
#' driver_drake(cache = storr_gcamdata_readonly)
#' #' FINISH DOCUMENTATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#'
#'
#' @param read_driver A storr::storr_rds() cache driver that is only used to read data. Typically, this will be the standard drake cache.
#' @param write_driver A storr::storr_environment() cache driver where data will be written, but not saved. Typicall NULL.
#' @export
storr_gcamdata_readonly <- function(read_driver = NULL, write_driver = NULL) {
  storr::storr(gcamdata_R6_driver_class$new(read_driver, write_driver))
}

gcamdata_R6_driver_class <- R6::R6Class(
  "gcamdata_driver_class",
  public = list(
    read_driver = NULL,
    write_driver = NULL,
    traits = list(accept = "object"),

    initialize = function(read_driver, write_driver) {
      if (is.null(read_driver)) {
        # Might need to rethink this - what should default be?
        # Should we even allow a null read_driver?
        read_driver <- storr::storr_rds(tempfile())
      } else {
        # What do we need to check here??
        assertthat::are_equal(typeof(read_driver), "environment")
        assertthat::are_equal(read_driver$traits$accept, "raw")
      }
      if (is.null(write_driver)) {
        write_driver <- storr::storr_environment()
      } else {
        # need to check that this is storr_environment and not storr_rds
        assertthat::are_equal(typeof(write_driver), "environment")
        assertthat::are_equal(write_driver$traits$accept, "object")
      }
      self$read_driver <- read_driver
      self$write_driver <- write_driver
    },

    type = function() {
      "environment"
    },

    destroy = function() {
      # Only want to destroy the write_driver
      self$write_driver <- self$write_driver$destroy()
    },

    get_hash = function(key, namespace) {
      # Get from write_driver if available (this will be more recent)
      # Otherwise get from read_driver
      if (key %in% self$write_driver$list()){
        self$write_driver$get_hash(key, namespace)
      } else {
        self$read_driver$get_hash(key, namespace)
      }
    },

    set_hash = function(key, namespace, hash) {
      # Only ever set to write_driver
      self$write_driver$driver$set_hash(key, namespace, hash)
    },

    get_object = function(hash) {
      # Get from write_driver if available (this will be more recent)
      # Otherwise get from read_driver
      if (hash %in% self$write_driver$list_hashes()){
        self$write_driver$driver$get_object(hash)
      } else {
        self$read_driver$driver$get_object(hash)
        }
      },

    set_object = function(hash, value) {
      # Only ever set to write_driver
      self$write_driver$driver$set_object(hash, value)
    },

    exists_hash = function(key, namespace) {
      # Get from write_driver if available (this will be more recent)
      # Otherwise get from read_driver
      if (key %in% self$write_driver$list()){
        self$write_driver$driver$exists_hash(key, namespace)
      } else {
        self$read_driver$driver$exists_hash(key, namespace)
      }
    },

    exists_object = function(hash) {
      # Get from write_driver if available (this will be more recent)
      # Otherwise get from read_driver
      if (hash %in% self$write_driver$list_hashes()){
        self$write_driver$exists_object(hash)
      } else {
        self$read_driver$exists_object(hash)
      }
    },

    del_hash = function(key, namespace) {
      # Only ever delete to write_driver
      self$write_driver$driver$del_hash(key, namespace)
    },

    del_object = function(hash) {
      # Only ever delete to write_driver
      self$write_driver$driver$del_object(hash)
    },

    list_hashes = function() {
      # combine both lists
      unique(c(self$write_driver$list_hashes(), self$read_driver$list_hashes()))
    },

    list_keys = function(namespace) {
      # combine both lists
      unique(c(self$write_driver$driver$list_keys(namespace), self$read_driver$driver$list_keys(namespace)))
    },

    list_namespaces = function() {
      # combine both lists
      unique(c(self$write_driver$list_namespaces(), self$read_driver$list_namespaces()))
    }
  ))
