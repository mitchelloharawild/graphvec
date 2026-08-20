# nocov start
.onLoad <- function(...) {
  register_s3_method("pillar", "pillar_shaft", "agg_vec")
  register_s3_method("pillar", "pillar_shaft", "node_vec")
  register_s3_method("pillar", "pillar_shaft", "edge_vec")
  register_s3_method("pillar", "type_sum", "agg_vec")
  register_s3_method("pillar", "type_sum", "node_vec")
  register_s3_method("pillar", "type_sum", "edge_vec")
  register_s3_method("igraph", "as.igraph", "agg_vec")
  register_s3_method("igraph", "as.igraph", "node_vec")
  register_s3_method("igraph", "as.igraph", "edge_vec")
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
