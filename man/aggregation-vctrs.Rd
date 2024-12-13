% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agg_vec.R, R/edge_vec.R, R/graph_vec.R
\name{aggregation-vctrs}
\alias{aggregation-vctrs}
\alias{vec_ptype2.agg_vec}
\alias{vec_ptype2.agg_vec.agg_vec}
\alias{vec_ptype2.agg_vec.default}
\alias{vec_ptype2.agg_vec.character}
\alias{vec_ptype2.character.agg_vec}
\alias{vec_ptype_abbr.agg_vec}
\alias{vec_cast.agg_vec}
\alias{vec_cast.agg_vec.agg_vec}
\alias{vec_cast.agg_vec.default}
\alias{vec_cast.character.agg_vec}
\alias{vec_proxy_compare.agg_vec}
\alias{vec_ptype2.edge_vec}
\alias{vec_ptype2.edge_vec.default}
\alias{vec_ptype2.edge_vec.character}
\alias{vec_cast.edge_vec}
\alias{vec_cast.edge_vec.edge_vec}
\alias{vec_cast.edge_vec.default}
\alias{vec_cast.character.edge_vec}
\alias{vec_ptype_abbr.edge_vec}
\alias{vec_ptype2.graph_vec}
\alias{vec_ptype2.graph_vec.default}
\alias{vec_ptype2.graph_vec.character}
\alias{vec_cast.graph_vec}
\alias{vec_cast.graph_vec.graph_vec}
\alias{vec_cast.graph_vec.default}
\alias{vec_cast.character.graph_vec}
\alias{vec_cast.graph_vec.character}
\alias{vec_ptype_abbr.graph_vec}
\title{Internal vctrs methods}
\usage{
\method{vec_ptype2}{agg_vec}(x, y, ...)

\method{vec_ptype2}{agg_vec.agg_vec}(x, y, ...)

\method{vec_ptype2}{agg_vec.default}(x, y, ...)

\method{vec_ptype2}{agg_vec.character}(x, y, ...)

\method{vec_ptype2}{character.agg_vec}(x, y, ...)

\method{vec_ptype_abbr}{agg_vec}(x, ...)

\method{vec_cast}{agg_vec}(x, to, ...)

\method{vec_cast}{agg_vec.agg_vec}(x, to, ...)

\method{vec_cast}{agg_vec.default}(x, to, ...)

\method{vec_cast}{character.agg_vec}(x, to, ...)

\method{vec_proxy_compare}{agg_vec}(x, ...)

\method{vec_ptype2}{edge_vec}(x, y, ...)

\method{vec_ptype2}{edge_vec.default}(x, y, ...)

\method{vec_ptype2}{edge_vec.character}(x, y, ...)

\method{vec_cast}{edge_vec}(x, to, ...)

\method{vec_cast}{edge_vec.edge_vec}(x, to, ...)

\method{vec_cast}{edge_vec.default}(x, to, ...)

\method{vec_cast}{character.edge_vec}(x, to, ...)

\method{vec_ptype_abbr}{edge_vec}(x, ...)

\method{vec_ptype2}{graph_vec}(x, y, ...)

\method{vec_ptype2}{graph_vec.default}(x, y, ...)

\method{vec_ptype2}{graph_vec.character}(x, y, ...)

\method{vec_cast}{graph_vec}(x, to, ...)

\method{vec_cast}{graph_vec.graph_vec}(x, to, ...)

\method{vec_cast}{graph_vec.default}(x, to, ...)

\method{vec_cast}{character.graph_vec}(x, to, ...)

\method{vec_cast}{graph_vec.character}(x, to, ...)

\method{vec_ptype_abbr}{graph_vec}(x, ...)
}
\description{
These methods are the extensions that allow aggregation vectors to work with
vctrs.
}
\keyword{internal}
