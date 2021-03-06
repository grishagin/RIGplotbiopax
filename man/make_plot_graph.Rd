% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_plot_graph.R
\name{make_plot_graph}
\alias{make_plot_graph}
\title{Make and Plot BioPAX Graph}
\usage{
make_plot_graph(allnodes, alledges, tag = NULL, pw_name = "Pathway")
}
\arguments{
\item{allnodes}{Dataframe with all nodes.}

\item{alledges}{Dataframe with all edges.}

\item{tag}{Tag for the file name.}

\item{pw_name}{Pathway name (optional). It will show up on-hover over the SVG background.}
}
\description{
Using dataframes of nodes and edges make and plot the graph.
}
\author{
Ivan Grishagin
}
