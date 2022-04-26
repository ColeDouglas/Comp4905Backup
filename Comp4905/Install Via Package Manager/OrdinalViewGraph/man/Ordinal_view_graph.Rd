\name{Ordinal_view_graph}
\alias{Ordinal_view_graph}
\title{Ordinal View Graph}
\usage{
ordinal_view_graph()
}
\description{
Takes in ordinal data and visualises it.


Optional params:


  - Probability = the alpha probability you are looking for

    default = 0.05


  - tails   = Whether you want the test to be two tailed or not

    default = 2


  - rankVector = The ranking of the categories to generate a categorical interpretation.

   default = NA


}
\examples{
  Ordinal_view_graph(input.csv)
  Ordinal_view_graph(input.csv, .37, 1)
  Ordinal_view_graph(input.csv, .37, 2, ("green","blue","red"))
}
