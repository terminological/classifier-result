% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributionFunctions.R
\name{LogNormalDistribution}
\alias{LogNormalDistribution}
\title{Get a log normal distribution wrapper}
\description{
Get a log normal distribution wrapper

Get a log normal distribution wrapper
}
\keyword{distributions}
\section{Super class}{
\code{\link[ClassifierResult:Distribution]{ClassifierResult::Distribution}} -> \code{LogNormalDistribution}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{mu}}{the mean of the normal distribuition}

\item{\code{sigma}}{the sd of the normal distribuition}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LogNormalDistribution$new()}}
\item \href{#method-label}{\code{LogNormalDistribution$label()}}
\item \href{#method-theoreticalEntropy}{\code{LogNormalDistribution$theoreticalEntropy()}}
\item \href{#method-clone}{\code{LogNormalDistribution$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="bigP">}\href{../../ClassifierResult/html/Distribution.html#method-bigP}{\code{ClassifierResult::Distribution$bigP()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="getInverseCdf">}\href{../../ClassifierResult/html/Distribution.html#method-getInverseCdf}{\code{ClassifierResult::Distribution$getInverseCdf()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="getPdf">}\href{../../ClassifierResult/html/Distribution.html#method-getPdf}{\code{ClassifierResult::Distribution$getPdf()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="inverse">}\href{../../ClassifierResult/html/Distribution.html#method-inverse}{\code{ClassifierResult::Distribution$inverse()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="p">}\href{../../ClassifierResult/html/Distribution.html#method-p}{\code{ClassifierResult::Distribution$p()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="plot">}\href{../../ClassifierResult/html/Distribution.html#method-plot}{\code{ClassifierResult::Distribution$plot()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="q">}\href{../../ClassifierResult/html/Distribution.html#method-q}{\code{ClassifierResult::Distribution$q()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="sample">}\href{../../ClassifierResult/html/Distribution.html#method-sample}{\code{ClassifierResult::Distribution$sample()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ClassifierResult" data-topic="Distribution" data-id="twoDp">}\href{../../ClassifierResult/html/Distribution.html#method-twoDp}{\code{ClassifierResult::Distribution$twoDp()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
get a LogNormal distribution based on tow paramteretisation options - mean and sd (on natural scale) or mode and sd (on natural scale)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogNormalDistribution$new(
  mode = runif(1, 1, 4),
  sd = runif(1, 0.5, 5),
  mean = NA
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mode}}{the mode on a natural scale}

\item{\code{sd}}{the standard deviation on a natural scale}

\item{\code{mean}}{the mean on a natural scale}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-label"></a>}}
\subsection{Method \code{label()}}{
gets a label for this distribution based on the parameters passed
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogNormalDistribution$label()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-theoreticalEntropy"></a>}}
\subsection{Method \code{theoreticalEntropy()}}{
calculates the theoretical differential entropy
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogNormalDistribution$theoreticalEntropy()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogNormalDistribution$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
