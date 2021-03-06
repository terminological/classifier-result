% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributionFunctions.R
\name{Distribution}
\alias{Distribution}
\title{Distribution class}
\description{
Distribution class

Distribution class
}
\details{
The distribution class wrapps PDF and Quatile functions from a number of distributions and provides some simple stats
for those functions, including a sampling function
}
\keyword{distributions}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pdf}}{the density function}

\item{\code{invCdf}}{the centile function}

\item{\code{cdf}}{the distribution function}

\item{\code{dots}}{the parameters of the pdf}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Distribution$new()}}
\item \href{#method-p}{\code{Distribution$p()}}
\item \href{#method-bigP}{\code{Distribution$bigP()}}
\item \href{#method-q}{\code{Distribution$q()}}
\item \href{#method-getPdf}{\code{Distribution$getPdf()}}
\item \href{#method-getInverseCdf}{\code{Distribution$getInverseCdf()}}
\item \href{#method-theoreticalEntropy}{\code{Distribution$theoreticalEntropy()}}
\item \href{#method-label}{\code{Distribution$label()}}
\item \href{#method-inverse}{\code{Distribution$inverse()}}
\item \href{#method-sample}{\code{Distribution$sample()}}
\item \href{#method-plot}{\code{Distribution$plot()}}
\item \href{#method-twoDp}{\code{Distribution$twoDp()}}
\item \href{#method-clone}{\code{Distribution$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
Creates a distribution
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$new(density, quantile, distribution, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{density}}{a function that accepts at least a vector of x value (e.g. dnorm)}

\item{\code{quantile}}{a function that accepts at least  a vector of p values (e.g. qnorm)}

\item{\code{...}}{passed to pdfFunction and centileFunction}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-p"></a>}}
\subsection{Method \code{p()}}{
calculate the probability density at x
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$p(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{a vector of values of x}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a vector of values of (p(x))
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-bigP"></a>}}
\subsection{Method \code{bigP()}}{
calculate the cumulative density at x
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$bigP(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{a vector of values of x}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a vector of values of (P(x))
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-q"></a>}}
\subsection{Method \code{q()}}{
calculate the value of x for a centile y
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$q(y)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{y}}{a vector of centiles}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a vector of values of x
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getPdf"></a>}}
\subsection{Method \code{getPdf()}}{
the PDF and CDF for a distribution as a dataframe
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$getPdf(xmin, xmax, resolution = 1001)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xmin}}{the smallest value}

\item{\code{xmax}}{the smallest value}

\item{\code{resolution}}{the number of increments in x}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getInverseCdf"></a>}}
\subsection{Method \code{getInverseCdf()}}{
defines a inverse CDF as a data frame
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$getInverseCdf(resolution = 1001)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{resolution}}{the number of increments in y}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-theoreticalEntropy"></a>}}
\subsection{Method \code{theoreticalEntropy()}}{
calculates the integral of -p(x)*log(p(x)) from -Infinity to Infinity
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$theoreticalEntropy()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-label"></a>}}
\subsection{Method \code{label()}}{
gets a label for this distribution based on the parameters passed
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$label()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-inverse"></a>}}
\subsection{Method \code{inverse()}}{
gets a label for this distribution based on the parameters passed
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$inverse(y)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{y}}{a vector of y values to apply the quantile function to}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a daat frame of y and corresponding x values
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sample"></a>}}
\subsection{Method \code{sample()}}{
produce a set of samples conforming to this distribution
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$sample(n = 10)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{the number of samples}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a data frame of samples (labelled x)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plot"></a>}}
\subsection{Method \code{plot()}}{
plot this dictributions as pdf and cdf
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$plot(xmin, xmax, resolution = 1001)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xmin}}{the minimum of the support range to plot}

\item{\code{xmax}}{the maximum of the support range to plot}

\item{\code{resolution}}{the number of points to generate for the plot (default 1001)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a ggassemble plot object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-twoDp"></a>}}
\subsection{Method \code{twoDp()}}{
a 2 dp formatter
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$twoDp(x, unit = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{a number}

\item{\code{unit}}{a unit}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Distribution$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
