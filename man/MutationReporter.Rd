% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporter.R
\name{MutationReporter}
\alias{MutationReporter}
\title{Reporter for Mutation Testing}
\description{
The job of a mutation reporter is to aggregate and display the results of mutation tests.
It tracks each mutation attempt, reporting on whether the tests killed the mutation or the mutation survived.
}
\seealso{
Other MutationReporter: 
\code{\link{ProgressMutationReporter}},
\code{\link{default_reporter}()}
}
\concept{MutationReporter}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{test_reporter}}{Reporter to use for the testthat::test_dir function}

\item{\code{out}}{Output destination for reporter messages}

\item{\code{width}}{Width of the console in characters}

\item{\code{unicode}}{Whether Unicode output is supported}

\item{\code{crayon}}{Whether colored output is supported}

\item{\code{rstudio}}{Whether running in RStudio}

\item{\code{hyperlinks}}{Whether terminal hyperlinks are supported}

\item{\code{current_file}}{Path of the file currently being mutated}

\item{\code{current_mutator}}{Mutator currently being applied}

\item{\code{plan}}{Complete mutation plan for the test run}

\item{\code{results}}{List of mutation test results, indexed by file path}

\item{\code{current_score}}{Current score of the mutation tests}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MutationReporter-new}{\code{MutationReporter$new()}}
\item \href{#method-MutationReporter-start_reporter}{\code{MutationReporter$start_reporter()}}
\item \href{#method-MutationReporter-start_file}{\code{MutationReporter$start_file()}}
\item \href{#method-MutationReporter-start_mutator}{\code{MutationReporter$start_mutator()}}
\item \href{#method-MutationReporter-add_result}{\code{MutationReporter$add_result()}}
\item \href{#method-MutationReporter-end_mutator}{\code{MutationReporter$end_mutator()}}
\item \href{#method-MutationReporter-end_file}{\code{MutationReporter$end_file()}}
\item \href{#method-MutationReporter-end_reporter}{\code{MutationReporter$end_reporter()}}
\item \href{#method-MutationReporter-get_score}{\code{MutationReporter$get_score()}}
\item \href{#method-MutationReporter-cat_tight}{\code{MutationReporter$cat_tight()}}
\item \href{#method-MutationReporter-cat_line}{\code{MutationReporter$cat_line()}}
\item \href{#method-MutationReporter-rule}{\code{MutationReporter$rule()}}
\item \href{#method-MutationReporter-clone}{\code{MutationReporter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-new"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new reporter
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$new(test_reporter = "silent", file = stdout())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{test_reporter}}{Reporter to use for the testthat::test_dir function}

\item{\code{file}}{Output destination (default: stdout)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-start_reporter"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-start_reporter}{}}}
\subsection{Method \code{start_reporter()}}{
Start reporter
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$start_reporter(plan = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{plan}}{The complete mutation plan}

\item{\code{temp_dir}}{Path to the temporary directory for testing}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-start_file"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-start_file}{}}}
\subsection{Method \code{start_file()}}{
Start testing a file
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$start_file(filename)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{filename}}{Path to the file being mutated}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-start_mutator"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-start_mutator}{}}}
\subsection{Method \code{start_mutator()}}{
Start testing with a specific mutator
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$start_mutator(mutator)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mutator}}{The mutator being applied}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-add_result"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-add_result}{}}}
\subsection{Method \code{add_result()}}{
Add a mutation test result
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$add_result(plan, killed, survived, errors)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{plan}}{Current testing plan. See \code{plan()}.}

\item{\code{killed}}{Whether the mutation was killed by tests}

\item{\code{survived}}{Number of survived mutations}

\item{\code{errors}}{Number of errors encountered}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-end_mutator"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-end_mutator}{}}}
\subsection{Method \code{end_mutator()}}{
End testing with current mutator
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$end_mutator()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-end_file"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-end_file}{}}}
\subsection{Method \code{end_file()}}{
End testing current file
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$end_file()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-end_reporter"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-end_reporter}{}}}
\subsection{Method \code{end_reporter()}}{
End reporter and show summary
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$end_reporter()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-get_score"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-get_score}{}}}
\subsection{Method \code{get_score()}}{
Get the current score
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$get_score()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-cat_tight"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-cat_tight}{}}}
\subsection{Method \code{cat_tight()}}{
Print a message to the output
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$cat_tight(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Message to print}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-cat_line"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-cat_line}{}}}
\subsection{Method \code{cat_line()}}{
Print a message to the output
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$cat_line(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Message to print}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-rule"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-rule}{}}}
\subsection{Method \code{rule()}}{
Print a message to the output with a rule
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$rule(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Message to print}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MutationReporter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MutationReporter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutationReporter$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
