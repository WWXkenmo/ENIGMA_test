% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotLossCurve.R
\name{plotLossCurve}
\alias{plotLossCurve}
\title{plot multiple loss curves of ENIGMA object}
\usage{
plotLossCurve(
  object,
  rlgType = "trace_norm",
  scale = "log",
  shape = FALSE,
  name_prefix = "method",
  all = TRUE,
  plotTerm = 1,
  moi = NULL
)
}
\arguments{
\item{object}{ENIGMA object}

\item{rlgType}{plot which type of model. "maximum L2 norm" or "trace norm"}

\item{scale}{if scale loss value. log or raw}

\item{shape}{if loss curve dot shape by the models}

\item{name_prefix}{the title of model comparison}

\item{all}{if plot the all object function in one figure}

\item{plotTerm}{plot which object function. The alternative values are 1,2,3,4(1: distance to bulk; 2: distance to reference; 3: regularization term; 4: total loss)}

\item{moi}{the model of interets to plot, input the name of models}

\item{alpha}{}
}
\value{
the ggplot object contains the plot of loss curve
}
\description{
plot multiple loss curves of ENIGMA object
}
\examples{
\dontrun{
plotMultiLossCurve(egm,rlgType = "trace_norm") # plot all saved trace norm models
plotMultiLossCurve(egm,rlgType = "trace_norm", scale="log")# plot all saved trace norm models and use log scale
}

}
