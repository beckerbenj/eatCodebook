% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createInputForDescriptives.r
\name{createInputForDescriptives}
\alias{createInputForDescriptives}
\title{Create input data.frame for subsequent calculation of descriptives}
\usage{
createInputForDescriptives(
  GADSdat,
  idExpr = "^ID",
  impExpr = c("IMPUTATION\\\\s+{0,1}[[:digit:]]{1,2}", "PV\\\\s+{0,1}[[:digit:]]{1,2}"),
  scaleExpr = "^Skala",
  itemExpr = "plausible|indikator",
  fakeItemExpr = "fake",
  nwExpr = "IDinClass",
  varNameSeparatorImp = "_",
  ncharSeparatorImp = 2,
  lastOccurrence = TRUE,
  groupSuffixImp = "imp",
  nCatsForOrdinal = c(2:5),
  nwVarNameSeparatorImp = "_",
  nwNcharSeparatorImp = 6,
  nwLastOccurrence = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{GADSdat}{Object of class \code{GADSdat}, created by \code{import_spss} from the \code{eatGADS}
package, for example. Alternatively, a list of objects of class \code{GADSdat}}

\item{idExpr}{Regular expression to identify ID variables from variable names (Note: for multiple
expressions, i.e. if \code{idExpr} is a character vector of length > 1, at least one expression
should match to identify the variable as ID variable). The use of a regular expression is due to
the fact that the data records often contain not just one, but several identification variables,
for example for pupils, teachers, schools, classes or federal states. Logically, no descriptive
values are reported for identification variables. They must be specified here so that the subsequent
\code{calculateDescriptives} function knows, so to speak, for which variables no descriptive values are
to be calculated.}

\item{impExpr}{Regular expression to identify imputed variables from variable labels in GADSdat
object (Note: for multiple expressions, i.e. if \code{impExpr} is a character vector of length > 1,
at least one expression should match to identify the variable as an imputed variable). Regular
expressions are also used here, as several variables (each with different individual IDs) can be imputed.}

\item{scaleExpr}{Regular expression to identify scale or fake scale variables from variable labels in GADSdat
object (Note: for multiple expressions, i.e. if \code{scaleExpr} is a character vector of length > 1,
at least one expression should match to identify the variable as a scale variable). Scales are defined when
several items measure a common (latent) construct and there is also a variable that represents the scale value.
The scale variable represents usually the averaged (or otherwise aggregated) value within a person across all
items of this scale. Fake scales are defined when several items measure a common (latent) construct, but there
is no additional variable that represents the scale value.}

\item{itemExpr}{Regular expression to identify items which constitute a true scale from the variable
labels in GADSdat object. Note: Only the regular expressions that identify the items must be entered here.
The additional scale variables do not have to be specified. If several scales are defined in the data set
(e.g. "self concept" and interest), no distinction needs to be made here as to which items belong to which
scale. (This is done elsewhere.) Assume that the "self concept" is measured with the items \code{SK_I1},
\code{SK_I2}, \code{SK_I3}, and the scale variable is called \code{SK_scale}. Let us also assume that
"interest" is measured with the items \code{Int_I1}, \code{Int_I2}, \code{Int_I3}, \code{Int_I4}, and
that the scale variable is called \code{Int_scale}. Then it could be specified
here: \code{itemExpr = "I[1-4]{1}$"}}

\item{fakeItemExpr}{Regular expression to identify fake items which constitute a fake scale from the variable
labels in GADSdat object. This works in the same way as with \code{itemExpr}.}

\item{nwExpr}{Regular expression to identify network variables from variable labels in GADSdat object
(Note: for multiple expressions, i.e. if \code{nwExpr} is a character vector of length > 1, at least
one expression should match to identify the variable as a network variable)}

\item{varNameSeparatorImp}{character sign to separate the "pooled" suffix from group name in group
column. For example, if multiple imputed variables occur in the wide-format data.frame as \code{pv_1},
\code{pv_2}, \code{pv_3}, use \code{"_"}. If no such sign exists in the data, i.e. if multiple imputations
occur as \code{pv1}, \code{pv2}, \code{pv3}, instead of \code{pv_1}, \code{pv_2}, \code{pv_3}, or \code{pv.1},
\code{pv.2}, \code{pv.3}, use \code{NA} or \code{NULL} or \code{""}. In this case, you will have to specify
the \code{ncharSeparatorImp} argument.}

\item{ncharSeparatorImp}{Integer: only relevant if no \code{varNameSeparatorImp} exists, i.e. if multiple
imputations occur as \code{pv1}, \code{pv2}, \code{pv3}, instead of \code{pv_1}, \code{pv_2}, \code{pv_3},
or \code{pv.1}, \code{pv.2}, \code{pv.3}. \code{ncharSeparatorImp} than specifies the number of character
signs which should be trimmed to identify the common variable stem. If \code{varNameSeparatorImp} is not
\code{NA} or \code{NULL} or \code{""}, \code{ncharSeparatorImp} will be ignored. For example, if multiple
imputations occur as \code{pv_1}, \code{pv_2}, \code{pv_3}, use \code{varNameSeparatorImp = "_"}. If multiple
imputations occur as \code{pv1}, \code{pv2}, \code{pv3}, use \code{varNameSeparatorImp = NULL} and
\code{ncharSeparatorImp = 2}. The first 2 signs of variables names (i.e., \code{"pv"}) will be used to
identify the imputed variables which belong to a common stem.}

\item{lastOccurrence}{Logical: If \code{varNameSeparatorImp} occurrs multiple times within a string,
\code{lastOccurrence} defines whether the last occurrence should be used for splitting}

\item{groupSuffixImp}{tbd}

\item{nCatsForOrdinal}{Numeric vector with number of categories considered for ordinal variables. Variables
with number of categories as defined here are considered to be ordinal instead of nominal. If NULL, this rule
will be ignored, and nominal/ordinal assignment is done in other ways}

\item{nwVarNameSeparatorImp}{character sign to separate network variable names from network variable groups.
For example, if network variables occur as \code{friend_1}, \code{friend_2}, ..., \code{friend_12}, use \code{"_"}.
If no such sign exists in the data, i.e. if network variable names occur as \code{friend1}, \code{friend2}, ...,
\code{friend12}, use \code{NA} or \code{NULL} or \code{""}. In this case, you will have to specify
the \code{nwNcharSeparatorImp} argument.}

\item{nwNcharSeparatorImp}{Integer: only relevant if no \code{nwVarNameSeparatorImp} exists, i.e. if network variables
occur as \code{friend1}, \code{friend2}, ..., \code{friend12}, instead of \code{friend_1}, \code{friend_2}, ...,
\code{friend_12}. \code{nwVcharSeparatorImp} than specifies the number of character signs which should be trimmed to
identify the common variable stem. If \code{nwVarNameSeparatorImp} is not \code{NA} or \code{NULL} or \code{""},
\code{ncharSeparatorImp} will be ignored. For example, if network variables occur as \code{friend_1}, \code{friend_2},
..., \code{friend_12}, use \code{nwVarNameSeparatorImp = "_"}. If network variables occur as \code{friend1}, \code{friend2},
..., \code{friend12}, use \code{nwVarNameSeparatorImp = NULL} and \code{nwNcharSeparatorImp = 6}. The first 6 signs of
variables names (i.e., \code{"friend"}) will be used to identify the group.}

\item{nwLastOccurrence}{Logical: If \code{nwVarNameSeparatorImp} occurrs multiple times within a string, \code{nwLastOccurrence}
defines whether the last occurrence should be used for splitting}

\item{verbose}{Should scale identification be reported?}
}
\value{
Returns a \code{data.frame} with variable information with following columns
\itemize{
 \item \code{varName} The name of the variable as it occurs in the data
 \item \code{varLabel} The label of the variable as it occurs in the \code{GADSdat} label sheet
 \item \code{format} The variable format as displayed in the labels sheet of the \code{GADSdat} object
 \item \code{imp} Logical: Whether or not the variable is imputed
 \item \code{type} The type of the variable. Two possible entries, \code{variable} or \code{scale}
 \item \code{scale} The scale level of the variable. Possible entries: \code{nominal}, \code{ordinal}, \code{numeric}. ID variables and character variables have missing entries in this column. Be cautious that 'ordinal' sometimes may be allocated erroneously. The resulting table should be exported to Excel for further checks.
 \item \code{group} If the variable is part of a scale with several items, a common entry in the group column indicates that these variables belong together
}
}
\description{
Create a variable information data.frame from the GADSdat object. This input can be used
to calculate the descriptives of the data via the \code{calculateDescriptives} function.
See the details section for further information.
}
\details{
The \code{eatCodebook} package aims to create a human-readable pdf codebook from a \code{GADSdat} data base object. The codebook
contains information about the variables used in the study, including their descriptive properties. Which descriptive properties are reported
in the codebook depends, among other things, on the scale level of the variables. For example, the mean and standard deviation are reported
for metric variables and frequency distributions for categorical variables. For non-imputed variables, the proportion of missing values is
also given, and for scale variables, the number of items that make up the scale and the internal consistency of the scale are given. The
codebook is created in several steps. In the first step, the \code{createInputForDescriptives} function is used to generate an auxiliary
object from the database that contains information on what type of descriptive information is to be reported for which variable. The object
created in this function is the basis for the \code{calculateDescriptives} function.
}
\examples{
varInfo <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")

}
