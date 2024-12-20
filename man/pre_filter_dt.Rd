% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_filter.R
\name{pre_filter_dt}
\alias{pre_filter_dt}
\title{Pre-Filter Data with data.table}
\usage{
pre_filter_dt(df, df_population, threshold, max.T, var_type_override)
}
\arguments{
\item{df}{A \code{data.table} containing the dataset to filter. Must include the columns
\code{ID}, \code{t}, \code{var}, and optionally \code{value}.}

\item{df_population}{A \code{data.table} containing the population data with a column \code{ID}.
Only rows in \code{df} with IDs in \code{df_population$ID} will be retained.}

\item{threshold}{Numeric. Minimum frequency threshold for variables to be retained,
as a proportion of unique IDs in \code{df_population}.}

\item{max.T}{Numeric. Maximum time value for filtering rows in \code{df}. Rows with \code{t >= max.T}
will be excluded.}

\item{var_type_override}{Named character vector. Indicates the types of variables
(e.g., "categorical", "numerical", "hierarchical"). Variable types are used to identify
categorical or hierarchical variables for special handling.}
}
\value{
A filtered \code{data.table} containing only the rows that satisfy the filtering criteria.
}
\description{
This function filters a dataset based on population inclusion, time constraints,
variable types, and frequency thresholds. It is designed to handle large datasets
efficiently using the \code{data.table} package.
}
\details{
The function performs the following filtering steps:
\itemize{
\item Filters rows in \code{df} to include only IDs present in \code{df_population}.
\item Retains rows with time (\code{t}) values within the range [0, \code{max.T}\).
\item Identifies categorical or hierarchical variables based on \code{var_type_override} and ensures
that numerical variables have no duplicate values for the same \code{ID} and \code{t}.
\item Calculates the frequency of each variable as the proportion of unique IDs and retains
variables with frequencies above the specified \code{threshold}.
}
}
\examples{
library(data.table)

# Sample dataset
df <- data.table(
  ID = c("1", "1", "2", "2", "3", "3", "4", "4", "5"),
  t = c(1, 2, 3, NA, 1, 5, 0, NA, 8),
  var = c("age", "heart_rate", "heart_rate", "sex", "age",
            "heart_rate", "bp_systolic", "sex", "bp_systolic"),
  value = c("55", "72", "80", "male", "60", "90", "120", "female", "130")
)

# Population data
df_population <- data.table(ID = c("1", "2", "3", "4", "5"))

# Variable type override
var_type_override <- c(age = "numerical", heart_rate = "numerical", 
                       bp_systolic = "numerical", sex = "categorical")

# Parameters
max.T <- 6
threshold <- 0.2

# Apply the pre_filter_dt function
filtered_df <- pre_filter_dt(df, df_population, threshold, max.T, var_type_override)
print(filtered_df)
}
