% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prj_chemical_system.R
\name{prj_solution}
\alias{prj_solution}
\title{prj_solution}
\usage{
prj_solution(
  temperature,
  pressure,
  pe,
  components,
  charge_balance = NULL,
  fixing_pe = NULL
)
}
\arguments{
\item{temperature}{string | double: Temperature}

\item{pressure}{string | double: Pressure}

\item{pe}{string | double: pe}

\item{components}{list: Components}

\item{charge_balance}{Optional: string: Charge balance}

\item{fixing_pe}{Optional: string}
}
\description{
tag: solution
}
\examples{
prj_solution(
    temperature = 25,
    pressure = 1,
    pe = 4,
    components = list(
        component = "C(4)",
        component = "Ca"
    ),
    charge_balance = "pH"
)
}
