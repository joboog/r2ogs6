% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prj_process.R
\name{prj_constitutive_relation}
\alias{prj_constitutive_relation}
\title{prj_constitutive_relation}
\usage{
prj_constitutive_relation(
  type,
  id = NULL,
  youngs_modulus = NULL,
  poissons_ratio = NULL,
  nonlinear_solver = NULL,
  behaviour = NULL,
  material_properties = NULL,
  shear_modulus = NULL,
  bulk_modulus = NULL,
  kappa = NULL,
  beta = NULL,
  gamma = NULL,
  hardening_modulus = NULL,
  alpha = NULL,
  delta = NULL,
  eps = NULL,
  m = NULL,
  alphap = NULL,
  deltap = NULL,
  epsp = NULL,
  mp = NULL,
  betap = NULL,
  gammap = NULL,
  tangent_type = NULL,
  damage_properties = NULL,
  youngs_moduli = NULL,
  shear_moduli = NULL,
  poissons_ratios = NULL,
  a = NULL,
  n = NULL,
  sigma0 = NULL,
  q = NULL,
  kelvin_shear_modulus = NULL,
  kelvin_viscosity = NULL,
  maxwell_shear_modulus = NULL,
  maxwell_bulk_modulus = NULL,
  maxwell_viscosity = NULL,
  dependency_parameter_mk = NULL,
  dependency_parameter_mvk = NULL,
  dependency_parameter_mvm = NULL
)
}
\arguments{
\item{type}{string:}

\item{id}{Optional:}

\item{youngs_modulus}{Optional:}

\item{poissons_ratio}{Optional:}

\item{nonlinear_solver}{Optional:}

\item{behaviour}{Optional:}

\item{material_properties}{Optional:}

\item{shear_modulus}{Optional:}

\item{bulk_modulus}{Optional:}

\item{kappa}{Optional:}

\item{beta}{Optional:}

\item{gamma}{Optional:}

\item{hardening_modulus}{Optional:}

\item{alpha}{Optional:}

\item{delta}{Optional:}

\item{eps}{Optional:}

\item{m}{Optional:}

\item{alphap}{Optional:}

\item{deltap}{Optional:}

\item{epsp}{Optional:}

\item{mp}{Optional:}

\item{betap}{Optional:}

\item{gammap}{Optional:}

\item{tangent_type}{Optional:}

\item{damage_properties}{Optional:}

\item{youngs_moduli}{Optional:}

\item{shear_moduli}{Optional:}

\item{poissons_ratios}{Optional:}

\item{a}{Optional:}

\item{n}{Optional:}

\item{sigma0}{Optional:}

\item{q}{Optional:}

\item{kelvin_shear_modulus}{Optional:}

\item{kelvin_viscosity}{Optional:}

\item{maxwell_shear_modulus}{Optional:}

\item{maxwell_bulk_modulus}{Optional:}

\item{maxwell_viscosity}{Optional:}

\item{dependency_parameter_mk}{Optional:}

\item{dependency_parameter_mvk}{Optional:}

\item{dependency_parameter_mvm}{Optional:}
}
\description{
tag: constitutive_relation
}
\examples{
prj_constitutive_relation(
    type = "LinearElasticIsotropic",
    youngs_modulus = "E",
    poissons_ratio = "nu"
)
}
