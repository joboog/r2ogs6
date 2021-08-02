---
title: 'r2ogs6: An OpenGeoSys 6 interface'
tags:
  - R
  - OpenGeoSys
  - finite element method
  - FEM
authors:
  - name: Ruben Heinrich^[co-first author]
    affiliation: 1
  - name: Johannes Boog^[co-first author]
    orcid: 0000-0003-0872-7098
    affiliation: 2
affiliations:
 - name: Hochschule für Technik, Wirtschaft und Kultur Leipzig
   index: 1
 - name: Helmholtz-Zentrum für Umweltforschung
   index: 2
date: 02 August 2021
bibliography: paper.bib

---

# Summary

Implementations of the finite element method (FEM) have become integral tools in a lot of research areas today [@rtc-steefel]. One of these programs is `OpenGeoSys 6`, a scientific open source project for the development of numerical methods for the simulation of thermo-hydro-mechanical-chemical (THMC) processes in porous and fractured media [@ogs]. But while `OGS6` is a powerful FEM code, setting up, running and evaluating simulations with it can prove complicated. Besides the numerical solver itself, external pre- and postprocessing tools are needed to create the finite element mesh(es), define all necessary simulation parameters and read in the output files. With a workflow like this, research results can't be reproduced without knowledge of multiple programs.

Here is where high-level languages like `R` or `Python` can prove useful. Via an interface that adds a layer on top of the FEM code, the user can access preprocessing tools, the solver itself and postprocessing tools alike, thus increasing usability.


# Statement of need

`r2ogs6` is an R package that acts as an interface for the finite element code `OpenGeoSys 6`. It contains functions that increase the usability of `OGS6` in different areas, the first of which being preprocessing. Simulation parameters can be defined in `R` by either reading in existing benchmark files or manually, using dedicated class objects. After all parameters have been defined, simulations can be started from `R` without having to switch to the command line. Finally, the output files can be read in to analyse the results.

In addition to single-simulation runs, `r2ogs6` supports ensemble runs that can be used to set up uncertainty and sensitivity analyses as well as parameter studies. It can also generate `R` scripts from existing `OpenGeoSys 6` benchmark files and includes some (non-exported) code generation functions for developers to speed up the updating process of the package. As an interface, `r2ogs6` naturally relies on `OpenGeoSys 6` to work. Additionally, a `Python` environment including the `numpy` and `vtk` packages is required.

`r2ogs6` was designed to be used by researchers, data scientists and students working with `OpenGeoSys 6`. It has already been applied in [@heinrich:2021], where its ensemble functionality was tested utilizing the `OGS6` Theis' problem benchmark files [@benchmark-theis-problem; @benchmark-theis-solution]. The example simulations were set up using `r2ogs6` as an interface to read in the benchmarks, start the simulations, read in the output files produced by `OGS6` and lastly evaluate the results. By simplifieing the `OpenGeoSys 6` workflow as described, `r2ogs6` aims to bridge the gap between data produced by a scientific code and data science.


# References
