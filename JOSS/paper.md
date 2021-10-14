---
title: 'r2ogs6: An interface to the OpenGeoSys 6 Multiphysics Simulator'
tags:
  - R
  - OpenGeoSys
  - finite element method
  - physics based modeling
  - groundwater flow modeling
authors:
  - name: Ruben Heinrich^[co-first author]
    affiliation: 1
  - name: Johannes Boog^[co-first author]
    orcid: 0000-0003-0872-7098
    affiliation: 2
  - name: Thomas Kalbacher^[co-first author]
    orcid: 0000-0002-7866-5702
    affiliation: 2
affiliations:
 - name: Leipzig University of Applied Sciences, Karl-Liebknecht-Strasse 132, 04277 Leipzig, Germany
   index: 1
 - name: Helmholtz Centre for Environmental Research, Department Environmental Informatics, Permoser Str. 15, 04318 Leipzig, Germany
   index: 2
date: 04 October 2021
bibliography: paper.bib

---

# Summary

<!--etwas Einleitung ist doch noetig. Ich weiss, ich hatte das vorher in den saechsten Abschnitt verschobe. Asche auf mein Haupt.-->
Understanding the effects of climate change and extreme events on our environmental systems, especially the subsurface, is of utmost importance and requires key tools such as environmental and geoscientific physics--based simulation models.
Our `R` package `r2ogs6` provides a file--based interface to the multi--physics simulation code `OpenGeoSys 6` [@Kolditz2012; @Bilke2019] and, therefore, allows `R` users to perform and analyze environmental and geoscientific simulations in `R`.
<!--`OpenGeoSys 6` itself is a scientific open source project for the development of numerical methods to simulate (coupled) thermo-hydro-mechanical-chemical and biological (THMC/B) processes in porous and fractured media [@ogs].-->
`r2ogs6` allows to access the capabilities of `OpenGeoSys 6` to simulate thermo-hydro-mechanical-chemical and biological (THMC/B) processes in porous and fractured media within `R`.
In this way, `r2ogs6` enables `R` users to model sub--surface phenomena and technologies such as groundwater flow, reactive transport, geothermal energy usage and/or nuclear waste repositories as well as to analyze and further process simulation output with the power of a high--level data science language.
`r2ogs6` enables users to prepare and manipulate `OpenGeoSys 6` simulation models, run the simulations and retrieve corresponding output, all within an `R` session.
Therefore, `R` classes and functions were designed to communicate with the respective `OpenGeoSys 6` input and output files as well as executables.
In addition to single-simulation runs, `r2ogs6` supports ensemble runs that can be used to set up uncertainty and sensitivity analyses as well as parameter studies.
It allows conducting and documenting `OpenGeoSys 6` simulations in reproducible `R` scripts or notebooks.
As `OpenGeoSys 6` is continuously being developed further, code generation functions for `r2ogs6` developers were included to speed up the package updating process in case of future changes to `OpenGeoSys 6`.

`r2ogs6` aims to bridge the gap between data produced by a scientific simulation code and data science, and, provides `R` users with an opportunity to model environmental sub--surface systems.
Besides increasing the usability of `OpenGeoSys 6`, `r2ogs6` also improves the reproducibility of research results.



# Statement of need

Major challenges humanity has to face in the coming decades are climate change and environmental extremes.
Understanding their effects on our environmental systems, especially the sub--surface is, therefore, of utmost importance.
Key tools to develop this understanding are environmental and geoscientific physics--based simulation models that describe the manifold interacting natural phenomena across time and space.
The multiple coupled natural processes implemented in environmental and geoscientific physics--based simulation models are usually described with partial differential equations.
Solving these equations requires appropriate numerical methods such as the finite element method (FEM).
Implementations of FEM have become integral tools in a lot of research areas today [@Steefel2015].
One of these tools is `OpenGeoSys 6`, a scientific open source project for the development of numerical methods to simulate thermo-hydro-mechanical-chemical and biological (THMC/B) processes in porous and fractured media [@Kolditz2012; @Bilke2019].
But while `OpenGeoSys 6` is a powerful FEM code, setting up, running and evaluating simulations can prove complicated.
Especially the set up of simulation ensembles as well as the calibration of simulation models can be a tedious process.

Here is where high--level languages such as `R` and `Python` can prove useful.
Via an interface that adds a layer on top of `OpenGeoSys 6`, the user can access preprocessing tools, the solver itself and postprocessing tools alike, thus increasing usability and accessibility.
The developement and application of user interfaces from geoscientific simulators to high--level programming languages has been gaining increasing attention in recent years; examples are `FloPy` [@Bakker2016], `ogs5py` [@Mueller2021], `RedModRPhree`[@DeLucia2021],  `r2ogs5` [@Schad2021] and `toughio` [@Luu2020].
For `OpenGeoSys 6`, a `Python` interface is currently under development [@Buchwald2021]. 
Nonetheless, we consider an `R` interface to be just as important, as `R` is a well known language in the environmental and geosciences.
Furthermore, since `R` is a popular language in the field of data science with many powerful packages for data analysis and visualisation, e. g. `dplyr` [@r-dplyr] and `ggplot2` [@r-ggplot2], it's a natural choice for processing data generated by simulation tools such as `OpenGeoSys 6`.
Especially, `r2ogs6` can facilitate the calibration of `OpenGeoSys 6` models due to the implemented functions to design ensemble runs as well the available `R` functions and packages for modeling such as  `lhs` [@lhs], `mlrBO` [@mlrMBO].
For `R` users who do not have a lot of (or any) experience with, yet an interest in environmental and geoscientific sub--surface simulations, `r2ogs6` provides a good starting point.
Utilizing `r2ogs6`, users can easily set up their first `OpenGeoSys 6` simulations by choosing one of numerous provided benchmark files.
Moreover, with `R` scripts and `R--Markdown` or `JupyteR` notebooks, modeling workflows can easily be documented, published and shared with peers.

`r2ogs6` was designed to be used by domain researchers, data scientists and students working with `OpenGeoSys 6`.
It has already been applied in [@heinrich:2021], where its ensemble functionality was tested utilizing the `OpenGeoSys 6` Theis' problem benchmark files [@benchmark-theis-problem; @benchmark-theis-solution].
In an ongoing research project, `r2ogs6` is used to enable the calibration of large-scale groundwater flow models [@Boog2021].


# Package Structure

`r2ogs6` is thought to set up an `OpenGeoSys 6` simulation inside an `R` session by executing specified model creation functions or by reading existing `OpenGeoSys 6` input files.
With further functions, the simulation can be executed and corresponding output can be read into the `R` session again.
\autoref{fig:structure} highlights the structure of `r2ogs6`.
The central element that represents an `OpenGeoSys 6` simulation is the `OGS6` object, which is an instance of a `R6` class.
This object represents a single simulation; multiple simulations can be defined with the `OGS6_Ensemble` class.
An `OGS6` object contains several child objects that represent the simulation input and output.
The main `OpenGeoSys 6` input files are the project file `*.prj`, geometry file `*.gml` and input FEM mesh file(s) `*.vtu`.
These are read in or written via `S3` class based functions (block `read_in* / export*` in  \autoref{fig:structure}).
When reading in, the XML--based `*.prj` input file is parsed. Individual tags are represented as `S3` class objects which are available via active fields in the `OGS6` object.
<!--`S3` classes for the `*.prj` file tags were preferred over one entire `R6 class` for reasons of simplicity.-->
Individual `*.prj` tags may change due to ongoing development activities in `OpenGeoSys 6`, therefore, future updates of the related classes may be necessary.
To simplify updates like this, helper functions for analyzing `*.prj` files as well as suggesting and creating classes were implemented.

As the `*.gml` and the `*.vtu` files are less complex and less likely to change, these files are represented as `R6` class objects and also available as active fields inside the `OGS6` object.
To execute simulations, functions for writing the `OpenGeoSys 6` input (`ogs6_export_simfiles()`) and call the `OpenGeoSys 6` executable (`ogs6_run_simulation()`) were implemented.
Note that an `OpenGeoSys 6` executable or singularity container needs to be present.
Default executables and paths can be defined in a configuration file.

During execution `OpenGeoSys 6` generates output data as `*.vtu` files.
These files are produced at user defined timesteps of the simulation and are referenced in a `*.pvd` file.
The function `ogs6_read_output_files()` then attaches the output files to the `OGS6` object as `OGS6_pvd` objects (which in turn reference `OGS6_vtu` objects).
In this way, all data required for and produced by `OpenGeoSys 6` can be represented as `R` native objects and results can be processed further using `R` functions.

![Schematic of the `r2ogs6` structure.\label{fig:structure}](r2ogs6_structure_schematic.png)

The package comes with tutorials demonstrating how to set up and run a single simulation ([link](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/blob/master/vignettes/user_workflow_vignette.Rmd)), set up simulation ensembles ([link](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/blob/master/vignettes/ensemble_workflow_vignette.Rmd)) and further develop the package ([link](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/blob/master/vignettes/dev_workflow_vignette.Rmd)).
Furthermore, a seperate repository provides `r2ogs6` scripts to set up `OpenGeoSys 6` benchmarks ([link](https://gitlab.opengeosys.org/ogs/tools/r2ogs6_benchmarks)).


# Acknowledgements

Johannes Boog acknowledges the Helmholtz Organization for funding within the context of the project *Digital Earth* (Ref. XXX).
Furthermore, Johannes Boog and Thomas Kalbacher acknowledge the Helmholtz Centre for Environmental Research--UFZ for additional funding and support.
We would like to express our gratitude to the *OpenGeoSys Community* for technical support and for hosting the GitLab server for our development.
Furthermore we also like to thank Philipp Schad for additional technical support.


# References
