
# While having r2ogs6 loaded:

prj_reduxml <- system.file("extdata/xml_redux/", "prj_redu.xml",
                           package = "r2ogs6")

build_redux_doc(export_path = prj_reduxml)
