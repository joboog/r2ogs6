#This is where the user defines his data.

#============================== Data for GML file ================================

#Specify some data (source: HydroComponent Benchmarks -> IdealGas -> flow_free_expansion -> cube_1x1x1.gml)

#The name of the desired geometry (will become first XML child under root)
geo_name <- "cube_1x1x1_geometry"

#Some points (will become second XML child under root)
my_gml_points <- tibble::tibble(x = c(0, 0, 0, 0, 1, 1, 1, 1),
                            y = c(0, 0, 1, 1, 0, 0, 1, 1),
                            z = c(0, 1, 1, 0, 0, 1, 1, 0),
                            name = c("origin", rep("", 7)))

#Some polylines (will become third XML child under root)
my_gml_polylines <- list(list(name = "front_left", c(0, 1)),
                     list(name = "front_right", c(4, 5)),
                     list(name = "front_bottom", c(0, 4)),
                     list(name = "front_top", c(1, 5)),
                     list(name = "bottom_left", c(0, 3)),
                     list(name = "bottom_right", c(4, 7)),
                     list(name = "top_left", c(1, 2)),
                     list(name = "top_right", c(5, 6)),
                     list(name = "back_left", c(2, 3)),
                     list(name = "back_right", c(6, 7)),
                     list(name = "back_bottom", c(3, 7)),
                     list(name = "back_top", c(2, 6)))

#Some surfaces (will become fourth XML child under root)
my_gml_surfaces <- list(list(name = "left", c(0, 1, 2), c(0, 3, 2)),
                    list(name = "right", c(4, 6, 5), c(4, 6, 7)),
                    list(name = "top", c(1, 2, 5), c(5, 2, 6)),
                    list(name = "bottom", c(0, 3, 4), c(4, 3, 7)),
                    list(name = "front", c(0, 1, 4), c(4, 1, 5)),
                    list(name = "back", c(2, 3, 6), c(6, 3, 7)))


#============================== Data for VTU file ================================


my_vtk_specification <- list(type="UnstructuredGrid",
                             version="0.1",
                             byte_order="LittleEndian",
                             header_type="UInt32",
                             compressor="vtkZLibDataCompressor")



my_vtk_pieces <- list()




#============================== Execution ================================

#Turn GML data into XML structure
my_gml_xml <- gml_data_to_xml(geo_name, my_gml_points, my_gml_polylines, my_gml_surfaces)


#Let's export that as a new .gml XML file
export_xml_to_file(my_gml_xml, "my_experimental_cube.gml")

#Now the .gml XML file should be in the r2ogs6 folder (not a perfect location but ok for a little test)

#The only thing missing from the new file compared to the original is the stylesheet declaration...
#I haven't found a way to add that one yet, but it should also be possible somehow