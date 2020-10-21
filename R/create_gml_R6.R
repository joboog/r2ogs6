#This script contains a lot of R6 classes describing various .gml elements.

#You can't build a complete .gml file from this (yet?)!!! Refer to the functions in create_gml instead.
#The aforementioned functions are also way more practical to create a .gml file.
#This script was really more of an exercise for getting used to handling XML data and R6 classes.

#============================== Class export functionality ================================

#' Creates a .gml XML file based on an GML object
#' @param gml_object The specified GML object (of class GMLObject)
#' @param file_name The name of the file the XML will be written to
#' @return WIP
# @examples
# create_gml_from_obj(GMLPrefabCube$new(), "cube_1x1x1_new.gml")
# create_gml_from_obj(GMLPrefabSquare$new(), "square_1x1_new.gml")
create_gml_from_obj <- function(gml_object, file_name) {
    doc <- xml2::as_xml_document(gml_object$as_node())
    xml2::write_xml(doc, file_name, options = "format", encoding="ISO-8859-1")
}


#============================== OpenGeoSysGLI wrapper class ================================

#' Class representing a GML object
#'
#' @param input_file_name An XML (.xml or, in the case of OpenGeoSys, .prj) file
#' @return The encoding of the input file
#'Class describing an entire GML object with a reference file name, points, polylines and surfaces
GMLObject <- R6::R6Class("GMLObject",
                         public = list(
                             initialize = function(name = NULL, points = NULL, polylines = NULL, surfaces = NULL,
                                                   xmlns_xsi = "http://www.w3.org/2001/XMLSchema-instance",
                                                   xmlns_ogs = "http://www.opengeosys.org") {
                                 stopifnot(is.character(xmlns_xsi), length(xmlns_xsi) == 1)
                                 stopifnot(is.character(xmlns_ogs), length(xmlns_ogs) == 1)
                                 stopifnot(is.character(name), length(name) == 1)
                                 stopifnot(checkmate::checkList(points, types = "GMLPoints"), TRUE)
                                 stopifnot(checkmate::checkList(polylines, types = "GMLPolylines"), TRUE)
                                 stopifnot(checkmate::checkList(surfaces, types = "GMLSurfaces"), TRUE)

                                 #(add more checks here)

                                 private$.xmlns_xsi <- xmlns_xsi
                                 private$.xmlns_ogs <- xmlns_ogs
                                 private$.name <- name
                                 private$.points <- points
                                 private$.polylines <- polylines
                                 private$.surfaces <- surfaces
                             },

                             as_node = function(...) {
                                 return(list(OpenGeoSysGLI = structure(list(c(list(name = private$.name),
                                                                              private$.points$as_node(),
                                                                              private$.polylines$as_node(),
                                                                              private$.surfaces$as_node())),
                                                                       "xmlns:xsi" = private$.xmlns_xsi,
                                                                       "xmlns:ogs" = private$.xmlns_ogs)))
                             }
                         ),

                         active = list(
                             xmlns_xsi = function(value) {
                                 if (missing(value)) {
                                     private$.xmlns_xsi
                                 } else {
                                     stop("`$xmlns_xsi` is read only", call. = FALSE)
                                 }
                             },

                             xmlns_ogs = function(value) {
                                 if (missing(value)) {
                                     private$.xmlns_ogs
                                 } else {
                                     stop("`$xmlns_ogs` is read only", call. = FALSE)
                                 }
                             },

                             name = function(value) {
                                 if (missing(value)) {
                                     private$.name
                                 } else {
                                     stop("`$name` is read only", call. = FALSE)
                                 }
                             },

                             points = function(value) {
                                 if (missing(value)) {
                                     private$.points
                                 } else {
                                     stop("`$points` is read only", call. = FALSE)
                                 }
                             },

                             polylines = function(value) {
                                 if (missing(value)) {
                                     private$.polylines
                                 } else {
                                     stop("`$polylines` is read only", call. = FALSE)
                                 }
                             },

                             surfaces = function(value) {
                                 if (missing(value)) {
                                     private$.surfaces
                                 } else {
                                     stop("`$surfaces` is read only", call. = FALSE)
                                 }
                             }
                         ),

                         private = list(
                             .xmlns_xsi = NULL,
                             .xmlns_ogs = NULL,
                             .name = NULL,
                             .points = NULL,
                             .polylines = NULL,
                             .surfaces = NULL
                         )
)


#============================== Point & Points ================================


#'Class describing a single point
#'
GMLPoint <- R6::R6Class("GMLPoint",
                        public = list(

                            initialize = function(id = 0, coordinates = c(0, 0, 0), is_origin = FALSE) {
                                stopifnot(is.numeric(id), length(id) == 1)
                                stopifnot(is.numeric(coordinates), length(coordinates) > 0)
                                stopifnot(is.logical(is_origin), length(is_origin) == 1)

                                private$.id <- id
                                private$.coordinates <- coordinates
                                private$.dim <- length(private$.coordinates)
                                private$.is_origin <- is_origin
                            },

                            as_list = function(...) {
                                point_list <- list(id = private$.id,
                                                   x = private$.coordinates[1], y = private$.coordinates[2], z = private$.coordinates[3])

                                if(private$.is_origin){
                                    point_list <- c(point_list, name = "origin")
                                }

                                return(point_list)
                            },

                            as_node = function(...){
                                if(private$.is_origin){
                                    return(list(point = structure(list(), id = private$.id,
                                                                  x = private$.coordinates[1], y = private$.coordinates[2], z = private$.coordinates[3],
                                                                  name = "origin")))
                                }else{
                                    return(list(point = structure(list(), id = private$.id,
                                                                  x = private$.coordinates[1], y = private$.coordinates[2], z = private$.coordinates[3])))
                                }
                            },

                            print = function(...) {
                                cat("GMLPoint: \n")
                                cat("  ID: ", private$.id, "\n", sep = "")
                                cat("  Coordinates:  ", private$.coordinates,"\n", sep = "\t")
                                cat("  Dimension:  ", private$.dim, "\n", sep = "")
                                cat("  Is origin:  ", private$.is_origin, "\n", sep = "")

                                invisible(self)
                            }
                        ),

                        active = list(
                            id = function(value) {
                                if (missing(value)) {
                                    private$.id
                                } else {
                                    stop("`$id` is read only", call. = FALSE)
                                }
                            },

                            coordinates = function(value) {
                                if (missing(value)) {
                                    private$.coordinates
                                } else {
                                    stop("`$coordinates` is read only", call. = FALSE)
                                }
                            },

                            dim = function(value) {
                                if (missing(value)) {
                                    private$.dim
                                } else {
                                    stop("`$dim` is read only", call. = FALSE)
                                }
                            },

                            is_origin = function(value) {
                                if (missing(value)) {
                                    private$.is_origin
                                } else {
                                    stop("`$is_origin` is read only", call. = FALSE)
                                }
                            }
                        ),

                        private = list(
                            .id = NULL,
                            .coordinates = NULL,
                            .dim = NULL,
                            .is_origin = NULL
                        )
)


#'Class describing a set of points
#'
GMLPoints <- R6::R6Class("GMLPoints",
                         public = list(
                             initialize = function(gml_points) {
                                 stopifnot(checkmate::checkList(gml_points, types = "GMLPoint"), TRUE)
                                 private$.ids <- self$validate_ids(gml_points)
                                 self$validate_dim(gml_points)
                                 self$validate_coordinates(gml_points)

                                 private$.gml_points <- gml_points
                             },

                             validate_ids = function(gml_points){
                                 ids = vector()

                                 for(i in 1:length(gml_points)){
                                     if(is.element(gml_points[[i]]$id, ids)){
                                         stop("Duplicate point IDs detected", call. = FALSE)
                                     }else{
                                         ids <- c(ids, gml_points[[i]]$id)
                                     }
                                 }
                                 return(ids)
                             },

                             validate_dim = function(gml_points) {
                                 for(i in 2:length(gml_points)){
                                     if(gml_points[[1]]$dim != gml_points[[i]]$dim){
                                         stop("Point dimensions not eqal to each other", call. = FALSE)
                                     }
                                 }
                             },

                             validate_coordinates = function(gml_points) {
                                 for(i in 1:(length(gml_points)-1)){
                                     for(j in (i+1):length(gml_points)){
                                         if(identical(gml_points[[i]]$coordinates, gml_points[[j]]$coordinates)){
                                             stop("Overlapping points (with the same coordinates) detected", call. = FALSE)
                                         }
                                     }
                                 }
                             },

                             as_list = function(...) {
                                 points_list <- list()

                                 for(i in 1:length(private$.gml_points)){
                                     points_list <- c(points_list, list(point=private$.gml_points[[i]]$as_list()))
                                 }

                                 return(points_list)
                             },

                             as_node = function(...) {
                                 points_node <- list(points = list())

                                 for(i in 1:length(private$.gml_points)){
                                     points_node[[1]] <- c(points_node[[1]], private$.gml_points[[i]]$as_node())
                                 }

                                 return(points_node)
                             },

                             print = function(...) {
                                 cat("GMLPoints: \n")
                                 cat("", "id", "x", "y", "z", "name", "\n", sep = "\t")
                                 for(i in 1:length(private$.gml_points)) {
                                     point_list <- private$.gml_points[[i]]$as_list()
                                     cat("", unlist(point_list),  "\n", sep = "\t")
                                 }
                                 invisible(self)
                             }

                         ),

                         active = list(
                             ids = function(value) {
                                 if (missing(value)) {
                                     private$.ids
                                 } else {
                                     stop("`$ids` is read only", call. = FALSE)
                                 }
                             },

                             gml_points = function(value) {
                                 if (missing(value)) {
                                     private$.gml_points
                                 } else {
                                     stop("`$gml_points` is read only", call. = FALSE)
                                 }
                             }
                         ),

                         private = list(
                             .ids = NULL,
                             .gml_points = NULL
                         )
)


#============================== Polyline & Polylines ================================


#'Class describing a single polyline
GMLPolyline <- R6::R6Class("GMLPolyline",
                           public = list(

                               initialize = function(id = 0, name = "front_left", point1 = 0, point2 = 1) {
                                   stopifnot(is.numeric(id), length(id) == 1)
                                   stopifnot(is.character(name), length(name) == 1)
                                   stopifnot(is.numeric(point1), length(point1) == 1)
                                   stopifnot(is.numeric(point2), length(point2) == 1)
                                   stopifnot(point1 != point2)

                                   private$.id <- id
                                   private$.name <- name
                                   private$.point1 <- point1
                                   private$.point2 <- point2
                               },

                               as_node = function(...) {
                                   return(list(polyline = structure(list(pnt = list(private$.point1), pnt = list(private$.point2)),
                                                                    id = private$.id, name = private$.name)))
                               },

                               print = function(...) {
                                   cat("GMLPolyline: \n")
                                   cat("  ID: ", private$.id, "\n", sep = "")
                                   cat("  Name:  ", private$.name,"\n",sep = "")
                                   cat("  Point 1:  ", private$.point1, "\n", sep = "")
                                   cat("  Point 2:  ", private$.point2, "\n", sep = "")
                                   invisible(self)
                               }
                           ),

                           active = list(
                               id = function(value) {
                                   if (missing(value)) {
                                       private$.id
                                   } else {
                                       stop("`$id` is read only", call. = FALSE)
                                   }
                               },

                               name = function(value) {
                                   if (missing(value)) {
                                       private$.name
                                   } else {
                                       stop("`$name` is read only", call. = FALSE)
                                   }
                               },

                               point1 = function(value) {
                                   if (missing(value)) {
                                       private$.point1
                                   } else {
                                       stop("`$point1` is read only", call. = FALSE)
                                   }
                               },

                               point2 = function(value) {
                                   if (missing(value)) {
                                       private$.point2
                                   } else {
                                       stop("`$point2` is read only", call. = FALSE)
                                   }
                               }
                           ),

                           private = list(
                               .id = NULL,
                               .name = NULL,
                               .point1 = NULL,
                               .point2 = NULL
                           )
)


#'Class describing a set of polylines
#'
GMLPolylines <- R6::R6Class("GMLPolylines",
                            public = list(
                                initialize = function(gml_polylines) {
                                    stopifnot(checkmate::checkList(gml_polylines, types = "GMLPolyline"), TRUE)
                                    self$validate_polylines(gml_polylines)

                                    private$.ids <- self$validate_ids(gml_polylines)
                                    private$.gml_polylines <- gml_polylines
                                },

                                validate_ids = function(gml_polylines){
                                    ids = vector()

                                    for(i in 1:length(gml_polylines)){
                                        if(is.element(gml_polylines[[i]]$id, ids)){
                                            stop("Duplicate polyline IDs detected", call. = FALSE)
                                        }else{
                                            ids <- c(ids, gml_polylines[[i]]$id)
                                        }
                                    }
                                    return(ids)
                                },

                                validate_polylines = function(gml_polylines) {
                                    for(i in 1:(length(gml_polylines)-1)){
                                        for(j in (i+1):length(gml_polylines)) {
                                            if((gml_polylines[[i]]$point1 == gml_polylines[[j]]$point1 &&
                                                gml_polylines[[i]]$point2 == gml_polylines[[j]]$point2) ||
                                               (gml_polylines[[i]]$point1 == gml_polylines[[j]]$point2 &&
                                                gml_polylines[[i]]$point2 == gml_polylines[[j]]$point1)
                                            ){
                                                stop("Overlapping polylines detected", call. = FALSE)
                                            }
                                        }
                                    }
                                },

                                as_node = function(...){
                                    polylines_node <- list(polylines = list())

                                    for(i in 1:length(private$.gml_polylines)){
                                        polylines_node[[1]] <- c(polylines_node[[1]], private$.gml_polylines[[i]]$as_node())
                                    }

                                    return(polylines_node)
                                }
                            ),

                            active = list(
                                ids = function(value) {
                                    if (missing(value)) {
                                        private$.ids
                                    } else {
                                        stop("`$ids` is read only", call. = FALSE)
                                    }
                                },

                                gml_polylines = function(value) {
                                    if (missing(value)) {
                                        private$.gml_polylines
                                    } else {
                                        stop("`$gml_polylines` is read only", call. = FALSE)
                                    }
                                }
                            ),

                            private = list(
                                .ids = NULL,
                                .gml_polylines = NULL
                            )
)


#============================== Surface & Surfaces ================================


#'Class describing a single surface element (using 3 point IDs)
GMLSurfaceElement <- R6::R6Class("GMLSurfaceElement",
                                 public = list(
                                     initialize = function(p1 = 0, p2 = 1, p3 = 2) {
                                         stopifnot(is.numeric(p1), length(p1) == 1)
                                         stopifnot(is.numeric(p2), length(p2) == 1)
                                         stopifnot(is.numeric(p3), length(p3) == 1)
                                         stopifnot(p1 != p2, p1 != p3, p2 != p3)

                                         private$.p1 <- p1
                                         private$.p2 <- p2
                                         private$.p3 <- p3
                                     },

                                     as_list = function(...) {
                                         return(list(p1 = private$.p1, p2 = private$.p2, p3 = private$.p3))
                                     },

                                     as_node = function(...){
                                         return(list(element = structure(list(), p1 = private$.p1, p2 = private$.p2, p3 = private$.p3)))
                                     }
                                 ),

                                 active = list(
                                     p1 = function(value) {
                                         if (missing(value)) {
                                             private$.p1
                                         } else {
                                             stop("`$p1` is read only", call. = FALSE)
                                         }
                                     },

                                     p2 = function(value) {
                                         if (missing(value)) {
                                             private$.p2
                                         } else {
                                             stop("`$p2` is read only", call. = FALSE)
                                         }
                                     },

                                     p3 = function(value) {
                                         if (missing(value)) {
                                             private$.p3
                                         } else {
                                             stop("`$p3` is read only", call. = FALSE)
                                         }
                                     }
                                 ),

                                 private = list(
                                     .p1 = NULL,
                                     .p2 = NULL,
                                     .p3 = NULL
                                 )
)


#'Class describing a single surface (using 2 GMLSurfaceElements)
GMLSurface <- R6::R6Class("GMLSurface",
                          public = list(

                              initialize = function(id = 0, name = "left",
                                                    gml_surface_element1 = GMLSurfaceElement$new(),
                                                    gml_surface_element2 = GMLSurfaceElement$new(p3 = 3)) {
                                  stopifnot(is.numeric(id), length(id) == 1)
                                  stopifnot(is.character(name), length(name) == 1)

                                  #check type of surface elements??

                                  self$validate_elements(gml_surface_element1, gml_surface_element2)

                                  private$.id <- id
                                  private$.name <- name
                                  private$.element1 <- gml_surface_element1
                                  private$.element2 <- gml_surface_element2
                              },

                              validate_elements = function (gml_surface_element1, gml_surface_element2) {
                                  elem_list1 <- gml_surface_element1$as_list()
                                  elem_list2 <- gml_surface_element2$as_list()
                                  equal_count <- 0

                                  for(i in 1:length(elem_list1)) {
                                      for(j in 1:length(elem_list2)) {
                                          if(elem_list1[[i]] == elem_list2[[j]]) {
                                              equal_count <- equal_count + 1
                                              break
                                          }
                                      }
                                  }

                                  if(equal_count != 2) {
                                      stop("Invalid surface detected", call. = FALSE)
                                  }
                              },

                              as_node = function() {
                                  return(list(surface = structure(c(private$.element1$as_node(), private$.element2$as_node()),
                                                                  id = private$.id, name = private$.name)))
                              }
                          ),

                          active = list(
                              id = function(value) {
                                  if (missing(value)) {
                                      private$.id
                                  } else {
                                      stop("`id` is read only", call. = FALSE)
                                  }
                              },

                              name = function(value) {
                                  if (missing(value)) {
                                      private$.name
                                  } else {
                                      stop("`name` is read only", call. = FALSE)
                                  }
                              },

                              element1 = function(value) {
                                  if (missing(value)) {
                                      private$.element1
                                  } else {
                                      stop("`element1` is read only", call. = FALSE)
                                  }
                              },

                              element2 = function(value) {
                                  if (missing(value)) {
                                      private$.element2
                                  } else {
                                      stop("`$element2` is read only", call. = FALSE)
                                  }
                              }
                          ),

                          private = list(
                              .id = NULL,
                              .name = NULL,
                              .element1 = NULL,
                              .element2 = NULL
                          )
)


#'Class describing a set of surfaces
GMLSurfaces <- R6::R6Class("GMLSurfaces",
                           public = list(

                               initialize = function(gml_surfaces) {
                                   stopifnot(checkmate::checkList(gml_surfaces, types = "GMLSurface"), TRUE)
                                   private$.ids <- self$validate_ids(gml_surfaces)
                                   #validation of surfaces is a bit more difficult, there is no method for it yet!

                                   private$.gml_surfaces <- gml_surfaces
                               },

                               validate_ids = function(gml_surfaces){
                                   ids = vector()

                                   for(i in 1:length(gml_surfaces)){
                                       if(is.element(gml_surfaces[[i]]$id, ids)){
                                           stop("Duplicate surface IDs detected", call. = FALSE)
                                       }else{
                                           ids <- c(ids, gml_surfaces[[i]]$id)
                                       }
                                   }
                                   return(ids)
                               },

                               as_node = function(...){
                                   surfaces_node <- list(surfaces = list())

                                   for(i in 1:length(private$.gml_surfaces)){
                                       surfaces_node[[1]] <- c(surfaces_node[[1]], private$.gml_surfaces[[i]]$as_node())
                                   }

                                   return(surfaces_node)
                               }
                           ),

                           active = list(
                               ids = function(value) {
                                   if (missing(value)) {
                                       private$.ids
                                   } else {
                                       stop("`ids` is read only", call. = FALSE)
                                   }
                               },

                               gml_surfaces = function(value) {
                                   if (missing(value)) {
                                       private$.gml_surfaces
                                   } else {
                                       stop("`gml_surfaces` is read only", call. = FALSE)
                                   }
                               }
                           ),

                           private = list(
                               .ids = NULL,
                               .gml_surfaces = NULL
                           )
)

#============================== Shared functions ================================

#Helper function to check for the existence of identical objects in a list
# @param object_list The list to check for duplicates
# @param object_name Which objects the list consists of (for better error messages)
# @param warn_only Should function throw error or only a warning?
# @examples
# check_for_duplicates(list(3, 0, 4), "point IDs")
# check_for_duplicates(list(0, 0), "point IDs")
# check_for_duplicates(list("no", "this", "is", "patrick"), "point names")
# check_for_duplicates(list("no", "this", "is", "patrick", "patrick"), "point names", TRUE)
check_for_duplicates <- function(object_list, object_name = NA, warn_only = FALSE) {
    for(i in 1:(length(object_list)-1)) {
        for(j in (i+1):length(object_list)) {
            if(object_list[[i]] == object_list[[j]]) {
                if(warn_only){
                    warning(c("Duplicate ", object_name, " detected"), call. = FALSE)
                }else{
                    stop(c("Duplicate ", object_name, " detected"), call. = FALSE)
                }
            }
        }
    }
}