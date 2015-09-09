#' Simplifies a polygon by removing every n vertices
#' @param sp_obj
#' @param every
#' @export
remove_coords <- function(sp_obj, every) {
    
    exist_pts <- coordinates(as(sp_obj, "SpatialLines"))
    
    create_Polygon <- function(coords, every) {
        coords[[1]] <- matrix(coords[[1]], ncol = 2)
        coords[[1]] <- coords[[1]][1:nrow(coords[[1]])%%every > 0, ]
        coords[[1]] <- rbind(coords[[1]], coords[[1]][1, ])
        return(Polygon(coords[[1]]))
    }
    
    create_Polygons <- function(polygon) {
        Srl_list <- list()
        for (i in 1:length(polygon)) {
            Srl_list[[i]] <- Polygons(list(polygon[[i]]), ID = as.character(i))
        }
        return(Srl_list)
    }
    
    reduced_Polygon <- lapply(exist_pts, function(x) create_Polygon(x, every))
    reduced_Polygons <- create_Polygons(reduced_Polygon)
    reduced_SpatialPolygons <- SpatialPolygons(reduced_Polygons, proj4string = CRS(proj4string(sp_obj)))
    return(reduced_SpatialPolygons)
} 
