
#' @title Check whether object is a Quake 1 or 2 alias model.
#'
#' @param x any R object
#'
#' @export
is.quakemodel <- function(x) {
  return(inherits(x, 'quakemodel_mdl') | inherits(x, 'quakemodel_md2'));
}


#' @title Convert Quake Model to 'fs.surface' instance.
#'
#' @param quakemodel an instance of \code{quakemodel_mdl} or \code{quakemodel_md2}.
#'
#' @param frame_idx integer, the frame to export. Quake models may contain animations made up of several frames. The mesh connectivity is unaltered between frames, but the vertex positions differ.
#'
#' @return \code{fs.surface} mesh instance, as used by the \code{freesurferformats} package.
#'
#' @export
quakemodel.to.fs.surface <- function(quakemodel, frame_idx = 1L) {
  sf = list('faces'=(quakemodel$triangles$vertex + 1L), 'vertices'=quakemodel$frames[[frame_idx]]$vertex_coords);
  class(sf) = c(class(sf), 'fs.surface');
  return(sf);
}


#' #' @title Convert Quake Model to rgl 'tmesh3d' instance.
#' #'
#' #' @param quakemodel an instance of \code{quakemodel_mdl} or \code{quakemodel_md2}.
#' #'
#' #' @param frame_idx integer, the frame to export. Quake models may contain animations made up of several frames. The mesh connectivity is unaltered between frames, but the vertex positions differ.
#' #'
#' #' @return \code{tmesh3d} mesh instance, as used by the \code{rgl} package. You can use \code{rgl::shade3d(your_tmesh3d_instance)} to visualize the model.
#' #'
#' #' @export
#' quakemodel.to.tmesh3d <- function(quakemodel, frame_idx = 1L) {
#'   sf = quakemodel.to.fs.surface(quakemodel, frame_idx = frame_idx);
#'   tm = rgl::tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = FALSE);
#'   return(tm);
#' }
#'

