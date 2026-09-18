\name{read_mesh_obs}
\alias{read_mesh_obs}
\title{Read mesh files}
\description{Read a list of mesh objects from one observer from files.}
\usage{read_mesh_obs(x, name, fix_issues=TRUE,
              reconstruct=c("No", "AFS", "SSS", "Poisson", "Ball_Pivoting",
                            "Alpha_Wrap"),
              smooth=c("No", "VCG", "CGAL"),
              remesh=c("No", "Isotropic"),
              ...)}
\arguments{
  \item{x}{\code{character} vector of file names.}
  \item{name}{\code{character} vector of names that should be assigned to the mesh objects. If missing, names are taken from the file names.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections, see \code{\link[SurfaceMesh]{readMeshFile}}.}
  \item{reconstruct}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[SurfaceMesh]{reconstructAFS}}. \code{"SSS"} uses \code{\link[SurfaceMesh]{reconstructSSS}}. \code{"Poisson"} uses \code{\link[SurfaceMesh]{reconstructPoisson}}. \code{"Ball_Pivoting"} uses \code{\link[Rvcg]{vcgBallPivoting}}. \code{"Alpha_Wrap"} uses \code{\link[SurfaceMesh]{alphaWrap}}.}
  \item{smooth}{\code{character}. Smoothing method. \code{"VCG"} uses \code{\link[Rvcg]{vcgSmooth}}, \code{"CGAL"} uses \code{\link[SurfaceMesh]{smoothShape}}.}
  \item{remesh}{\code{character}. \code{"Isotropic"} uses \code{\link[Rvcg]{vcgIsotropicRemeshing}}.}
  \item{...}{Options passed to \code{\link[SurfaceMesh]{reconstructAFS}}, \code{\link[SurfaceMesh]{reconstructSSS}}, \code{\link[SurfaceMesh]{reconstructPoisson}}, \code{\link[Rvcg]{vcgBallPivoting}}, \code{\link[SurfaceMesh]{alphaWrap}}, \code{\link[Rvcg]{vcgSmooth}}, or \code{\link[Rvcg]{vcgIsotropicRemeshing}}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF.}
\value{List of mesh objects. Each object is a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{CGALmesh} object as created by \code{\link[SurfaceMesh]{makeMesh}}. Use \code{\link[MeshAgreement]{read_mesh}} to read a list of several mesh objects each for several observers. Use \code{\link[MeshAgreement]{read_mesh_one}} to read a single mesh file.}
\seealso{
\code{\link[SurfaceMesh]{makeMesh}},
\code{\link[SurfaceMesh]{reconstructAFS}},
\code{\link[SurfaceMesh]{reconstructSSS}},
\code{\link[SurfaceMesh]{reconstructPoisson}},
\code{\link[SurfaceMesh]{alphaWrap}},
\code{\link[SurfaceMesh]{smoothShape}},
\code{\link[Rvcg]{vcgBallPivoting}},
\code{\link[Rvcg]{vcgIsotropicRemeshing}},
\code{\link[Rvcg]{vcgSmooth}},
\code{\link[MeshAgreement]{read_mesh}},
\code{\link[MeshAgreement]{read_mesh_one}},
}
\examples{
if(interactive()) {
    src_dir <- system.file("extdata", package="MeshAgreement")
    ff      <- list.files(src_dir, pattern="^Obs01", full.names=TRUE)
    meshL   <- read_mesh_obs(ff, fillHoles=TRUE, verbose=TRUE)

    get_mesh_info_obs(meshL)
}
}
