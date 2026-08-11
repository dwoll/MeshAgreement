\name{read_mesh_one}
\alias{read_mesh_one}
\title{Read one mesh file}
\description{Read one mesh object from a file.}
\usage{read_mesh_one(x, name, fix_issues=TRUE,
              reconstruct=c("No", "AFS", "SSS", "Poisson", "Ball_Pivoting",
                            "Alpha_Wrap"),
              smooth=c("No", "VCG"),
              remesh=c("No", "Isotropic"),
              ...)}
\arguments{
  \item{x}{\code{character}. File name.}
  \item{name}{\code{character}. Name that should be assigned to the mesh object. If missing, the name is taken from the file name.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections, see \code{\link[MeshUtils]{readMeshFile}}.}
  \item{reconstruct}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[MeshUtils]{reconstructAFS}}. \code{"SSS"} uses \code{\link[MeshUtils]{reconstructSSS}}. \code{"Poisson"} uses \code{\link[MeshUtils]{reconstructPoisson}}. \code{"Ball_Pivoting"} uses \code{\link[Rvcg]{vcgBallPivoting}}. \code{"Alpha_Wrap"} uses \code{\link[MeshUtils]{alphaWrap}}.}
  \item{smooth}{\code{character}. Smoothing method. \code{"VCG"} uses \code{\link[Rvcg]{vcgSmooth}}.}
  \item{remesh}{\code{character}. \code{"Isotropic"} uses \code{\link[Rvcg]{vcgIsotropicRemeshing}}.}
  \item{...}{Options passed to \code{\link[MeshUtils]{reconstructAFS}}, \code{\link[MeshUtils]{reconstructSSS}}, \code{\link[MeshUtils]{reconstructPoisson}}, \code{\link[Rvcg]{vcgBallPivoting}}, \code{\link[MeshUtils]{alphaWrap}}, \code{\link[Rvcg]{vcgSmooth}}, or \code{\link[Rvcg]{vcgIsotropicRemeshing}}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF.}
\value{Mesh object - a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{\link[MeshUtils]{CGALmesh}} object. Use \code{\link[MeshAgreement]{read_mesh}} to read a list of several mesh objects each for several observers. Use \code{\link[MeshAgreement]{read_mesh_obs}} to read a list of several mesh objects each for 1 observer.}
\seealso{
\code{\link[MeshUtils]{readMeshFile}},
\code{\link[MeshUtils]{reconstructAFS}},
\code{\link[MeshUtils]{reconstructSSS}},
\code{\link[MeshUtils]{reconstructPoisson}},
\code{\link[MeshUtils]{alphaWrap}},
\code{\link[Rvcg]{vcgBallPivoting}},
\code{\link[Rvcg]{vcgIsotropicRemeshing}},
\code{\link[Rvcg]{vcgSmooth}},
\code{\link[MeshAgreement]{read_mesh}},
\code{\link[MeshAgreement]{read_mesh_obs}},
}
\examples{
if(interactive()) {
    src_dir <- system.file("extdata", package="MeshAgreement")
    ff      <- list.files(src_dir, pattern="^Obs01", full.names=TRUE)
    mesh    <- read_mesh_one(ff[1], reconstruct="AFS")

    get_mesh_info_one(mesh)
}
}
