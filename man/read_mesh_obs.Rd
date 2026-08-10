\name{read_mesh_obs}
\alias{read_mesh_obs}
\title{Read mesh files}
\description{Read a list of mesh objects from one observer from files.}
\usage{read_mesh_obs(x, name, fix_issues=TRUE,
              reconstruct=c("No", "AFS", "SSS", "Poisson", "Ball_Pivoting",
                            "Alpha_Wrap"),
              smooth=c("No", "VCG"),
              remesh=c("No", "Isotropic"),
              ...)}
\arguments{
  \item{x}{\code{character} vector of file names.}
  \item{name}{\code{character} vector of names that should be assigned to the mesh objects. If missing, names are taken from the file names.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections. Uses surface reconstruction if mesh is not closed.}
  \item{reconstruct}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[CGALmeshops]{reconstructAFS}}. \code{"SSS"} uses \code{\link[CGALmeshops]{reconstructSSS}}. \code{"Poisson"} uses \code{\link[CGALmeshops]{reconstructPoisson}}. \code{"Ball_Pivoting"} uses \code{\link[Rvcg]{vcgBallPivoting}}. \code{"Alpha_Wrap"} uses \code{\link[CGALmeshops]{alphaWrap}}.}
  \item{smooth}{\code{character}. Smoothing method. \code{"VCG"} uses \code{\link[Rvcg]{vcgSmooth}}.}
  \item{remesh}{\code{character}. Isotropic remeshing using the \code{\link[Rvcg]{vcgIsotropicRemeshing}} function from package \code{Rvcg}.}
  \item{...}{Options passed to \code{\link[CGALmeshops]{reconstructAFS}}, \code{\link[CGALmeshops]{reconstructSSS}}, \code{\link[CGALmeshops]{reconstructPoisson}}, \code{\link[Rvcg]{vcgBallPivoting}}, \code{\link[CGALmeshops]{alphaWrap}}, or \code{\link[Rvcg]{vcgSmooth}}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF.}
\value{List of mesh objects. Each object is a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{\link[CGALmeshops]{CGALmesh}} object. Use \code{\link[MeshAgreement]{read_mesh}} to read a list of several mesh objects each for several observers. Use \code{\link[MeshAgreement]{read_mesh_one}} to read a single mesh file.}
\seealso{
\code{\link[CGALmeshops]{reconstructAFS}},
\code{\link[CGALmeshops]{reconstructSSS}},
\code{\link[CGALmeshops]{reconstructPoisson}},
\code{\link[CGALmeshops]{alphaWrap}},
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
    meshL   <- read_mesh_obs(ff, reconstruct="AFS")

    get_mesh_info_obs(meshL)
}
}
