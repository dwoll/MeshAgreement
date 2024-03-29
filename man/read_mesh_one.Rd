\name{read_mesh_one}
\alias{read_mesh_one}
\title{Read one mesh file}
\description{Read one mesh object from a file.}
\usage{read_mesh_one(x, name, fix_issues=TRUE, iso_remesh=FALSE,
              reconstr_when=c("No", "Fix_Issues", "Yes"),
              reconstr_method=c("AFS", "SSS", "Poisson", "Ball_Pivoting"),
              ...)}
\arguments{
  \item{x}{\code{character}. File name.}
  \item{name}{\code{character}. Name that should be assigned to the mesh object. If missing, the name is taken from the file name.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections. Uses surface reconstruction if mesh is not closed.}
  \item{iso_remesh}{\code{logical}. Isotropic remeshing using the \code{\link[cgalMeshes]{cgalMesh}} method \code{isotropicRemeshing()}.}
  \item{reconstr_when}{\code{character}. Use \code{"No"} for no reconstruction. \code{"Fix_Issues"} does surface reconstruction only if the mesh appears not to be closed. \code{"Yes"} always does surface reconstruction as the first step when readubg a mesh.}
  \item{reconstr_method}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[cgalMeshes]{AFSreconstruction}}. \code{"SSS"} uses \code{\link[cgalMeshes]{SSSreconstruction}}. \code{"Poisson"} uses \code{PoissonReconstruction} if the development branch of \code{cgalMeshes} is installed, and falls back to \code{AFS} otherwise. \code{"Ball_Pivoting"} uses \code{\link[Rvcg]{vcgBallPivoting}}.}
  \item{...}{Options passed to \code{\link[cgalMeshes]{AFSreconstruction}}, \code{SSSreconstruction}, \code{PoissonReconstruction}, or \code{\link[Rvcg]{vcgBallPivoting}}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF.}
\value{Mesh object - a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{\link[cgalMeshes]{cgalMesh}} object. Use \code{\link[MeshAgreement]{read_mesh}} to read a list of several mesh objects each for several observers. Use \code{\link[MeshAgreement]{read_mesh_obs}} to read a list of several mesh objects each for 1 observer.}
\seealso{
\code{\link[cgalMeshes]{cgalMesh}},
\code{\link[cgalMeshes]{AFSreconstruction}},
\code{\link[Rvcg]{vcgBallPivoting}},
\code{\link[MeshAgreement]{read_mesh}},
\code{\link[MeshAgreement]{read_mesh_obs}},
}
\examples{
if(interactive()) {
    src_dir <- system.file("extdata", package="MeshAgreement")
    ff      <- list.files(src_dir, pattern="^Obs01", full.names=TRUE)
    mesh    <- read_mesh_one(ff[1],
                             reconstr_when="Yes",
                             reconstr_method="AFS")
                             
    get_mesh_info_one(mesh)
}
}
