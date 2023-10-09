\name{read_mesh_obs}
\alias{read_mesh_obs}
\title{Read mesh files}
\description{Read a list of mesh objects from one observer from files.}
\usage{read_mesh_obs(x, name, fix_issues=TRUE, iso_remesh=FALSE,
              reconstr_when=c("No", "Fix_Issues", "Yes"),
              reconstr_method=c("AFS", "SSS", "Poisson", "Ball_Pivoting"),
              ...)}
\arguments{
  \item{x}{\code{character} vector of file names.}
  \item{name}{\code{character} vector of names that should be assigned to the mesh objects. If missing, names are taken from the file names.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections. Uses surface reconstruction if mesh is not closed.}
  \item{iso_remesh}{\code{logical}. Isotropic remeshing using the \code{\link[cgalMeshes]{cgalMesh}} method \code{isotropicRemeshing()}.}
  \item{reconstr_when}{\code{character}. Use \code{"No"} for no reconstruction. \code{"Fix_Issues"} does surface reconstruction only if the mesh appears not to be closed. \code{"Yes"} always does surface reconstruction as the first step when readubg a mesh.}
  \item{reconstr_method}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[cgalMeshes]{AFSreconstruction}}. \code{"SSS"} uses \code{\link[cgalMeshes]{SSSreconstruction}}. \code{"Poisson"} uses \code{PoissonReconstruction} if the development branch of \code{cgalMeshes} is installed, and falls back to \code{AFS} otherwise. \code{"Ball_Pivoting"} uses \code{\link[Rvcg]{vcgBallPivoting}}.}
  \item{...}{Options passed to \code{\link[cgalMeshes]{AFSreconstruction}}, \code{SSSreconstruction}, \code{PoissonReconstruction}, or \code{\link[Rvcg]{vcgBallPivoting}}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF.}
\value{List of mesh objects. Each object is a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{\link[cgalMeshes]{cgalMesh}} object. Use \code{\link[MeshAgreement]{read_mesh}} to read a list of several mesh objects each for several observers. Use \code{\link[MeshAgreement]{read_mesh_one}} to read a single mesh file.}
\seealso{
\code{\link[cgalMeshes]{cgalMesh}},
\code{\link[cgalMeshes]{AFSreconstruction}},
\code{\link[Rvcg]{vcgBallPivoting}},
\code{\link[MeshAgreement]{read_mesh}},
\code{\link[MeshAgreement]{read_mesh_one}},
}
\examples{
if(interactive()) {
    src_dir <- system.file("extdata", package="MeshAgreement")
    ff      <- list.files(src_dir, pattern="^Obs01", full.names=TRUE)
    meshL   <- read_mesh_obs(ff,
                             reconstr_when="Yes",
                             reconstr_method="AFS")

    get_mesh_info_obs(meshL)
}
}
