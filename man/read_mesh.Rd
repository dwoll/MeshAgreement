\name{read_mesh}
\alias{read_mesh}
\title{Read mesh files}
\description{Read several mesh objects each for several observers from files.}
\usage{read_mesh(x, name, fix_issues=TRUE,
          reconstr_when=c("No", "Fix_Issues", "Yes"),
          reconstr_method=c("AFS", "SSS", "Poisson"),
          ...)}
\arguments{
  \item{x}{\code{list} of \code{character} vectors of file names. One vector for the files from one observer.}
  \item{name}{\code{list} of \code{character} vectors of mesh names that should be assigned to the mesh objects. If missing, names are taken from the file names.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections. Uses surface reconstruction if mesh is not closed.}
  \item{reconstr_when}{\code{character}. Use \code{"No"} for no reconstruction. \code{"Fix_Issues"} does surface reconstruction only if the mesh appears not to be closed. \code{"Yes"} always does surface reconstruction as the first step when readubg a mesh.}
  \item{reconstr_method}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[cgalMeshes]{AFSreconstruction}}. \code{"SSS"} uses \code{\link[cgalMeshes]{SSSreconstruction}}. \code{"Poisson"} uses \code{PoissonReconstruction} if the development branch of \code{cgalMeshes} is installed, and falls back to \code{AFS} otherwise.}
  \item{...}{Options passed to \code{\link[cgalMeshes]{AFSreconstruction}}, \code{\link[cgalMeshes]{SSSreconstruction}}, or \code{PoissonReconstruction}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF. For subsequent comparison, the first file from observer 1 will be paired with the first file from observer 2, and so on.}
\value{\code{list} (each component representing one observer) of \code{list}s of mesh objects. Each object is a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{\link[cgalMeshes]{cgalMesh}} object. Sample object given in \code{\link[MeshAgreement]{data_heart_obsL}}. Use \code{\link[MeshAgreement]{read_mesh_obs}} to read several mesh files from one observer. Use \code{\link[MeshAgreement]{read_mesh_one}} to read a single mesh file.}
\seealso{
\code{\link[cgalMeshes]{cgalMesh}},
\code{\link[cgalMeshes]{AFSreconstruction}},
\code{\link[cgalMeshes]{SSSreconstruction}},
\code{\link[MeshAgreement]{data_heart_obsL}},
\code{\link[MeshAgreement]{read_mesh_obs}},
\code{\link[MeshAgreement]{read_mesh_one}},
}
\examples{
if(interactive()) {
    src_dir <- system.file("extdata", package="MeshAgreement")
    ff1     <- list.files(src_dir, pattern="^Obs01", full.names=TRUE)
    ff2     <- list.files(src_dir, pattern="^Obs02", full.names=TRUE)
    ff3     <- list.files(src_dir, pattern="^Obs03", full.names=TRUE)
    obsL    <- read_mesh(list(Obs01=ff1,
                              Obs02=ff2,
                              Obs03=ff3),
                         reconstr_when="Yes",
                         reconstr_method="AFS")

    get_mesh_info(obsL)
}
}
