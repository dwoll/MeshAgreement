\name{read_mesh}
\alias{read_mesh}
\title{Read mesh files}
\description{Read several mesh objects each for several observers from files.}
\usage{read_mesh(x, name, fix_issues=TRUE,
          reconstruct=c("No", "AFS", "SSS", "Poisson", "Ball_Pivoting",
                        "Alpha_Wrap"),
          smooth=c("No", "VCG"),
          remesh=c("No", "Isotropic"),
          ...)}
\arguments{
  \item{x}{\code{list} of \code{character} vectors of file names. One vector for the files from one observer.}
  \item{name}{\code{list} of \code{character} vectors of mesh names that should be assigned to the mesh objects. If missing, names are taken from the file names.}
  \item{fix_issues}{\code{logical}. Try to fix mesh issues, such as not being closed, not bounding a volume, or having self-intersections.}
  \item{reconstruct}{\code{character}. Surface reconstruction method. \code{"AFS"} uses \code{\link[CGALmeshops]{reconstructAFS}}. \code{"SSS"} uses \code{\link[CGALmeshops]{reconstructSSS}}. \code{"Poisson"} uses \code{\link[CGALmeshops]{reconstructPoisson}}. \code{"Ball_Pivoting"} uses \code{\link[Rvcg]{vcgBallPivoting}}. \code{"Alpha_Wrap"} uses \code{\link[CGALmeshops]{alphaWrap}}.}
  \item{smooth}{\code{character}. Smoothing method. \code{"VCG"} uses \code{\link[Rvcg]{vcgSmooth}}.}
  \item{remesh}{\code{character}. Isotropic remeshing using the \code{\link[Rvcg]{vcgIsotropicRemeshing}} function from package \code{Rvcg}.}
  \item{...}{Options passed to \code{\link[CGALmeshops]{reconstructAFS}}, \code{\link[CGALmeshops]{reconstructSSS}}, \code{\link[CGALmeshops]{reconstructPoisson}}, \code{\link[Rvcg]{vcgBallPivoting}}, \code{\link[CGALmeshops]{alphaWrap}}, or \code{\link[Rvcg]{vcgSmooth}}.}
}
\details{Supported file formats: STL, PLY, OBJ, OFF. For subsequent comparison, the first file from observer 1 will be paired with the first file from observer 2, and so on.}
\value{\code{list} (each component representing one observer) of \code{list}s of mesh objects. Each object is a \code{list} with components \code{name}, \code{mesh}, \code{volume}, \code{centroid}, where \code{mesh} is a \code{\link[CGALmeshops]{CGALmesh}} object. Sample object given in \code{\link[MeshAgreement]{data_heart_obsL}}. Use \code{\link[MeshAgreement]{read_mesh_obs}} to read several mesh files from one observer. Use \code{\link[MeshAgreement]{read_mesh_one}} to read a single mesh file.}
\seealso{
\code{\link[CGALmeshops]{reconstructAFS}},
\code{\link[CGALmeshops]{reconstructSSS}},
\code{\link[CGALmeshops]{reconstructPoisson}},
\code{\link[CGALmeshops]{alphaWrap}},
\code{\link[Rvcg]{vcgBallPivoting}},
\code{\link[Rvcg]{vcgIsotropicRemeshing}},
\code{\link[Rvcg]{vcgSmooth}},
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
                         reconstruct="AFS")

    get_mesh_info(obsL)
}
}
