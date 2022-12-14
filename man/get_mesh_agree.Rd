\name{get_mesh_agree}
\alias{get_mesh_agree}
\title{Agreement measures for pairs from a list of 3D meshes}
\description{Calculates distance-based as well as volume-overlap-based agreement measures in each possible pairwise comparison for a list of 3D meshes.}
\usage{
get_mesh_agree(x, boov=FALSE, do_ui=FALSE, chop=TRUE, ...)
}
\arguments{
  \item{x}{\code{list} of observer/mesh objects as generated by \code{\link[MeshAgreement]{read_mesh}}.}
  \item{boov}{\code{logical}. Use package \code{\link[Boov]{MeshesUnion}} / \code{\link[Boov]{MeshesIntersection}} instead of the corresponding \code{\link[cgalMeshes]{cgalMesh}} methods for calculating union/intersection volumes. Depending on the meshes, this may have better performance.}
  \item{do_ui}{\code{logical}. Calculate volume-overlap-based measures from union/intersection volumes. More time intensive than distance-based measures.}
  \item{chop}{\code{logical}. Delete components from the output of \code{\link[Rvcg]{vcgMetro}} that will not be used later on.}
  \item{...}{Options passed to \code{\link[Rvcg]{vcgMetro}}.}
}
\details{Currently, no mesh can be designated gold standard. \code{\link[MeshAgreement]{get_mesh_agree_pair}} calculates the agreement for a single pair of meshes. \code{\link[MeshAgreement]{get_mesh_metro}} calculates the distances. \code{\link[MeshAgreement]{get_mesh_ui}} calculates the mesh union and intersection. For \code{boov=TRUE}, package \code{Boov} has to be installed separately.}
\value{
\code{data.frame} with these variables
\item{mesh_1}{Name of the first mesh of the pair.}
\item{mesh_2}{Name of the second mesh of the pair.}
\item{group}{The grouping variable indicating which pairs belong to the same structure.}
\item{vol_1}{Volume of the first mesh of the pair.}
\item{vol_2}{Volume of the second mesh of the pair.}
\item{vol_u}{Volume of the union.}
\item{vol_i}{Volume of the intersection.}
\item{DCOM}{Euclidean distance between the respective center of mass of both meshes.}
\item{HD_max}{Hausdorff distance - worst case, maximum of both directed Hausdorff distances.}
\item{HD_avg}{Hausdorff distance - average, arithmetic mean of both directed Hausdorff distancess.}
\item{ASD}{Average symmetric surface distance.}
\item{RMSD}{Root mean squared symmetric surface distance.}
\item{JSC}{Jaccard similarity coefficient.}
\item{DSC}{Dice similarity coefficient.}
}
\references{
Fotina et al. Critical discussion of evaluation parameters for inter-observer variability in target definition for radiation therapy. Strahlenther Onkol 2012; 188: 160-167.

Hanna  et al. Geometrical Analysis of Radiotherapy Target Volume Delineation: a Systematic Review of Reported Comparison Methods. Clin Oncol 2010; 22, 515-525.

Heimann et al. Comparison and Evaluation of Methods for Liver Segmentation From CT Datasets. IEEE Trans Med Imaging 2009; 28: 1251-1265.

Sherer et al. Metrics to evaluate the performance of auto-segmentation for radiation treatment planning: A critical review. Radiother Oncol 2021; 160: 185-191.

\url{https://www.cgal.org/}

\url{http://vcg.isti.cnr.it/vcglib/}
}
\seealso{
\code{\link[MeshAgreement]{get_mesh_agree_pair}},
\code{\link[MeshAgreement]{get_mesh_metro}},
\code{\link[MeshAgreement]{get_mesh_ui}}
}
\examples{
heartL <- mesh3dL_to_cgalMeshL(data_heart_obsL)
get_mesh_agree(heartL, do_ui=FALSE, nSamples=500L, silent=TRUE)
}
