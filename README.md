# MeshAgreement

Agreement measures for 3D structures saved as mesh files. R package containing an interactive [R](https://www.r-project.org/) [Shiny](https://shiny.rstudio.com/) app. You can upload mesh files (STL, PLY, OBJ, OFF) to generate agreement measures for all pairwise comparisons, as well as the corresponding aggregated agreement. The intended application is to compare delineated structures for radiotherapy treatment planning.

  * Live demo here: [http://shiny.imbei.uni-mainz.de:3838/MeshAgreement/](http://shiny.imbei.uni-mainz.de:3838/MeshAgreement/)
  * [ESAPI export script](https://github.com/dwoll/MeshAgreement/tree/main/inst/extdata) for getting 3D mesh files from Varian Eclipse

# Implemented agreement measures

 * Pairwise distance-based and volume-overlap-based metrics
     * DCOM: Distance between centers of mass
     * ASD: Average surface distance
     * RMSD: Root mean squared surface distance
     * HD_max: Hausdorff distance - max of both directed HDs
     * HD_avg: Hausdorff distance - average of both directed HDs
 * JSC: Jaccard similarity coefficient
 * DSC: Dice similarity coefficient

# Required packages

`MeshAgreement` heavily relies on packages developed by Stéphane Laurent, which build on package [RcppCGAL](https://https://github.com/ericdunipace/RcppCGAL) to enable the functionality from the [CGAL](https://www.cgal.org/) library for computational geometry. The second back-end for geometry calculations is the [VCG Library](http://www.vcglib.net/).

**Note that as of 10/2023, some of these packages are not on CRAN, but have to be installed from GitHub using `remotes::install_github()`**

  * [Rvcg](https://CRAN.R-project.org/package=Rvcg)
  * [cgalMeshes](https://github.com/stla/cgalMeshes)
  * [Boov](https://https://github.com/stla/Boov) - useful for getting better performance in union/intersection calculation for some meshes
  * [shiny](https://CRAN.R-project.org/package=shiny)
  * [bs4Dash](https://CRAN.R-project.org/package=bs4Dash)
  * [DT](https://CRAN.R-project.org/package=DT)
  * [sortable](https://CRAN.R-project.org/package=sortable)

# TODO

  * Enable setting a mesh as gold standard for comparison instead of doing all pairwise comparisons - then calculate TP, FP, TN, FN from union/intersection

# Literature (selection)

 * Fotina et al. Critical discussion of evaluation parameters for inter-observer variability in target definition for radiation therapy. Strahlenther Onkol 2012; 188: 160-167.
 * Hanna  et al. Geometrical Analysis of Radiotherapy Target Volume Delineation: a Systematic Review of Reported Comparison Methods. Clin Oncol 2010; 22, 515-525.
 * Heimann et al. Comparison and Evaluation of Methods for Liver Segmentation From CT Datasets. IEEE Trans Med Imaging 2009; 28: 1251-1265.
 * Sherer et al. Metrics to evaluate the performance of auto-segmentation for radiation treatment planning: A critical review. Radiother Oncol 2021; 160: 185-191."
