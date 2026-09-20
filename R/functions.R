get_name_pair <- function(x, y, sep=" <-> ") {
    paste0(x, sep, y)
}

get_name_elem <- function(x, pos=1L, sep=" <-> ") {
    stopifnot(pos %in% c(1L, 2L))
    stopifnot(grepl(sep, x))
    if(pos == 1L) {
        gsub(paste0("^(.+)", sep, ".+$"), "\\1", x)
    } else {
        gsub(paste0("^.+", sep, "(.+$)"), "\\1", x)
    }
}

remesh_mesh <- function(x, how=c("No", "Isotropic", "Simplify"), ...) {
    how <- match.arg(tolower(how),
                     choices=c("no", "isotropic", "simplify"))

    ## arguments for remesh method
    args_remesh <- list(isotropic=c("TargetLen", "FeatureAngleDeg",
                                    "MaxSurfDist", "iterations", "Adaptive"),
                        simplify=c("method", "stopUeRatio", "stopUeCount", "policyGH"))

    dotsL     <- list(...)
    dotsL_sub <- dotsL[names(dotsL) %in% args_remesh[[how]]]

    if(how == "isotropic") {
        x_rgl      <- toRGL(x)
        argL       <- c(list(x=x_rgl), dotsL_sub)
        mesh_rgl_r <- do.call(vcgIsotropicRemeshing, argL)
        makeMeshValid(mesh_rgl_r)
    } else if(how == "simplify") {
        argL <- c(list(x=x), dotsL_sub)
        do.call(remeshSimplify, argL)
    } else if(how == "no") {
        x
    }
}

smooth_mesh <- function(x, how=c("No", "VCG", "CGAL"), ...) {
    how <- match.arg(tolower(how),
                     choices=c("no", "vcg", "cgal"))

    ## arguments for smoothing methods
    args_remesh <- list(vcg=c("type", "iteration", "lambda", "mu", "delta"),
                        cgal=c("nIter", "time"))

    dotsL     <- list(...)
    dotsL_sub <- dotsL[names(dotsL) %in% args_remesh[[how]]]

    if(how == "vcg") {
        x_rgl      <- toRGL(x)
        argL       <- c(list(mesh=x_rgl), dotsL_sub)
        mesh_rgl_r <- do.call(vcgSmooth, argL)
        makeMeshValid(mesh_rgl_r)
    } else if(how == "cgal") {
        argL   <- c(list(x=x), dotsL_sub)
        mesh_r <- do.call(remeshSmooth, argL)
    } else if(how == "no") {
        x
    }
}

reconstruct_mesh <- function(x,
                             how=c("No", "AFS", "SSS", "Poisson",
                                   "Ball_Pivoting", "Alpha_Wrap"),
                             ...) {
    how <- match.arg(tolower(how),
                     choices=c("no", "afs", "sss", "poisson",
                               "ball_pivoting", "alpha_wrap"))

    ## arguments for reconstruction methods
    args_recon <- list(afs          =c("jetSmoothing"),
                       sss          =c("scaleIterations", "neighbors", "samples", "separateShells", "forceManifold", "borderAngle"),
                       poisson      =c("normals", "normalsMethod", "spacing", "smAngle", "smRadius", "smDistance"),
                       ball_pivoting=c("radius", "clustering", "angle", "deleteFaces"),
                       alpha_wrap   =c("alphaRel", "offsetRel"))

    dotsL     <- list(...)
    dotsL_sub <- dotsL[names(dotsL) %in% args_recon[[how]]]

    if(how == "afs") {
        argL     <- c(list(x=x[["vertices"]]), dotsL_sub)
        do.call(reconstructAFS, argL)
    } else if(how == "sss") {
        argL     <- c(list(x=x[["vertices"]]), dotsL_sub)
        do.call(reconstructSSS, argL)
    } else if(how == "poisson") {
        normalsMethod <- dotsL_sub[["normalsMethod"]]
        if(!is.null(normalsMethod)) {
            pnm     <- tolower(normalsMethod)
            normals <- dotsL_sub[["normals"]]
            if(!is.null(normals) &&
               (pnm %in% c("jet", "pca"))) {
                dotsL_sub[["normalsFun"]] <- getNormalsFun(normals, method=pnm)
            }

            dotsL_sub[["normals"]]       <- NULL
            dotsL_sub[["normalsMethod"]] <- NULL
        }

        argL     <- c(list(x=x[["vertices"]]), dotsL_sub)
        do.call(reconstructPoisson, argL)
    } else if(how == "ball_pivoting") {
        x_rgl    <- toRGL(x)
        argL     <- c(list(x=x_rgl), dotsL_sub)
        mesh_rgl <- do.call("vcgBallPivoting", argL)
        makeMeshValid(mesh_rgl)
    } else if(how == "alpha_wrap") {
        argL     <- c(list(x=x[["vertices"]]), dotsL_sub)
        do.call(alphaWrap, argL)
    } else if(how == "no") {
        x
    }
}

read_mesh_one <- function(x,
                          name,
                          fix_issues =TRUE,
                          reconstruct=c("No", "AFS", "SSS", "Poisson",
                                        "Ball_Pivoting", "Alpha_Wrap"),
                          smooth     =c("No", "VCG", "CGAL"),
                          remesh     =c("No", "Isotropic", "Simplify"),
                          ...) {
    reconstruct <- match.arg(tolower(reconstruct),
                             choices=c("no", "afs", "sss", "poisson",
                                       "ball_pivoting", "alpha_wrap"))

    smooth <- match.arg(tolower(smooth),
                        choices=c("no", "vcg", "cgal"))

    remesh <- match.arg(tolower(remesh),
                        choices=c("no", "isotropic", "simplify"))

    ## collect arguments intended to be passed to other functions
    dotsL0 <- list(...)

    ## dotsL0 may have 1 extra hierarchy level -> strip
    dotsL <- if(is.null(names(dotsL0)) && (length(dotsL0) == 1L)) {
        unlist(dotsL0, recursive=FALSE)
    } else {
        dotsL0
    }

    ## arguments for makeMesh()
    args_makeMesh <- c("removeIntersections",
                       "removeMethod",
                       "fillHoles",
                       "fairHole",
                       "maxNumHoles",
                       "verbose")

    mesh_name <- if(missing(name)) {
        basename(tools::file_path_sans_ext(x))
    } else {
        basename(tools::file_path_sans_ext(name))
    }

    # mesh_raw <- readMeshFile(x)
    dotsL_makeMesh <- c(list(x          =x,
                             triangulate=TRUE,
                             repairSoup =fix_issues,
                             normals    =FALSE),
                        dotsL[names(dotsL) %in% args_makeMesh])

    ## fixed max number of holes allowed
    ## TODO expose this as an option in the shiny frontend
    if(!hasName(dotsL_makeMesh, "removeIntersections")) {
        dotsL_makeMesh$removeIntersections <- TRUE
    }

    if(!hasName(dotsL_makeMesh, "fillHoles")) {
        dotsL_makeMesh$fillHoles <- TRUE
    }

    if( hasName(dotsL_makeMesh, "fillHoles") &&
       !hasName(dotsL_makeMesh, "maxNumHoles")) {
        dotsL_makeMesh$maxNumHoles <- 10L
    }

    mesh_in <- do.call(makeMesh, dotsL_makeMesh)

    ## remove makeMesh() arguments from dotsL
    dotsL[names(dotsL) %in% args_makeMesh] <- list(NULL)

    ## reconstruct?
    mesh_r0 <- if(reconstruct != "no") {
        argL <- c(list(x=mesh_in, how=reconstruct), dotsL)
        do.call(reconstruct_mesh, argL)
    } else {
        mesh_in
    }

    ## smooth?
    mesh_r1 <- if(smooth != "no") {
        argL <- c(list(x=mesh_r0, how=smooth), dotsL)
        do.call(smooth_mesh, argL)
    } else {
        mesh_r0
    }

    ## re-mesh?
    mesh_r2 <- if(remesh != "no") {
        argL <- c(list(x=mesh_r1, how=remesh), dotsL)
        do.call(remesh_mesh, argL)
    } else {
        mesh_r1
    }

    vol <- getVolume(mesh_r2)
    ctr <- getCentroid(mesh_r2)

    list(name    =mesh_name,
         mesh    =mesh_r2,
         volume  =vol,
         centroid=ctr)
}

read_mesh_obs <- function(x,
                          name,
                          fix_issues =TRUE,
                          reconstruct=c("No", "AFS", "SSS", "Poisson",
                                        "Ball_Pivoting", "Alpha_Wrap"),
                          smooth     =c("No", "VCG", "CGAL"),
                          remesh     =c("No", "Isotropic", "Simplify"),
                          ...) {
    reconstruct <- match.arg(tolower(reconstruct),
                             choices=c("no", "afs", "sss", "poisson",
                                       "ball_pivoting", "alpha_wrap"))

    smooth <- match.arg(tolower(smooth),
                        choices=c("no", "vcg", "cgal"))

    remesh <- match.arg(tolower(remesh),
                        choices=c("no", "isotropic", "simplify"))

    mesh_names <- if(missing(name)) {
        basename(tools::file_path_sans_ext(x))
    } else {
        basename(tools::file_path_sans_ext(name))
    }

    meshL <- lapply(seq_along(x), function(i) {
        read_mesh_one(x[i],
                      name       =mesh_names[i],
                      fix_issues =fix_issues,
                      reconstruct=reconstruct,
                      smooth     =smooth,
                      remesh     =remesh,
                      ...)
    })

    setNames(meshL, mesh_names)
}

read_mesh <- function(x,
                      name,
                      fix_issues =TRUE,
                      reconstruct=c("No", "AFS", "SSS", "Poisson",
                                    "Ball_Pivoting", "Alpha_Wrap"),
                      smooth     =c("No", "VCG", "CGAL"),
                      remesh     =c("No", "Isotropic"),
                      ...) {
    reconstruct <- match.arg(tolower(reconstruct),
                             choices=c("no", "afs", "sss", "poisson",
                                       "ball_pivoting", "alpha_wrap"))

    smooth <- match.arg(tolower(smooth),
                        choices=c("no", "vcg", "cgal"))

    remesh <- match.arg(tolower(remesh),
                        choices=c("no", "isotropic", "simplify"))

    dotsL <- list(...)

    obs_names <- if(missing(name)) {
        names(x)
    } else {
        names(name)
    }

    mesh_names <- if(missing(name)) {
        lapply(x, function(y) { basename(tools::file_path_sans_ext(y)) })
    } else {
        lapply(x, function(y) { basename(tools::file_path_sans_ext(name)) })
    }

    Map(read_mesh_obs,
        setNames(x, obs_names),
        mesh_names,
        fix_issues =fix_issues,
        reconstruct=reconstruct,
        smooth     =smooth,
        remesh     =remesh,
        list(dotsL))
    }

get_mesh_info_one <- function(x) {
    mesh <- x[["mesh"]]
    data.frame(name=x[["name"]],
               n_verts=nrow(mesh[["vertices"]]),
               n_faces=nrow(mesh[["faces"]]),
               volume=x[["volume"]],
               ctr_x=x[["centroid"]][1],
               ctr_y=x[["centroid"]][2],
               ctr_z=x[["centroid"]][3])
}

get_mesh_info_obs <- function(x) {
    d_out <- do.call(rbind, Map(get_mesh_info_one, x))
    rownames(d_out) <- NULL
    d_out
}

get_mesh_info <- function(x) {
    x_lens <- lengths(x)
    d_out  <- do.call(rbind, Map(get_mesh_info_obs, x))
    rownames(d_out) <- NULL
    cbind(observer=rep(names(x), times=x_lens),
          d_out)
}

## starting from list of observers, each with a list of meshes
## generate all observer-pairs for each corresponding mesh-list entry
get_mesh_pairs <- function(x, sep=" <-> ", names_only=FALSE) {
    if(length(x) <= 1L) { stop("Need more than 1 mesh for comparisons") }

    ## number of meshes per observer
    n_obs_meshes <- lengths(x) # may be different per observer
    n_meshes     <- max(n_obs_meshes)

    ## for given pair, put corresponding meshes in a list
    get_pair_mesh <- function(pair, mesh) {
        idx1  <- pairs_idx[pair, 1L] # index observer 1
        idx2  <- pairs_idx[pair, 2L] # index observer 2
        obs1  <- x[[idx1]]           # observer 1
        obs2  <- x[[idx2]]           # observer 2
        ## do both observers have the mesh?
        if((length(obs1) >= mesh) && (length(obs2) >= mesh)) {
            mesh_1 <- x[[idx1]][[mesh]]
            mesh_2 <- x[[idx2]][[mesh]]

            ## add group information -> same structure
            if(names_only) {
                list(name=get_name_pair(mesh_1[["name"]], mesh_2[["name"]], sep=sep),
                     group=sprintf("strct_%.3d", mesh))
            } else {
                list(name=get_name_pair(mesh_1[["name"]], mesh_2[["name"]], sep=sep),
                     mesh_1=mesh_1,
                     mesh_2=mesh_2,
                     group=sprintf("strct_%.3d", mesh))
            }
        } else {
            NULL
        }
    }

    pairs_idx <- if(length(x) >= 2L) {
        t(combn(seq_along(x), 2L))
    } else {
        matrix(c(1L, 1L), ncol=2L)
    }

    ll_outer <- lapply(seq_len(n_meshes), function(idx_mesh) {
        lapply(seq_len(nrow(pairs_idx)), function(idx_pair) { get_pair_mesh(idx_pair, idx_mesh) })
    })

    ## weed out NULL components
    ll <- Filter(Negate(is.null), unlist(ll_outer, recursive=FALSE))
    pair_names <- lapply(ll, function(x) { x[["name"]] })
    setNames(ll, pair_names)
}

get_mesh_metro_pair <- function(x, chop=TRUE, ...) {
    metro <- vcgMetro(toRGL(x[["mesh_1"]][["mesh"]]),
                      toRGL(x[["mesh_2"]][["mesh"]]),
                      ...)

    if(chop) {
        metro[["distances1"]]    <- NULL
        metro[["distances2"]]    <- NULL
        metro[["forward_hist"]]  <- NULL
        metro[["backward_hist"]] <- NULL
    }

    metro[["mesh_1"]] <- metro[["mesh1"]]
    metro[["mesh_2"]] <- metro[["mesh2"]]
    metro[["mesh1"]]  <- NULL
    metro[["mesh2"]]  <- NULL
    metro[["name"]]   <- x[["name"]]
    metro[["group"]]  <- x[["group"]]
    metro
}

get_mesh_metro <- function(x, chop=TRUE, ...) {
    pairL <- get_mesh_pairs(x)
    Map(get_mesh_metro_pair, pairL, chop=chop, ...)
}

## distance measures, union, intersection for each mesh pair
get_mesh_agree_pair <- function(x, do_ui=FALSE, ...) {
    DCOM  <- sqrt(sum((x[["mesh_2"]][["centroid"]] -
                       x[["mesh_1"]][["centroid"]])^2))
    HD <- SurfaceMesh::getHausdorff(x[["mesh_1"]][["mesh"]],
                                  x[["mesh_2"]][["mesh"]],
                                  symmetric=TRUE,
                                  errorBound=0.001)

    surf_dist <- SurfaceMesh::getSurfaceDist(x[["mesh_1"]][["mesh"]],
                                           x[["mesh_2"]][["mesh"]],
                                           symmetric=TRUE,
                                           p=0.95)
    HD95 <- surf_dist[["HDq"]]
    ASSD <- surf_dist[["ASSD"]]
    RMSE <- surf_dist[["RMSE"]]

    ## volume-overlap-based measures
    if(do_ui) {
        vol_ui <- SurfaceMesh::getJSCDSC(x[["mesh_1"]][["mesh"]],
                                       x[["mesh_2"]][["mesh"]])
        vol_i <- vol_ui[["VolI"]]
        vol_u <- vol_ui[["VolU"]]
        JSC   <- vol_ui[["JSC"]]
        DSC   <- vol_ui[["DSC"]]
    } else {
        vol_u <- NA_real_
        vol_i <- NA_real_
        JSC   <- NA_real_
        DSC   <- NA_real_
    }

    data.frame(mesh_1=x[["mesh_1"]][["name"]],
               mesh_2=x[["mesh_2"]][["name"]],
               group =x[["group"]],
               vol_u =vol_u,
               vol_i =vol_i,
               DCOM  =DCOM,
               HD    =HD,
               HD95  =HD95,
               ASSD  =ASSD,
               RMSE  =RMSE,
               JSC   =JSC,
               DSC   =DSC)
}

get_mesh_agree <- function(x, do_ui=FALSE, ...) {
    pairL  <- get_mesh_pairs(x)
    agreeL <- Map(get_mesh_agree_pair,
                  pairL,
                  do_ui=do_ui)

    d <- do.call(rbind, agreeL)
    rownames(d) <- NULL
    d
}

get_mesh_agree_long <- function(x) {
    vars_varying <- c("DCOM",
                      "HD", "HD95", "ASSD", "RMSE",
                      "vol_u", "vol_i",
                      "JSC", "DSC")
    ## TODO add vol_1, vol_2 above?
    ## this does not work as vol_* variables may be missing,
    ## leading to missing values for ID variable created in
    ## reshapeLong()
    # vars_id <- names(x)[!(names(x) %in% vars_varying)]
    vars_id <- c("mesh_1", "mesh_2", "group")

    dL <- reshape(x,
                  direction="long",
                  idvar    =vars_id,
                  varying  =vars_varying,
                  v.names  ="observed",
                  timevar  ="metric")

    rownames(dL) <- NULL

    dL[["metric"]] <- factor(dL[["metric"]],
                             levels=seq_along(vars_varying),
                             labels=vars_varying)

    dL
}

get_mesh_agree_aggr <- function(x, na.rm=FALSE) {
    d_agreeL <- get_mesh_agree_long(x)
    d_agreeL[["observed_ln"]] <- log(d_agreeL[["observed"]])

    d_mean   <- aggregate(observed    ~ group + metric, FUN=mean,   data=d_agreeL, na.rm=na.rm)
    d_median <- aggregate(observed    ~ group + metric, FUN=median, data=d_agreeL, na.rm=na.rm)
    d_sd     <- aggregate(observed    ~ group + metric, FUN=sd,     data=d_agreeL, na.rm=na.rm)
    d_var    <- aggregate(observed    ~ group + metric, FUN=var,    data=d_agreeL, na.rm=na.rm)
    d_varlog <- aggregate(observed_ln ~ group + metric, FUN=var,    data=d_agreeL, na.rm=na.rm)

    d_aggr <- Reduce(function(x, y) { suppressWarnings(merge(x, y, by=c("group", "metric"))) },
                     list(d_mean, d_median, d_sd, d_var, d_varlog))

    names(d_aggr)       <- c("group", "metric", "Mean", "Median", "SD", "VAR", "VAR_log")
    d_aggr[["CV"]]      <- d_aggr[["SD"]] / d_aggr[["Mean"]]
    d_aggr[["CV_ln"]]   <- sqrt(exp(d_aggr[["VAR_log"]]) - 1)
    d_aggr[["VAR"]]     <- NULL
    d_aggr[["VAR_log"]] <- NULL

    d_aggr
}

get_mesh_agree_aggr_long <- function(x) {
    vars_varying <- c("Mean", "Median", "SD", "CV", "CV_ln")
    vars_id      <- names(x)[!(names(x) %in% vars_varying)]

    dL <- reshape(x,
                  direction="long",
                  idvar    =vars_id,
                  varying  =vars_varying,
                  v.names  ="observed",
                  timevar  ="statistic")

    rownames(dL) <- NULL

    dL[["statistic"]] <- factor(dL[["statistic"]],
                                levels=seq_along(vars_varying),
                                labels=vars_varying)

    dL
}

meshL_to_observerL <- function(x) {
    ll <- Map(function(i, name) {
        setNames(list(i), name)
    }, x, names(x))

    setNames(ll, sprintf("Observer_%.2d", seq_along(ll)))
}

mesh3dL_to_CGALmeshL <- function(x) {
    convert_mesh_one <- function(y) {
        if(inherits(y[["mesh"]], "mesh3d")) {
            y[["mesh"]] <- makeMeshValid(y[["mesh"]])
        } else if(!inherits(y[["mesh"]], "CGALmesh")) {
            stop("Input must be either `mesh3d` or `CGALmesh` object.")
        }

        y
    }

    convert_meshL <- function(z) {
        lapply(z, convert_mesh_one)
    }

    lapply(x, convert_meshL)
}
