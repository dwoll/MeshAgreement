#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Daniel Wollschläger <wollschlaeger@uni-mainz.de>
## Package MeshAgreement
## https://github.com/dwoll/MeshAgreement
## Agreement measures for 3D structures
## Pairwise distance-based and volume-overlap-based metrics
## DCOM:   Distance between centers of mass
## ASD:    Average surface distance
## RMSD:   Root mean squared surface distance
## HD_max: Hausdorff distance - max of both directed HDs
## HD_avg: Hausdorff distance - average of both directed HDs
## JSC:    Jaccard similarity coefficient
## DSC:    Dice similarity coefficient
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(ggplot2)
library(plotly)
library(sortable)
library(rgl)
library(MeshAgreement)

source("app_00_global.R")

#####---------------------------------------------------------------------------
## app code
#####---------------------------------------------------------------------------

shiny::shinyApp(
    ui=dashboardPage(
        title="Agreement measures for 3D structures",
        dark=NULL,
        help=FALSE,
        fullscreen=TRUE,
        scrollToTop=TRUE,
        header=dashboardHeader(
            title=NULL,
            fixed=FALSE,
            leftUI=tagList(tags$code(tags$h3("Agreement measures for 3D structures",
                                             style="display: flex; align-items: center; justify-content: center; margin-right: 20px;"))),
            rightUI=tagList()
        ),
        # controlbar=source("app_ui_controlbar.R", encoding="UTF8")$value,
        footer=dashboardFooter(
            fixed=FALSE,
            left=NULL,
            right=tagList(p(actionButton(
                inputId="bttn_footer_impressum",
                label="Impressum / Haftung / Urheberrecht",
                size="sm",
                no_outline=TRUE,
                style="minimal",
                color="primary")
            ))
        ),
        body=dashboardBody(
            tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
            tabItems(
                tabItem(
                    tabName="tab_home",
                    source("app_ui_tab_home.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_data",
                    source("app_ui_tab_data.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_agreement",
                    source("app_ui_tab_agreement.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_view",
                    source("app_ui_tab_view.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_about",
                    source("app_ui_tab_about.R", local=TRUE, encoding="UTF8")$value
                )
            )
        ),
        sidebar=source("app_ui_sidebar.R", encoding="UTF8")$value #,
        # tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css"))
        # includeCSS("www/custom.css")
        # tags$head(tags$style(HTML('#apply_box .box-header{ display: none}')))
    ),
    #####-----------------------------------------------------------------------
    ## server
    #####-----------------------------------------------------------------------
    server=function(input, output, session) {
        observeEvent(input$bttn_home_go_about1, {
            updateTabItems(session, inputId="sidebar_tabs", selected="tab_about")
        })
        observeEvent(input$bttn_home_go_about2, {
            updateTabItems(session, inputId="sidebar_tabs", selected="tab_about")
        })
        observeEvent(input$bttn_footer_impressum, {
            showModal(popup_info_impressum)
        })
        observeEvent(input$meshes_input_source, {
            if(!is.null(input$meshes_input_source) &&
               (input$meshes_input_source == "builtin")) {
                updatebs4Card("read_reconstruct_card", action="remove")
                updatebs4Card("read_smooth_card",      action="remove")
                updatebs4Card("read_remesh_card",      action="remove")
            }
        })
        observeEvent(input$read_mesh_reconstruct, {
            if(!is.null(input$read_mesh_reconstruct)) {
                if(input$read_mesh_reconstruct != "No") {
                    updatebs4Card("read_reconstruct_card", action="restore")
                } else {
                    updatebs4Card("read_reconstruct_card", action="remove")
                }
            } else {
                updatebs4Card("read_reconstruct_card", action="remove")
            }
        })
        observeEvent(input$read_mesh_smooth, {
            if(!is.null(input$read_mesh_smooth)) {
                if(input$read_mesh_smooth != "No") {
                    updatebs4Card("read_smooth_card", action="restore")
                } else {
                    updatebs4Card("read_smooth_card", action="remove")
                }
            } else {
                updatebs4Card("read_smooth_card", action="remove")
            }
        })
        observeEvent(input$read_mesh_remesh, {
            if(!is.null(input$read_mesh_remesh)) {
                if(input$read_mesh_remesh != "No") {
                    updatebs4Card("read_remesh_card", action="restore")
                } else {
                    updatebs4Card("read_remesh_card", action="remove")
                }
            } else {
                updatebs4Card("read_remesh_card", action="remove")
            }
        })
        react_file_sel <- reactive({
            ## only change when explicitly applied
            input$apply_file_sel
            ## isolate against non-applied changes in data input UI elements
            isolate({
                n_observers <- if(is.null(input$num_observers)) {
                    2L
                } else {
                    input$num_observers
                }
                
                meshL <- if(input$meshes_input_source == "builtin") {
                    if(exists("CGALmesh_heart_obsL") && !is.null(CGALmesh_heart_obsL)) {
                        ## use builtin data
                        ll <- CGALmesh_heart_obsL
                        if(input$meshes_sel_mode == "indiv") {
                            ll[seq_len(min(c(length(ll), n_observers)))]
                        } else {
                            ll
                        }
                    } else {
                        NULL
                    }
                } else {
                    ## read data from file
                    ## number of file selection elements = number of observers
                    n_file_sel <- if(input$meshes_sel_mode == "all_pairwise") {
                        1L
                    } else {
                        n_observers
                    }
                    
                    ## for each file selection element -> read files
                    ll <- lapply(seq_len(n_file_sel), function(i) {
                        input_file_sel <- input[[sprintf("file_sel_%.2d", i)]]
                        if(!is.null(input_file_sel)) {
                            f_files <- input_file_sel$datapath
                            f_names <- input_file_sel$name
                            
                            isorem_TargetLen       <- NULL
                            isorem_FeatureAngleDeg <- NULL
                            isorem_MaxSurfDist     <- NULL
                            isorem_iterations      <- NULL
                            isorem_Adaptive        <- NULL
                            # isorem_relaxSteps      <- NULL
                            #
                            afs_jetSmoothing       <- NULL
                            #
                            sss_scaleIterations    <- NULL
                            sss_neighbors          <- NULL
                            sss_samples            <- NULL
                            sss_separateShells     <- NULL
                            sss_forceManifold      <- NULL
                            sss_borderAngle        <- NULL
                            #
                            pois_normals           <- NULL
                            pois_normalsMethod     <- NULL
                            pois_spacing           <- NULL
                            pois_smAngle           <- NULL
                            pois_smRadius          <- NULL
                            pois_smDistance        <- NULL
                            #
                            ballp_radius           <- NULL
                            ballp_clustering       <- NULL
                            ballp_angle            <- NULL
                            ballp_deleteFaces      <- NULL
                            #
                            alwrap_alphaRel        <- NULL
                            alwrap_offsetRel       <- NULL
                            #
                            smooth_type            <- NULL
                            smooth_iter            <- NULL
                            smooth_lambda          <- NULL
                            smooth_mu              <- NULL
                            smooth_delta           <- NULL
                            
                            ## remeshing options
                            if(!is.null(input$read_mesh_remesh) &&
                               (input$read_mesh_remesh != "No")) {
                                isorem_TargetLen       <- input$read_mesh_remesh_iso_targlen
                                isorem_FeatureAngleDeg <- input$read_mesh_remesh_iso_fang
                                isorem_MaxSurfDist     <- input$read_mesh_remesh_iso_msurfdst
                                isorem_iterations      <- input$read_mesh_remesh_iso_iter
                                isorem_Adaptive        <- input$read_mesh_remesh_iso_adapt
                                # isorem_relaxSteps      <- input$read_mesh_remesh_iso_relstep
                            }
                            
                            ## some surface reconstruction requested
                            if(!is.null(input$read_mesh_reconstruct) &&
                               (input$read_mesh_reconstruct != "No")) {
                                ## method = AFS
                                if(input$read_mesh_reconstruct == "AFS") {
                                    if(input$read_mesh_reconstruct_afs_jetsm_bool) {
                                        afs_jetSmoothing <- input$read_mesh_reconstruct_afs_jetsm_int
                                    }
                                } else if(input$read_mesh_reconstruct == "SSS") {
                                    sss_scaleIterations <- input$read_mesh_reconstruct_sss_scit
                                    sss_neighbors       <- input$read_mesh_reconstruct_sss_neigh
                                    sss_samples         <- input$read_mesh_reconstruct_sss_smpls
                                    sss_separateShells  <- input$read_mesh_reconstruct_sss_sshell
                                    sss_forceManifold   <- input$read_mesh_reconstruct_sss_fmanif
                                    sss_borderAngle     <- input$read_mesh_reconstruct_sss_angle
                                } else if(input$read_mesh_reconstruct == "Poisson") {
                                    pois_normalsMethod <- input$read_mesh_reconstruct_pois_normethod
                                    pois_normals <- if(pois_normalsMethod %in% c("Jet", "PCA")) {
                                        k <- round(input$read_mesh_reconstruct_pois_normals)
                                        stopifnot(k >= 2)
                                        k
                                    } else if(pois_normalsMethod == "VCG") {
                                        NULL
                                    } else {
                                        stop("Invalid Poisson normals method")
                                    }
                                    
                                    pois_spacing <- if(input$read_mesh_reconstruct_pois_spmethod == "avg") {
                                        NULL
                                    } else {
                                        ## method = numeric
                                        val <- input$read_mesh_reconstruct_pois_spacing
                                        stopifnot(val > 0)
                                        val
                                    }
                                    
                                    pois_smAngle     <- input$read_mesh_reconstruct_pois_smang
                                    pois_smRadius    <- input$read_mesh_reconstruct_pois_smrad
                                    pois_smDistance  <- input$read_mesh_reconstruct_pois_smdst
                                } else if(input$read_mesh_reconstruct == "Ball_Pivot") {
                                    ballp_radius      <- input$read_mesh_reconstruct_ballpivot_radius
                                    ballp_clustering  <- input$read_mesh_reconstruct_ballpivot_clust
                                    ballp_angle       <- input$read_mesh_reconstruct_ballpivot_angle
                                    ballp_deleteFaces <- input$read_mesh_reconstruct_ballpviot_delface
                                } else if(input$read_mesh_reconstruct == "Alpha_Wrap") {
                                    alwrap_alphaRel   <- input$read_mesh_reconstruct_alwrap_alphaRel
                                    alwrap_offsetRel  <- input$read_mesh_reconstruct_alwrap_offsetRel
                                }
                            }
                            
                            if(!is.null(input$read_mesh_smooth) &&
                               (input$read_mesh_smooth != "No")) {
                                smooth_type <- input$read_mesh_smooth_type
                                smooth_iter <- input$read_mesh_smooth_iter
                                if(input$read_mesh_smooth_type == "taubin") {
                                    smooth_lambda <- input$read_mesh_smooth_taubin_lambda
                                    smooth_mu     <- input$read_mesh_smooth_taubin_mu
                                } else if(input$read_mesh_smooth_type == "fujiLaplace") {
                                    smooth_delta  <- input$read_mesh_smooth_fujilaplace_delta
                                }
                            }
                            
                            argL <- list(x              =f_files,
                                         name           =f_names,
                                         fix_issues     =input$read_mesh_fix_issues,
                                         #
                                         remesh         =input$read_mesh_remesh,
                                         smooth         =input$read_mesh_smooth,
                                         ##
                                         TargetLen      =isorem_TargetLen,
                                         FeatureAngleDeg=isorem_FeatureAngleDeg,
                                         MaxSurfDist    =isorem_MaxSurfDist,
                                         iterations     =isorem_iterations,
                                         Adaptive       =isorem_Adaptive,
                                         # relaxSteps     =isorem_relaxSteps,
                                         #
                                         reconstruct    =input$read_mesh_reconstruct,
                                         #
                                         jetSmoothing   =afs_jetSmoothing,
                                         #
                                         scaleIterations=sss_scaleIterations,
                                         neighbors      =sss_neighbors,
                                         samples        =sss_samples,
                                         separateShells =sss_separateShells,
                                         forceManifold  =sss_forceManifold,
                                         borderAngle    =sss_borderAngle,
                                         #
                                         normals        =pois_normals,
                                         normalsMethod  =pois_normalsMethod,
                                         spacing        =pois_spacing,
                                         smAngle        =pois_smAngle,
                                         smRadius       =pois_smRadius,
                                         smDistance     =pois_smDistance,
                                         #
                                         radius         =ballp_radius,
                                         clustering     =ballp_clustering,
                                         angle          =ballp_angle,
                                         deleteFaces    =ballp_deleteFaces,
                                         #
                                         alphaRel       =alwrap_alphaRel,
                                         offsetRel      =alwrap_offsetRel,
                                         #
                                         type           =smooth_type,
                                         iteration      =smooth_iter,
                                         lambda         =smooth_lambda,
                                         mu             =smooth_mu,
                                         delta          =smooth_delta)
                            
                            do.call("read_mesh_obs", Filter(Negate(is.null), argL))
                        } else {
                            NULL
                        }
                    })
                    
                    ll <- Filter(Negate(is.null), ll)
                    if(input$meshes_sel_mode == "all_pairwise") {
                        meshL_to_observerL(unlist(ll, recursive=FALSE))
                    } else {
                        setNames(ll, sprintf("Observer_%.2d", seq_along(ll)))
                    }
                }
                
                meshL
            })
        })
        react_file_sel_sorted <- reactive({
            meshL <- react_file_sel()
            if(!is.null(meshL)) {
                ll <- lapply(seq_along(meshL), function(i) {
                    observer <- meshL[[i]]
                    ranklist <- input[[sprintf("ranklist_obs%.2d", i)]]
                    observer_sorted <- if(!is.null(ranklist)) {
                        observer[ranklist]
                    } else {
                        observer
                    }
                })
                
                setNames(ll, names(meshL))
            } else {
                NULL
            }
        })
        react_mesh_ui <- reactive({
            meshL <- react_file_sel_sorted()
            if(!is.null(meshL)) {
                get_mesh_ui(meshL)
            } else {
                NULL
            }
        })
        react_mesh_metro <- reactive({
            meshL <- react_file_sel_sorted()
            if(!is.null(meshL)) {
                valid_nSamples <- !is.null(input$vcgMetro_nSamples) &&
                                  (input$vcgMetro_nSamples >= 0L)
                
                valid_nSamplesArea <- !is.null(input$vcgMetro_nSamplesArea) &&
                    (input$vcgMetro_nSamplesArea >= 0L)

                nSamples <- if(valid_nSamples) {
                    input$vcgMetro_nSamples
                } else {
                    0L
                }
                
                nSamplesArea <- if(valid_nSamples) {
                    input$vcgMetro_nSamplesArea
                } else {
                    0L
                }

                argL <- list(meshL,
                             chop        =TRUE,
                             silent      =FALSE,
                             colormeshes =TRUE,
                             nSamples    =nSamples,
                             nSamplesArea=nSamplesArea,
                             vertSamp    =input$vcgMetro_vertSamp,
                             edgeSamp    =input$vcgMetro_edgeSamp,
                             faceSamp    =input$vcgMetro_faceSamp,
                             unrefVert   =input$vcgMetro_unrefVert,
                             samplingTyp =input$vcgMetro_samplingTyp,
                             searchStruct=input$vcgMetro_searchStruct,
                             from        =input$vcgMetro_from,
                             to          =input$vcgMetro_to)
                
                do.call("get_mesh_metro", Filter(Negate(is.null), argL))
            } else {
                NULL
            }
        })
        ## list of pairwise meshes
        react_mesh_agree <- reactive({
            meshL  <- react_file_sel_sorted()
            metroL <- react_mesh_metro()
            uiL    <- if(!is.null(input$mesh_agree_do_ui) && input$mesh_agree_do_ui) {
                react_mesh_ui()
            } else {
                list(NULL)
            }

            if(!is.null(meshL) && !is.null(metroL)) {
                do_ui       <- !is.null(input$mesh_agree_do_ui) && input$mesh_agree_do_ui
                mesh_pairL  <- get_mesh_pairs(meshL)
                agree_pairL <- Map(get_mesh_agree_pair,
                                   mesh_pairL,
                                   metro=metroL,
                                   ui   =uiL,
                                   do_ui=do_ui)
                
                d <- do.call("rbind", agree_pairL)
                rownames(d) <- NULL
                d
            } else {
                NULL
            }
        })
        output$ui_mesh_agree_ui <- renderUI({
            tagList(p("Volume-overlap based metrics (DSC, JSC) take more time to compute than distance-based metrics."),
                    checkboxInput("mesh_agree_do_ui",
                                  "Calculate DSC, JSC",
                                  value=TRUE))
        })
        output$ui_select_comparisons <- renderUI({
            if(input$meshes_sel_mode == "indiv") {
                numericInput("num_observers", "Number of observers", min=2L, value=2L, step=1L)
            } else {
                NULL
            }
        })
        output$ui_import_fix_note <- renderUI({
            if(input$meshes_input_source == "builtin") {
                p("Mesh transformation options are available when uploading 3D mesh files")
            } else {
                NULL
            }
        })
        output$ui_import_fix <- renderUI({
            if(input$meshes_input_source == "file") {
                tagList(checkboxInput("read_mesh_fix_issues", "Try to fix mesh issues on import?", value=TRUE))
            } else {
                NULL
            }
        })
        output$ui_smooth <- renderUI({
            if((input$meshes_input_source == "file")) {
                radioButtons("read_mesh_smooth",
                             "Smoothing method",
                             choices=c("None"="No", "VCG"="VCG"),
                             selected="No",
                             inline=TRUE)
            } else {
                NULL
            }
        })
        output$ui_smooth_vcg_opts <- renderUI({
            if((input$meshes_input_source == "file") &&
               !is.null(input$read_mesh_smooth)      &&
               (input$read_mesh_smooth == "VCG")) {
                tagList(radioButtons("read_mesh_smooth_type",
                                     "Algorithm",
                                     list("Taubin"="taubin",
                                          "Laplace"="laplace",
                                          "Improved Laplace"="HClaplace",
                                          "Scale dependent Laplace"="fujiLaplace",
                                          "Laplace angle weighted"="angWeight"),
                                     inline=TRUE),
                        numericInput("read_mesh_smooth_iter",
                                     "Number of iterations",
                                     min=1L,
                                     value=10L,
                                     step=1L))
            } else {
                NULL
            }
        })
        output$ui_smooth_vcg_taubin_opts <- renderUI({
            if((input$meshes_input_source == "file") &&
               !is.null(input$read_mesh_smooth)      &&
               (input$read_mesh_smooth == "VCG")     &&
               !is.null(input$read_mesh_smooth_type) &&
               (input$read_mesh_smooth_type == "taubin")) {
                tagList(numericInput("read_mesh_smooth_taubin_lambda",
                                     "lambda",
                                     min=0L,
                                     value=0.5,
                                     step=0.05),
                        numericInput("read_mesh_smooth_taubin_mu",
                                     "mu",
                                     min=-100,
                                     value=-0.53,
                                     step=0.01))
            } else {
                NULL
            }
        })
        output$ui_smooth_vcg_fujilaplace_opts <- renderUI({
            if((input$meshes_input_source == "file") &&
               !is.null(input$read_mesh_smooth)      &&
               (input$read_mesh_smooth == "VCG")     &&
               !is.null(input$read_mesh_smooth_type) &&
               (input$read_mesh_smooth_type == "fujiLaplace")) {
                numericInput("read_mesh_smooth_fujilaplace_delta",
                             "delta (angle in rad)",
                             min=-pi,
                             value=0.1,
                             step=0.1)
            } else {
                NULL
            }
        })
        output$ui_remesh <- renderUI({
            if((input$meshes_input_source == "file")) {
                radioButtons("read_mesh_remesh",
                             "Remesh method",
                             choices=c("None"="No", "Isotropic"="Isotropic"),
                             selected="No",
                             inline=TRUE)
            } else {
                NULL
            }
        })
        output$ui_remesh_iso_opts <- renderUI({
            if((input$meshes_input_source == "file") &&
               !is.null(input$read_mesh_remesh)  &&
               (input$read_mesh_remesh != "No")) {
                tagList(numericInput("read_mesh_remesh_iso_targlen",
                                     "Target edge length (lower -> more expensive)",
                                     min=0.01,
                                     value=1,
                                     step=0.01),
                        numericInput("read_mesh_remesh_iso_fang",
                                     "Crease angle (deg)",
                                     min=0L,
                                     value=10L,
                                     step=1L),
                        numericInput("read_mesh_remesh_iso_msurfdst",
                                     "Maximum surface distance",
                                     min=0L,
                                     value=1,
                                     step=0.05),
                        numericInput("read_mesh_remesh_iso_iter",
                                     "Iterations",
                                     min=1L,
                                     value=1L,
                                     step=1L),
                        # numericInput("read_mesh_remesh_iso_relstep",
                        #              "Relax steps",
                        #              min=1L,
                        #              value=1L,
                        #              step=1L),
                        checkboxInput("read_mesh_remesh_iso_adapt",
                                      "Enable adaptive remeshing?",
                                      value=FALSE)
                        )
            } else {
                NULL
            }
        })
        output$ui_reconstruct <- renderUI({
            if((input$meshes_input_source == "file")) {
                radioButtons("read_mesh_reconstruct",
                             "Surface reconstruction",
                             choices=c("None"="No",
                                       "AFS"="AFS",
                                       "SSS"="SSS", 
                                       "Poisson"="Poisson",
                                       "Ball Pivoting"="Ball_Pivot",
                                       "Alpha Wrap"="Alpha_Wrap"),
                             selected="No",
                             inline=TRUE)
            } else {
                NULL
            }
        })
        output$ui_reconstruct_cave <- renderUI({
            if(!is.null(input$meshes_input_source)          &&
               (input$meshes_input_source   == "file")      &&
               !is.null(input$read_mesh_reconstruct)        &&
               (input$read_mesh_reconstruct != "No")) {
                p("Cave: Surface reconstruction enabled. Please visually validate results using 'View meshes'.",
                  id="asdf",
                  style="font-weight:bold;color:red;")
            } else {
                NULL
            }
        })
        output$ui_reconstruct_sss_opts <- renderUI({
            if(!is.null(input$meshes_input_source)     &&
               !is.null(input$read_mesh_reconstruct)   &&
               (input$meshes_input_source   == "file") &&
               (input$read_mesh_reconstruct == "SSS")) {
                tagList(numericInput("read_mesh_reconstruct_sss_scit",
                                     "Scale Iterations",
                                     value=1L, min=1L, step=1L),
                        numericInput("read_mesh_reconstruct_sss_neigh",
                                     "Neighbors",
                                     value=12L, min=1L, step=1L),
                        numericInput("read_mesh_reconstruct_sss_smpls",
                                     "Samples",
                                     value=300L, min=1L, step=1L),
                        checkboxInput("read_mesh_reconstruct_sss_sshell",
                                      "Separate Shells",
                                      value=FALSE),
                        checkboxInput("read_mesh_reconstruct_sss_fmanif",
                                      "Force Manifold",
                                      value=TRUE),
                        numericInput("read_mesh_reconstruct_sss_angle",
                                     "Border Angle",
                                     value=45L, step=1L))
            } else {
                NULL
            }
        })
        output$ui_reconstruct_pois_method <- renderUI({
            if(!is.null(input$meshes_input_source)     &&
               !is.null(input$read_mesh_reconstruct)   &&
               (input$meshes_input_source   == "file") &&
               (input$read_mesh_reconstruct == "Poisson")) {
                tagList(radioButtons("read_mesh_reconstruct_pois_normethod",
                                     "Normals method",
                                     choices=c("VCG", "Jet", "PCA"),
                                     selected="VCG",
                                     inline=TRUE),
                        radioButtons("read_mesh_reconstruct_pois_spmethod",
                                     "Spacing: Average or numeric",
                                     choices=c("Average"="avg", "numeric -> positive number"="num"),
                                     selected="avg",
                                     inline=TRUE))
            } else {
                NULL
            }
        })
        output$ui_reconstruct_pois_opts <- renderUI({
            if(!is.null(input$meshes_input_source)               &&
               !is.null(input$read_mesh_reconstruct)             &&
               (input$meshes_input_source   == "file")           &&
               (input$read_mesh_reconstruct == "Poisson")        &&
               !is.null(input$read_mesh_reconstruct_pois_normethod) &&
               !is.null(input$read_mesh_reconstruct_pois_spmethod)) {
                
                ui_pois_normals <- if(input$read_mesh_reconstruct_pois_normethod == "VCG") {
                    NULL
                } else {
                    numericInput("read_mesh_reconstruct_pois_normals",
                                 "Normals Parameter",
                                 min=2L,
                                 value=12L,
                                 step=1L)
                }
                ui_pois_spacing <- if(input$read_mesh_reconstruct_pois_spmethod == "avg") {
                    NULL
                } else {
                    spacing_min     <- 0.001
                    spacing_default <- 2
                    spacing_step    <- 0.1
                    numericInput("read_mesh_reconstruct_pois_spacing",
                                 "Spacing Parameter",
                                 min=spacing_min,
                                 value=spacing_default,
                                 step=spacing_step)
                }
                ## normals
                tagList(ui_pois_normals,
                        ui_pois_spacing,
                        numericInput("read_mesh_reconstruct_pois_smang",
                                     "SM Angle",
                                     min=0L,
                                     value=20L,
                                     step=1L),
                        numericInput("read_mesh_reconstruct_pois_smrad",
                                     "SM Radius",
                                     min=0,
                                     value=30),
                        numericInput("read_mesh_reconstruct_pois_smdst",
                                     "SM Distance",
                                     min=0,
                                     value=0.375))
            } else {
                NULL
            }
        })
        output$ui_reconstruct_ballpivot_opts <- renderUI({
            if(!is.null(input$meshes_input_source)     &&
               !is.null(input$read_mesh_reconstruct)   &&
               (input$meshes_input_source   == "file") &&
               (input$read_mesh_reconstruct == "Ball_Pivot")) {
                tagList(numericInput("read_mesh_reconstruct_ballpivot_radius",
                                     "Radius",
                                     min=0,
                                     value=0,
                                     step=0.01),
                        numericInput("read_mesh_reconstruct_ballpivot_clust",
                                     "Clustering",
                                     min=0.01,
                                     value=0.2,
                                     step=0.01),
                        numericInput("read_mesh_reconstruct_ballpivot_angle",
                                     "Angle (rad)",
                                     min=0.01,
                                     value=round(pi/2, 3),
                                     step=0.01),
                        checkboxInput("read_mesh_reconstruct_ballpviot_delface",
                                     "Delete Faces?",
                                     value=FALSE))
            } else {
                NULL
            }
        })
        output$ui_reconstruct_alwrap_opts <- renderUI({
            if(!is.null(input$meshes_input_source)     &&
               !is.null(input$read_mesh_reconstruct)   &&
               (input$meshes_input_source   == "file") &&
               (input$read_mesh_reconstruct == "Alpha_Wrap")) {
                tagList(numericInput("read_mesh_reconstruct_alwrap_alphaRel",
                                     "Alpha (relative to bounding box)",
                                     min=0.01,
                                     value=5,
                                     step=1),
                        numericInput("read_mesh_reconstruct_alwrap_offsetRel",
                                     "Offset (relative to bounding box)",
                                     min=0.01,
                                     value=5,
                                     step=1))
            } else {
                NULL
            }
        })
        output$ui_reconstruct_afs_jetsm_bool <- renderUI({
            if(!is.null(input$meshes_input_source)     &&
               !is.null(input$read_mesh_reconstruct)   &&
               (input$meshes_input_source   == "file") &&
               (input$read_mesh_reconstruct == "AFS")) {
                checkboxInput("read_mesh_reconstruct_afs_jetsm_bool",
                              "Jet Smoothing for AFS reconstruction?",
                              value=FALSE)
            } else {
                NULL
            }
        })
        output$ui_reconstruct_afs_jetsm_int <- renderUI({
            if(!is.null(input$meshes_input_source)               &&
               !is.null(input$read_mesh_reconstruct)             &&
               (input$meshes_input_source   == "file")           &&
               (input$read_mesh_reconstruct == "AFS")            &&
               !is.null(input$read_mesh_reconstruct_afs_jetsm_bool) &&
               (input$read_mesh_reconstruct_afs_jetsm_bool)) {
                numericInput("read_mesh_reconstruct_afs_jetsm_int",
                             "Jet Smoothing integer for AFS reconstruction",
                             min=2L,
                             value=2L,
                             step=1L)
            } else {
                NULL
            }
        })
        output$ui_select_files <- renderUI({
            if(input$meshes_input_source == "file") {
                if(input$meshes_sel_mode == "all_pairwise") {
                    n_observers <- 1L
                    sel_label   <- ""
                } else {
                    valid_n <- !is.null(input$num_observers) &&
                               (input$num_observers >= 2L)   &&
                               (input$num_observers <=100L)
                    
                    n_observers <- if(!valid_n) {
                        2L
                    } else {
                        round(input$num_observers)
                    }
                    
                    sel_label <- paste0(" (Observer ", sprintf("%.2d", seq_len(n_observers)), ")")
                }
                
                file_selL <- lapply(seq_len(n_observers), function(i) {
                    finput_id    <- sprintf("file_sel_%.2d", i)
                    finput_label <- paste0("Select files", sel_label[i], ":")
                    column(width=12/n_observers,
                           fileInput(finput_id,
                                     finput_label,
                                     width="100%",
                                     multiple=TRUE))
                })
                
                ## weed out NULL components, convert the list to a tagList and return
                file_selL <- Filter(Negate(is.null), file_selL)
                fluidRow(do.call(tagList, file_selL))
            } else {
                NULL
            }
        })
        output$ui_ranklist_files <- renderUI({
            input$apply_file_sel
            isolate({
                if((input$meshes_sel_mode == "indiv")) {
                    n_observers <- if(is.null(input$num_observers)) {
                        2L
                    } else {
                        input$num_observers
                    }
                    
                    meshL <- react_file_sel()
                    if(!is.null(meshL)) {
                        n_obs_max <- min(c(length(meshL), n_observers))
                        ranklistL <- lapply(seq_len(n_obs_max), function(i) {
                            ranklist_ui_name <- sprintf("ui_ranklist_obs%.2d", i)
                            ranklist_inputid <- sprintf("ranklist_obs%.2d",    i)
                            meshL_obs <- meshL[[i]]
                            ranklist_labels <- vapply(meshL_obs, function(x) { x[["name"]] }, character(1))
                            column(width=12/n_obs_max,
                                   rank_list(sprintf("Files Observer %.2d", i),
                                             labels=ranklist_labels,
                                             input_id=ranklist_inputid))
                        })
                        
                        ## weed out NULL components, convert the list to a tagList and return
                        ranklistL <- Filter(Negate(is.null), ranklistL)
                        tagList(fluidRow(column(width=12,
                                                p("Drag-and-drop fifle names to define comparison sets.",
                                                  "All first elements are compared to each other between observers, and so on."))),
                                fluidRow(do.call(tagList, ranklistL)))
                    } else {
                        NULL
                    }
                } else {
                    NULL
                }
            })
        })
        output$table_ui_compare <- DT::renderDataTable({
            meshL <- react_file_sel_sorted()
            if(!is.null(meshL)) {
                pairL <- get_mesh_pairs(meshL, names_only=TRUE)
                DT::datatable(data.frame(Comparison=names(pairL)))
            } else {
                NULL
            }
        })
        output$table_mesh_info <- DT::renderDataTable({
            meshL <- react_file_sel_sorted()
            if(!is.null(meshL)) {
                d_mesh_info <- get_mesh_info(meshL)
                cols_numeric <- unname(which(vapply(d_mesh_info, is.numeric, logical(1))))
                DT_out <- DT::datatable(d_mesh_info,
                                        extensions="Buttons",
                                        options=list(dom='Bfrtip',
                                                     buttons=c("csv", "excel")))
                DT::formatRound(DT_out, columns=cols_numeric, digits=2)
            } else {
                NULL
            }
        })
        output$ui_mesh_agree_metro_options <- renderUI({
            tagList( numericInput("vcgMetro_nSamples",     "Number of samples (0 for automatic setting)", value=0L, min=0L, step=1L),
                     numericInput("vcgMetro_nSamplesArea", "Number of samples per area (overrides nSamples)", value=0L, min=0L, step=1L),
                    checkboxInput("vcgMetro_vertSamp",     "Vertex sampling",            value=TRUE),
                    checkboxInput("vcgMetro_edgeSamp",     "Edge sampling",              value=TRUE),
                    checkboxInput("vcgMetro_faceSamp",     "Face sampling",              value=TRUE),
                    checkboxInput("vcgMetro_unrefVert",    "Ignore unreferred vertices", value=FALSE),
                     radioButtons("vcgMetro_samplingTyp",  "Face sampling mode", choices=c("SS", "MC", "SD"), selected="SS", inline=TRUE),
                     radioButtons("vcgMetro_searchStruct", "Search structure",   choices=c("SGRID", "AABB", "OCTREE", "HGRID"), selected="SGRID", inline=TRUE),
                     numericInput("vcgMetro_from",         "Color mapping: minimum", value=0, min=0),
                     numericInput("vcgMetro_to",           "Color mapping: maximum", value=0, min=0)
            )
        })
        output$rgl_view_selection <- renderUI({
            meshL <- react_file_sel_sorted()
            if(!is.null(meshL)) {
                pairL <- get_mesh_pairs(meshL, names_only=TRUE)
                selectInput("rgl_view_select",
                            "Select mesh pair",
                            choices=names(pairL),
                            multiple=FALSE)
            } else {
                NULL
            }
        })
        output$rgl_mesh1_name <- renderUI({
            p(get_name_elem(input$rgl_view_select, pos=1L))
        })
        output$rgl_mesh2_name <- renderUI({
            p(get_name_elem(input$rgl_view_select, pos=2L))
        })
        output$rgl_dist1_name <- renderUI({
            p(get_name_elem(input$rgl_view_select, pos=1L))
        })
        output$rgl_dist2_name <- renderUI({
            p(get_name_elem(input$rgl_view_select, pos=2L))
        })
        output$table_agree_pairwise <- DT::renderDataTable({
            # input$apply_compare
            # isolate({
                d_agree_pairW <- react_mesh_agree()
                
                if(!is.null(d_agree_pairW)) {
                    cols_numeric <- unname(which(vapply(d_agree_pairW, is.numeric, logical(1))))
                    DT_out <- DT::datatable(d_agree_pairW,
                                            extensions="Buttons",
                                            options=list(dom='Bfrtip',
                                                         buttons=c("csv", "excel")))
                    DT::formatRound(DT_out, columns=cols_numeric, digits=2)
                } else {
                    NULL
                }
            # })
        })
        output$table_agree_aggr <- DT::renderDataTable({
            # input$apply_compare
            # isolate({
                d_agree_pairW <- react_mesh_agree()
                if(!is.null(d_agree_pairW)) {
                    d_agree_aggr <- get_mesh_agree_aggr(d_agree_pairW)                    
                    cols_numeric <- unname(which(vapply(d_agree_aggr, is.numeric, logical(1))))
                    DT_out <- DT::datatable(d_agree_aggr,
                                            extensions="Buttons",
                                            options=list(dom='Bfrtip',
                                                         buttons=c("csv", "excel")))
                    DT::formatRound(DT_out, columns=cols_numeric, digits=2)
                } else {
                    NULL
                }
            # })
        })
        output$diag_agree_pairwise <- renderPlotly({
            d_agree_pairW <- react_mesh_agree()
            
            if(!is.null(d_agree_pairW)) {
                d_agree_pairL <- get_mesh_agree_long(d_agree_pairW)
                d_agree_pairL[["pair"]] <- paste(d_agree_pairL[["mesh_1"]],
                                                 d_agree_pairL[["mesh_2"]],
                                                 sep=" <-> ")
                p <- ggplot(d_agree_pairL, aes(x=pair, y=observed)) +
                    geom_point() +
                    facet_grid(metric ~ ., scales="free_y") +
                    xlab(NULL) +
                    ylab(NULL) +
                    theme_bw()
                
                ggplotly(p, height=600)
            } else {
                NULL
            }
            # })
        })
        output$diag_agree_aggr <- renderPlotly({
            d_agree_pairW <- react_mesh_agree()
            
            if(!is.null(d_agree_pairW)) {
                d_agree_aggrW <- get_mesh_agree_aggr(d_agree_pairW)
                d_agree_aggrL <- get_mesh_agree_aggr_long(d_agree_aggrW)
                # metric, statistic, observed
                p <- ggplot(d_agree_aggrL,
                            aes(x=statistic, y=observed,
                                group=group, color=group)) +
                    geom_point(position=position_dodge(w=0.2)) +
                    facet_grid(metric ~ ., scales="free_y") +
                    xlab(NULL) +
                    ylab(NULL) +
                    theme_bw()
                
                ggplotly(p, height=600)
            } else {
                NULL
            }
            # })
        })
        output$rgl_mesh1 <- renderRglwidget({
            meshL       <- react_file_sel_sorted()
            view_select <- input$rgl_view_select
            if(!is.null(meshL) && !is.null(view_select)) {
                pairL <- get_mesh_pairs(meshL, names_only=FALSE)
                mesh  <- pairL[[view_select]][["mesh_1"]]
                if(!is.null(mesh)) {
                    try(close3d())
                    wire3d(MeshUtils::toRGL(mesh[["mesh"]]))
                    rglwidget()
                } else {
                    NULL
                }
            } else {
                NULL
            }
        })
        output$rgl_mesh2 <- renderRglwidget({
            meshL       <- react_file_sel_sorted()
            view_select <- input$rgl_view_select
            if(!is.null(meshL) && !is.null(view_select)) {
                pairL <- get_mesh_pairs(meshL, names_only=FALSE)
                mesh  <- pairL[[view_select]][["mesh_2"]]
                if(!is.null(mesh)) {
                    try(close3d())
                    wire3d(MeshUtils::toRGL(mesh[["mesh"]]))
                    rglwidget()
                } else {
                    NULL
                }
            } else {
                NULL
            }
        })
        output$rgl_mesh_dist1 <- renderRglwidget({
            metroL      <- react_mesh_metro()
            view_select <- input$rgl_view_select

            if(!is.null(metroL) && !is.null(view_select)) {
                metro <- metroL[[view_select]]
                if(!is.null(metro)) {
                    try(close3d())
                    shade3d(metro[["mesh_1"]])
                    rglwidget()
                } else {
                    NULL
                }
            } else {
                NULL
            }
        })
        output$rgl_mesh_dist2 <- renderRglwidget({
            metroL      <- react_mesh_metro()
            view_select <- input$rgl_view_select

            if(!is.null(metroL) && !is.null(view_select)) {
                metro <- metroL[[view_select]]
                if(!is.null(metro)) {
                    try(close3d())
                    shade3d(metro[["mesh_2"]])
                    rglwidget()
                } else {
                    NULL
                }
            } else {
                NULL
            }
        })
    }
)
