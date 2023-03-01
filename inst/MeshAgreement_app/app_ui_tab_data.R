fluidPage(
    fluidRow(
        column(
            width=12,
            tabBox(id="tabbox_data",
                   title=NULL, # "Select 3D mesh data",
                width=12,
                # height="650px",
                # closable=FALSE,
                # maximizable=FALSE,
                # collapsible=FALSE,
                # status=NULL,
                status="primary",
                solidHeader=FALSE,
                type="tabs",
                selected="3D Mesh Data",
                tabPanel(title="3D Mesh Data",
                         tagList(radioButtons("meshes_input_source",
                                              label="Data Source",
                                              list("Use built-in data"="builtin",
                                                   "Upload 3D mesh files (Supported file formats: STL, PLY, OBJ, OFF)"="file"),
                                              inline=TRUE),
                                 radioButtons("meshes_sel_mode",
                                              label="Comparison Mode",
                                              list("All pairwise comparisons"="all_pairwise",
                                                   "Define comparisons individually"="indiv"),
                                              inline=TRUE),
                                 uiOutput("ui_select_comparisons"),
                                 uiOutput("ui_select_files"))),
                tabPanel(title="Options for transforming meshes",
                         tagList(uiOutput("ui_import_fix_note"),
                                 uiOutput("ui_import_fix"),
                                 uiOutput("ui_iso_remesh_opts"),
                                 uiOutput("ui_reconstruct_when"),
                                 uiOutput("ui_reconstruct_method"),
                                 uiOutput("ui_reconstruct_afs_jetsm_bool"),
                                 uiOutput("ui_reconstruct_afs_jetsm_int"),
                                 uiOutput("ui_reconstruct_sss_opts"),
                                 uiOutput("ui_reconstruct_pois_method"),
                                 uiOutput("ui_reconstruct_pois_opts"),
                                 uiOutput("ui_reconstruct_ballpivot_opts")))
            )
        )
    ),
    fluidRow(
        column(
            width=12,
            box(title=NULL,
                id="apply_box",
                width=12,
                headerBorder=FALSE,
                # height="650px",
                status=NULL,
                closable=FALSE,
                maximizable=FALSE,
                collapsible=FALSE,
                uiOutput("ui_reconstruct_cave"),
                actionButton("apply_file_sel", "Apply")
            )
        )
    ),
    fluidRow(
        column(
            width=12,
            box(title="Comparisons",
                width=12,
                # height="650px",
                status=NULL,
                closable=FALSE,
                maximizable=FALSE,
                collapsible=FALSE,
                uiOutput("ui_ranklist_files"),
                DT::dataTableOutput("table_ui_compare")
            )
        )
    ),
    fluidRow(
        column(
            width=12,
            box(title="Information on selected meshes",
                width=12,
                # height="650px",
                status=NULL,
                closable=FALSE,
                maximizable=FALSE,
                collapsible=FALSE,
                p("Note: If information on mesh volume and mesh centroid are missing ('NA'), the mesh is not proper.",
                  "This means that the mesh may not be closed, may not bound a volume, may have self-intersections, or may contain duplicated faces.",
                  "Some agreement measures will then be unavailable.",
                  "Ticking the 'fix mesh issues' box or using surface reconstruction on import may help.",
                  "Otherwise, inspection of the mesh with a tool such as",
                  tags$a(href="https://www.meshlab.net/", "MeshLab"), "is advised."),
                DT::dataTableOutput("table_mesh_info")
            )
        )
    )
)
