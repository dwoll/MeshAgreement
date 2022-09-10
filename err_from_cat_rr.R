library(dplyr)
library(readxl)

source("metaanalysis_global.R")

#####---------------------------------------------------------------------------
## do all expensive calculations
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## read in data for different cancer sites
excel_file   <- "SPCaRT_Chapter5_Epidemiology_incomplete first draft_TABLES_08.07.2022.xlsx"
excel_sheets <- list(HaemLym="Tab_5_B_2_Haem_Lymph_meta",
                     Sarcoma="Tab_5_B_3_Sarcoma_meta",
                     Breast ="Tab_5_B_4_Breast_meta",
                     Lung   ="Tab_5_B_5_Lung_meta",
                     Gastro ="Tab_5_B_6_Gastro_meta",
                     Thyroid="Tab_5_B_7_Thyroid_meta",
                     Brain  ="Tab_5_B_8_Brain_meta")

d_RR0_allL <- Map(function(s) { read_xlsx(excel_file, sheet=s) }, excel_sheets)

#####---------------------------------------------------------------------------
## remove studies with overlapping data
sens_overlappingL <- list(HaemLym=c("{Boice, 1987, 3480381}",
                                    "{Blettner, 1991, 1947508}",
                                    "{Holowaty_ANLL, 1995, 8625159}",
                                    "{Holowaty_NCLL, 1995, 8625159}",
                                    "{Allodji, 2020, 33105035}",
                                    "{Storm, 1985, 3932230}"),
                          Sarcoma=c("{Le Vu, 1998, 9663598}"),
                          Breast =c("{Inskip, 2009, 19620485}",      # overlap Veiga 2019
                                    "{Schonfeld, 2020, 31794291}",   # overlap Veiga 2019
                                    "{van Leeuwen, 2003, 12837833}", # overlap Travis 2003
                                    "{Rubino, 2003, 12942115}"),     # overlap Guibout 2005
                          Lung   =c("{Travis, 2002, 11830608}",
                                    "{Taylor, 2017, 28319436}"),
                          Gastro =c("{Gilbert, 2017, 28118119}",
                                    "{Hauptmann, 2015, 25349972}",
                                    "{Kleinerman, 2013, 23707149}",
                                    "{Morton, 2013, 23980092}",
                                    "{de Vathaire, 1997, 9043033}",
                                    "{Rubino, 2003, 14583762}",
                                    "{Rubino, 2005, 15990012}"),
                          Thyroid=c("{de Vathaire, 1999, 10597762}",
                                    "{Ron, 1995, 7871153}",
                                    "{RubinoA, 2003, 12942115}",
                                    "{Sigurdson, 2005, 15950715}",
                                    "{Chow, 2009, 19459201}",
                                    "{Ronckers, 2006, 17007558}",
                                    "{Veiga, 2012, 22857014}"),
                          Brain  =c("{Little, 1998, 9766556}",
                                    "{Schonfeld, 2020, 31794291}"))

## check that manuscript identifiers match those used in data Excel table
Map(function(x, y) { y[!(y %in% x$Reference)] }, d_RR0_allL, sens_overlappingL)

#####---------------------------------------------------------------------------
## studies with only children
sens_child_onlyL <- list(HaemLym=c("{Allodji, 2015, 26461008}",
                                   "{Tucker, 1987, 3469460}",
                                   "{Hawkins, 1992, 1581717}",
                                   "{Haddy, 2006, 16965909}",
                                   "{Allard, 2010, 20303670}",
                                   "{Allodji, 2020, 33105035}"),
                         Sarcoma=c("{Bechler, 1992, 1522094}",
                                   "{Hawkins, 1996, 8614005}",
                                   "{Henderson, 2012, 22795729}",
                                   "{Jenkinson, 2007, 17653071}",
                                   "{Kuttesch, 1996, 8874344}",
                                   "{Le Vu, 1998, 9663598}",
                                   "{Menu-Branthomme, 2004, 15054872}",
                                   "{Schwartz, 2014, 24419490}",
                                   "{Strong, 1979, 220452}",
                                   "{Svahn-Tapper, 2006, 16760180}",
                                   "{Tucker, 1987, 3475572}",
                                   "{Wong, 1997, 9333268}"),
                         Breast =c("{Guibout, 2005, 15625374}",
                                   "{Inskip, 2009, 19620485}",
                                   "{Rubino, 2003, 12942115}",
                                   "{Svahn-Tapper, 2006, 16760180}",
                                   "{Schonfeld, 2020, 31794291}",
                                   "{Veiga, 2019, 31657853}"),
                         Lung   =c(),
                         Gastro =c("{Allodji, 2019, 30345604}",
                                   "{Nottage, 2012, 22665546}"),
                         Thyroid=c("{Bhatti, 2010, 21128798}",
                                   "{de Vathaire, 1999, 10597762}",
                                   "{de Vathaire, 2015, 26327481}",
                                   "{RubinoA, 2003, 12942115}",
                                   "{Sigurdson, 2005, 15950715}",
                                   "{Svahn-Tapper, 2006, 16760180}",
                                   "{Chow, 2009, 19459201}",
                                   "{Finke, 2015, 26517987}",
                                   "{Ronckers, 2006, 17007558}",
                                   "{RubinoB, 2003, 14566829}",
                                   "{Veiga, 2012, 22857014}",
                                   "{Tucker, 1991, 1851664}"),
                         Brain  =c("{Bhatia, 2002, 12036851}",
                                   "{Little, 1998, 9766556}",
                                   "{Neglia, 2006, 17077355}",
                                   "{Svahn-Tapper, 2006, 16760180}",
                                   "{Taylor, 2010, 21079138}",
                                   "{Lindberg, 1995, 7576739}",
                                   "{Schonfeld, 2020, 31794291}"))

## check that manuscript identifiers match those used in data Excel table
Map(function(x, y) { y[!(y %in% x$Reference)] }, d_RR0_allL, sens_child_onlyL)

#####---------------------------------------------------------------------------
## studies with only adults
sens_adult_onlyL <- list(HaemLym=c("{Storm, 1985, 3932230}",
                                   "{Lonn, 2010, 20142245}",
                                   "{Boice_NHL, 1988, 3186929}",
                                   "{Boice_MM, 1988, 3186929}",
                                   "{Murohashi, 1985, 4068751}",
                                   "{Travis, 1994, 8089863}",
                                   "{Curtis, 1994, 8064889}",
                                   "{Curtis, 1989, 2909667}",
                                   "{Curtis, 1992, 1594016}",
                                   "{Boice, 1987, 3480381}",
                                   "{Blettner, 1991, 1947508}",
                                   "{Holowaty_ANLL, 1995, 8625159}",
                                   "{Holowaty_NCLL, 1995, 8625159}"),
                         Sarcoma=c("{Boice, 1988, 3186929}",
                                   "{Rubino, 2003, 14583762}",
                                   "{Rubino, 2005, 15754127}"),
                         Breast =c("{Boice, 1989, 2744900}",
                                   "{Hooning, 2008, 18854572}",
                                   "{Storm, 1992, 1640483}",
                                   "{Stovall, 2008, 18556141}",
                                   "{Boice, 1992, 1538720}"),
                         Lung   =c("{Grantzau, 2014, 24909095}"),
                         Gastro =c("{Boice_Stomach, 1988, 3186929}",
                                   "{Boice_Colon, 1988, 3186929}",
                                   "{Boice_Rectum, 1988, 3186929}",
                                   "{Hauptmann, 2015, 25349972}",
                                   "{Lonn_Oesophagus, 2010, 20142245}",
                                   "{Lonn_Stomach, 2010, 20142245}",
                                   "{Lonn_SmallInt, 2010, 20142245}",
                                   "{Lonn_Colon, 2010, 20142245}",
                                   "{Lonn_Rectum, 2010, 20142245}",
                                   "{Kleinerman, 2013, 23707149}"),
                         Thyroid=c("{Adjadj, 2003, 12973856}",
                                   "{Boice, 1988, 3186929}",
                                   "{Lonn, 2010, 20142245}"),
                         Brain  =c())

## check that manuscript identifiers match those used in data Excel table
Map(function(x, y) { y[!(y %in% x$Reference)] }, d_RR0_allL, sens_adult_onlyL)

#####---------------------------------------------------------------------------
## studies with patients without chemotherapy
sens_chemo_noL <- list(HaemLym=c("{Storm, 1985, 3932230}",
                                 "{Boice_NHL, 1988, 3186929}",
                                 "{Boice_MM, 1988, 3186929}",
                                 "{Murohashi, 1985, 4068751}",
                                 "{Boice, 1987, 3480381}",
                                 "{Blettner, 1991, 1947508}"),
                       Sarcoma=c("{Boice, 1988, 3186929}",
                                 "{Rubino, 2003, 14583762}",
                                 "{Wong, 1997, 9333268}"),
                       Breast =c("{Adjadj, 2003, 12973856}",
                                 "{Boice, 1989, 2744900}",
                                 "{Boice, 1992, 1538720}"),
                       Lung   =c(),
                       Gastro =c("{Boice_Stomach, 1988, 3186929}",
                                 "{Boice_Colon, 1988, 3186929}",
                                 "{Boice_Rectum, 1988, 3186929}",
                                 "{Lonn_Oesophagus, 2010, 20142245}",
                                 "{Lonn_Stomach, 2010, 20142245}",
                                 "{Lonn_SmallInt, 2010, 20142245}",
                                 "{Lonn_Colon, 2010, 20142245}",
                                 "{Lonn_Rectum, 2010, 20142245}"),
                       Thyroid=c("{Boice, 1988, 3186929}",
                                 "{RubinoB, 2003, 14566829}"),
                       Brain  =c("{Lindberg, 1995, 7576739}"))

## check that manuscript identifiers match those used in data Excel table
Map(function(x, y) { y[!(y %in% x$Reference)] }, d_RR0_allL, sens_chemo_noL)

#####---------------------------------------------------------------------------
## studies with patients with chemotherapy and adjustment for chemo
sens_chemo_yes_adjL <- list(HaemLym=c("{Allodji, 2015, 26461008}",
                                      "{Travis, 1994, 8089863}",
                                      "{Tucker, 1987, 3469460}",
                                      "{Hawkins, 1992, 1581717}",
                                      "{Haddy, 2006, 16965909}",
                                      "{Curtis, 1994, 8064889}",
                                      "{Curtis, 1989, 2909667}",
                                      "{Curtis, 1992, 1594016}",
                                      "{Allard, 2010, 20303670}",
                                      "{Allodji, 2020, 33105035}"),
                            Sarcoma=c("{Hawkins, 1996, 8614005}",
                                      "{Henderson, 2012, 22795729}",
                                      "{Jenkinson, 2007, 17653071}",
                                      "{Le Vu, 1998, 9663598}",
                                      "{Menu-Branthomme, 2004, 15054872}",
                                      "{Schwartz, 2014, 24419490}",
                                      "{Svahn-Tapper, 2006, 16760180}",
                                      "{Tucker, 1987, 3475572}"),
                            Breast =c("{Guibout, 2005, 15625374}",
                                      "{Hooning, 2008, 18854572}",
                                      "{Inskip, 2009, 19620485}",
                                      "{Krul, 2017, 28888722}",
                                      "{Rubino, 2003, 12942115}",
                                      "{Storm, 1992, 1640483}",
                                      "{Stovall, 2008, 18556141}",
                                      "{Svahn-Tapper, 2006, 16760180}",
                                      "{Travis, 2003, 12876089}",
                                      "{van Leeuwen, 2003, 12837833}",
                                      "{Schonfeld, 2020, 31794291}",
                                      "{Veiga, 2019, 31657853}"),
                            Lung   =c("{Kaldor, 1992, 1428226}",
                                      "{Travis, 2002, 11830608}",
                                      "{Gilbert, 2003, 12537521}"),
                            Gastro =c("{Gilbert, 2017, 28118119}",
                                      "{Hauptmann, 2015, 25349972}",
                                      "{Morton, 2013, 23980092}",
                                      "{Allodji, 2019, 30345604}",
                                      "{Nottage, 2012, 22665546}"),
                            Thyroid=c("{Bhatti, 2010, 21128798}",
                                      "{de Vathaire, 1999, 10597762}",
                                      "{de Vathaire, 2015, 26327481}",
                                      "{Lonn, 2010, 20142245}",
                                      "{Ron, 1995, 7871153}",
                                      "{RubinoA, 2003, 12942115}",
                                      "{Sigurdson, 2005, 15950715}",
                                      "{Svahn-Tapper, 2006, 16760180}",
                                      "{Chow, 2009, 19459201}",
                                      "{Finke, 2015, 26517987}",
                                      "{Ronckers, 2006, 17007558}",
                                      "{Veiga, 2012, 22857014}",
                                      "{Tucker, 1991, 1851664}"),
                            Brain  =c("{Bhatia, 2002, 12036851}",
                                      "{Little, 1998, 9766556}",
                                      "{Neglia, 2006, 17077355}",
                                      "{Svahn-Tapper, 2006, 16760180}",
                                      "{Taylor, 2010, 21079138}"))

## check that manuscript identifiers match those used in data Excel table
Map(function(x, y) { y[!(y %in% x$Reference)] }, d_RR0_allL, sens_chemo_yes_adjL)

#####---------------------------------------------------------------------------
## studies with patients with chemotherapy and adjustment for chemo
sens_brachyL <- list(HaemLym=c("{Storm, 1985, 3932230}",
                               "{Lonn, 2010, 20142245}",
                               "{Boice_NHL, 1988, 3186929}",
                               "{Boice_MM, 1988, 3186929}",
                               "{Murohashi, 1985, 4068751}",
                               "{Curtis, 1994, 8064889}",
                               "{Boice, 1987, 3480381}",
                               "{Blettner, 1991, 1947508}",
                               "{Allard, 2010, 20303670}",
                               "{Holowaty_ANLL, 1995, 8625159}",
                               "{Holowaty_NCLL, 1995, 8625159}"),
                     Sarcoma=c("{Boice, 1988, 3186929}",
                               "{Tucker, 1987, 3475572}"),
                     Breast =c("{Adjadj, 2003, 12973856}"),
                     Lung   =c("{Lonn, 2010, 20142245}"),
                     Gastro =c("{Lonn_Oesophagus, 2010, 20142245}",
                               "{Lonn_Stomach, 2010, 20142245}",
                               "{Lonn_SmallInt, 2010, 20142245}",
                               "{Lonn_Colon, 2010, 20142245}",
                               "{Lonn_Rectum, 2010, 20142245}",
                               "{de Vathaire, 1997, 9043033}",
                               "{Rubino, 2003, 14583762}"),
                     Thyroid=c("{Boice, 1988, 3186929}",
                               "{Lonn, 2010, 20142245}",
                               "{Veiga, 2012, 22857014}"),
                     Brain  =c("{Lindberg, 1995, 7576739}"))

## check that manuscript identifiers match those used in data Excel table
Map(function(x, y) { y[!(y %in% x$Reference)] }, d_RR0_allL, sens_brachyL)

#####---------------------------------------------------------------------------
## fill in missing information such as CI boundaries, reference doses
## thyroid: downturn in dose-response at high doses -> set threshold
dose_thresh_thyroid <- 10

d_RR_allL <- Map(rr_impute_missing, d_RR0_allL)
d_RR_allL$Thyroid <- d_RR_allL$Thyroid %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid))

#####---------------------------------------------------------------------------
## save to file
saveRDS(d_RR0_allL, file="d_RR0_allL.rda")
saveRDS(d_RR_allL,  file="d_RR_allL.rda")

#####---------------------------------------------------------------------------
## run this chunk by hand to re-do estimating ERR/Gy + CI from category RRs
#####---------------------------------------------------------------------------

n_repl     <- 5000
d_RR0_allL <- readRDS("d_RR0_allL.rda")
d_RR_allL  <- readRDS("d_RR_allL.rda")
d_ERR_allL <- Map(get_all_ERR_from_RR, d_RR_allL, n_repl=n_repl)
saveRDS(d_ERR_allL, file="d_ERR_allL.rda")

#####---------------------------------------------------------------------------
## sensitivity: different sets of studies
#####---------------------------------------------------------------------------

## RR
d_RR_uniqueL        <- Map(function(x, y) { x %>% filter(!(Reference %in% y)) },
                           d_RR_allL, sens_overlappingL)

d_RR_childrenL      <- Map(function(x, y) { x %>% filter(Reference %in% y) },
                           d_RR_allL, sens_child_onlyL)

d_RR_adultsL        <- Map(function(x, y) { x %>% filter(Reference %in% y) },
                           d_RR_allL, sens_adult_onlyL)

d_RR_chemo_noL      <- Map(function(x, y) { x %>% filter(Reference %in% y) },
                           d_RR_allL, sens_chemo_noL)

d_RR_chemo_yes_adjL <- Map(function(x, y) { x %>% filter(Reference %in% y) },
                           d_RR_allL, sens_chemo_yes_adjL)

d_RR_brachy_noL     <- Map(function(x, y) { x %>% filter(!(Reference %in% y)) },
                           d_RR0_allL, sens_brachyL)

## ERR
d_ERR_uniqueL <- Map(function(x, y) {
    list(d_ERR_long=x$d_ERR_long %>% filter(!(Reference %in% y)),
         d_ERR_wide=x$d_ERR_wide %>% filter(!(Reference %in% y)))
}, d_ERR_allL, sens_overlappingL)

d_ERR_childrenL <- Map(function(x, y) {
    list(d_ERR_long=x$d_ERR_long %>% filter(Reference %in% y),
         d_ERR_wide=x$d_ERR_wide %>% filter(Reference %in% y))
}, d_ERR_allL, sens_child_onlyL)

d_ERR_adultsL <- Map(function(x, y) {
    list(d_ERR_long=x$d_ERR_long %>% filter(Reference %in% y),
         d_ERR_wide=x$d_ERR_wide %>% filter(Reference %in% y))
}, d_ERR_allL, sens_adult_onlyL)

d_ERR_chemo_noL <- Map(function(x, y) {
    list(d_ERR_long=x$d_ERR_long %>% filter(Reference %in% y),
         d_ERR_wide=x$d_ERR_wide %>% filter(Reference %in% y))
}, d_ERR_allL, sens_chemo_noL)

d_ERR_chemo_yes_adjL <- Map(function(x, y) {
    list(d_ERR_long=x$d_ERR_long %>% filter(Reference %in% y),
         d_ERR_wide=x$d_ERR_wide %>% filter(Reference %in% y))
}, d_ERR_allL, sens_chemo_yes_adjL)

d_ERR_brachy_noL <- Map(function(x, y) {
    list(d_ERR_long=x$d_ERR_long %>% filter(!(Reference %in% y)),
         d_ERR_wide=x$d_ERR_wide %>% filter(!(Reference %in% y)))
}, d_ERR_allL, sens_brachyL)

saveRDS(d_RR_uniqueL,         file="d_RR_uniqueL.rda")
saveRDS(d_ERR_uniqueL,        file="d_ERR_uniqueL.rda")
saveRDS(d_ERR_childrenL,      file="d_ERR_childrenL.rda")
saveRDS(d_ERR_adultsL,        file="d_ERR_adultsL.rda")
saveRDS(d_ERR_chemo_noL,      file="d_ERR_chemo_noL.rda")
saveRDS(d_ERR_chemo_yes_adjL, file="d_ERR_chemo_yes_adjL.rda")
saveRDS(d_ERR_brachy_noL,     file="d_ERR_brachy_noL.rda")

#####---------------------------------------------------------------------------
## sensitivity: different dose thresholds for thyroid
#####---------------------------------------------------------------------------

dose_thresh_thyroid <- c(5, 20)

## 5 Gy
d_RR_all_thresh05 <- rr_impute_missing(d_RR0_allL$Thyroid) %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid[1]))

## 20 Gy
d_RR_all_thresh20 <- rr_impute_missing(d_RR0_allL$Thyroid) %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid[2]))

d_ERR_thresh05L <- get_all_ERR_from_RR(d_RR_all_thresh05, n_repl=n_repl)
d_ERR_thresh20L <- get_all_ERR_from_RR(d_RR_all_thresh20, n_repl=n_repl)

## save
saveRDS(d_ERR_thresh05L$d_ERR_wide,
        file=paste0("d_ERR_thyroid_thresh",
                    sprintf("%.2d", dose_thresh_thyroid[1]), "_sel_wide.rda"))

saveRDS(d_ERR_thresh20L$d_ERR_wide,
        file=paste0("d_ERR_thyroid_thresh",
                    sprintf("%.2d", dose_thresh_thyroid[2]), "_sel_wide.rda"))
