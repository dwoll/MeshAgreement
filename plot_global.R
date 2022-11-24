library(ggplot2)

## ggplot2 theme
ggpt <- theme_bw() +
    theme(axis.title.x=element_text(size=rel(1.1)),
          axis.title.y=element_text(size=rel(1.1)),
          axis.text.x=element_text(size=rel(1.1)),
          axis.text.y=element_text(size=rel(1.1)),
          strip.text.x=element_text(size=rel(1.1)),
          strip.text.y=element_text(size=rel(1.1)),
          legend.text=element_text(size=rel(1)),
          legend.title=element_text(size=rel(1.1)),
          plot.title=element_text(size=rel(1.1)))

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## plot functions
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## plot category RRs
#####---------------------------------------------------------------------------

plot_categ_rr_ci <- function(x, ylim, show=TRUE) {
    p <- ggplot(x,
           aes(x=d_kPrime, y=RR_cat,
               ymin=RR_cat_CIlo, ymax=RR_cat_CIup,
               color=auth_year)) +
        geom_point() +
        geom_linerange() +
        coord_cartesian(ylim=ylim) +
        xlab("Dose [Gy]") +
        ylab("RR category") +
        ggpt +
        theme(legend.position="bottom",
              legend.title=element_blank())
    
    if(show) { print(p) }
    invisible(p)
}

#####---------------------------------------------------------------------------
## plot per-study ERR + CIs with category RR + CIs
#####---------------------------------------------------------------------------

plot_err_categ_rr_ci <- function(d_err, d_rr, ylim, lpos, show=TRUE) {
    d_ribbon <- data.frame(d_kPrime=seq(from=min(d_rr$d_kPrime, na.rm=TRUE),
                                        to  =max(d_rr$d_kPrime, na.rm=TRUE),
                                        length.out=2))
    
    d_ERR_ribbon <- d_err %>%
        full_join(d_ribbon, by=character()) %>%
        mutate(ERR_CI_Ylo=1 + d_kPrime*ERR_CIlo,
               ERR_CI_Yup=1 + d_kPrime*ERR_CIup)
    
    p <- ggplot(d_rr) +
        geom_point(aes(x=d_kPrime, y=RR_cat)) +
        geom_linerange(aes(x=d_kPrime, y=RR_cat,
                           ymin=RR_cat_CIlo, ymax=RR_cat_CIup)) +
        geom_ribbon(data=d_ERR_ribbon,
                    aes(x=d_kPrime, ymin=ERR_CI_Ylo, ymax=ERR_CI_Yup,
                        color=from, fill=from), alpha=0.3, linetype=0) +
        geom_abline(data=d_err,
                    aes(intercept=1, slope=ERR, color=from)) +
        coord_cartesian(ylim=ylim) +
        facet_wrap(~ auth_year) +
        xlab("Dose [Gy]") +
        ylab("RR") +
        ggpt +
        theme(legend.position=lpos)
    
    if(show) { print(p) }
    invisible(p)
}

#####---------------------------------------------------------------------------
## plot new ERR+CI with org ERR+CI
#####---------------------------------------------------------------------------

plot_err_org_new <- function(x, ylim, lpos, show=TRUE) {
    p <- ggplot(x,
                aes(x=auth_year,
                    y=ERR,
                    group=from,
                    color=from,
                    shape=from,
                    ymin=ERR_CIlo,
                    ymax=ERR_CIup)) +
        geom_point(position=position_dodge(width=0.75)) +
        geom_linerange(position=position_dodge(width=0.75)) +
        xlab(NULL) +
        coord_flip(ylim=ylim) +
        ggpt +
        theme(legend.position=lpos)
    
    if(show) { print(p) }
    invisible(p)
}

#####---------------------------------------------------------------------------
## plot metaanalysis results
#####---------------------------------------------------------------------------

plot_meta_results <- function(x, show=TRUE) {
    d_ref_line <- x %>%
        filter(type == "primary") %>%
        select(site, ERR) %>%
        rename(ref_line=ERR)
    
    d_plot <- x %>%
        left_join(d_ref_line, by="site")
    
    p <- ggplot(d_plot,
               aes(x=type, y=ERR, ymin=ERR_CIlo, ymax=ERR_CIup, color=type)) +
        geom_hline(yintercept=0, col="gray") +
        geom_hline(aes(yintercept=ref_line), col="darkgray", size=0.7) +
        geom_point(position=position_dodge(width=0.3)) +
        geom_linerange(position=position_dodge(width=0.3)) +
        xlab("Metaanalysis type") +
        ylab(expression(paste('ERR ', Gy^-1, ' with 95% CI'))) +
        # scale_y_log10() +
        coord_flip() +
        facet_grid(site ~ .) +
        ggpt +
        theme(legend.position="none")

    if(show) { print(p) }
    invisible(p)
}
