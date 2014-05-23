# lattice.pbo.R

#' Writes grid text to a default predetermined location.
#' @note Meant for internal use only.
#' @param p an object of class \code{pbo} as returned by \code{\link[pbo]{pbo}}.
#' @import grid
pbo_show_config <- function(p) {
  grid.text(label=p$test_config,
            x = unit(1,"npc") - unit(3,"mm"),
            y = unit(1,"npc") - unit(3,"mm"),
            just="right")
}


#' @title PBO rank logits histogram.
#' @description Draws an annotated histogram of PBO rank logits.
#' @details Uses \pkg{lattice} function \code{\link[lattice]{histogram}}, 
#' \code{\link[lattice]{densityplot}}, and
#' \code{\link[lattice]{panel.abline}} panels together with 
#' class-specific annotations.
#' @param x an object of class \code{pbo} as returned by \code{\link[pbo]{pbo}}.
#' @param data should not be used
#' @param show_pbo whether to show the PBO value annotation, 
#' default TRUE
#' @param show_regions whether to show the overfit region annotations, 
#' default TRUE
#' @param show_config whether to show the study dimension annotations, 
#' default TRUE
#' @param col_bar histogram bar fill color passed to histogram panel
#' @param col_line density plot line color passed to density plot panel
#' @param ... other parameters passed to \code{\link[lattice]{histogram}},
#' \code{\link[lattice]{densityplot}}, or \code{\link[lattice]{panel.abline}}.
#' @seealso pbo, dotplot.pbo, xyplot.pbo
#' @importFrom lattice histogram
#' @importFrom lattice panel.histogram
#' @importFrom lattice panel.densityplot
#' @importFrom lattice panel.abline
#' @export
histogram.pbo <- function(x,
                          data=NULL,
                          show_pbo=TRUE,
                          show_regions=TRUE,
                          show_config=TRUE,
                          col_bar="#cc99cc",
                          col_line="#3366cc",
                          ...)
{
  # advise ignoring data
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored; using 'pbo' object")

  # class reference for use within panels
  p <- x
  
  # plot rank logit with PBO annotation
  histogram(p$lambda,
            xlim=c(-p$inf_sub,p$inf_sub),
            xlab=expression(lambda[c]),
            panel = function(x, ...){
              panel.histogram(x, col=col_bar,...)
              panel.densityplot(x, col=col_line, ...)
              panel.abline(v=0,lty=3,...)
              if (show_pbo) {
                xa <- unit(0, "npc") + unit(2, "mm")
                ya <- unit(1, "npc") - unit(3, "mm")
                grid.text(label = bquote(PBO == .(round(p$phi,digits=3))),
                          x = xa,
                          y = ya,
                          just = "left")
              }
              if (show_config)
                pbo_show_config(p)
              if (show_regions) {
                ya <- unit(1, "npc") - unit(3, "mm")
                grid.text(label = "Less overfit",
                          x = unit(0.5,"npc") + unit(2,"mm"),
                          y=ya,
                          just="left")
                grid.text(label = "More overfit",
                          x = unit(0.5,"npc") - unit(2,"mm"),
                          y=ya,
                          just="right")
              }
            },
            ...
  )

}

#' @title PBO in-sample selection dot plot.
#' @description Draws an annotated dot plot of study selection sorted 
#' by in-sample selection frequency.
#' @param x a \code{pbo} object as returned by \code{\link[pbo]{pbo}}.
#' @param data should not be used
#' @param main plot title, default computed internally, 
#' passed to \code{\link[lattice]{dotplot}}.
#' @param xlab x-axis label with default, 
#' passed to \code{\link[lattice]{dotplot}}.
#' @param ylab y-axis label with default, 
#' passed to \code{\link[lattice]{dotplot}}.
#' @param show_config whether to show the study dimension annotations, 
#' default TRUE
#' @param show_grid whether to show the grid panel, default TRUE
#' @param sel_threshold the minimum in-sample frequency subsetting threshold,
#' default 0; selection frequencies at or below this value will be omitted
#' @param ... other parameters as passed to \code{\link[lattice]{dotplot}}.
#' @keywords pbo backtest overfitting
#' @importFrom lattice dotplot
#' @importFrom lattice panel.xyplot
#' @importFrom lattice panel.grid
#' @export
#' @seealso pbo, histogram.pbo, xyplot.pbo
dotplot.pbo <- function(x,
                        data=NULL,
                        main,
                        xlab="Sorted Study Number (N)",
                        ylab="IS Selection Frequency",
                        show_config=TRUE,
                        show_grid=TRUE,
                        sel_threshold=0,
                        ...)
{
  # advise ignoring data
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored; using 'pbo' object")

  p <- x
  # x <- p$results
  Freq <- NULL # only to appease check
  ns <- as.integer(x$results[,'n*']) # n_star result in-sample
  tns <- data.frame(table(ns)) # for frequency counts
  tns$ns <- reorder(tns$ns,-tns$Freq) # sorted by decreasing frequency

  if (missing(main))
    main=paste("IS Study Selection (Frequency > ",sel_threshold,")",sep='')

  dotplot(Freq ~ ns,
          data=tns,
          subset=Freq>sel_threshold,
          xlab=xlab,
          ylab=ylab,
          main=main,
          panel = function(x,...) {
            panel.xyplot(x,...)
            if (show_grid)
              panel.grid(-1,-1,...)
            if (show_config)
              pbo_show_config(p)
          },
          ...
  )
}


#' @title PBO xy-plots
#' @description Draws an annotated plot of performance degradation and 
#' probability of loss.
#' @details Provides several variations of xy-plots suitable for presentation
#' of PBO analysis results.  Use the \code{plotType} argument to indicate
#' which variation or result to plot:
#' \itemize{  
#' \item The \code{cscv} type shows in-sample
#' and out-of-sample results by CSCV iteration case (default).  
#' \item The \code{degradation} type shows the performance degradation regression 
#' fit results and the probability of loss.  
#' \item The \code{dominance} type shows the results of the first-order and 
#' second-order stochastic dominance analysis using two axes.
#' \item The \code{pairs} type shows the in-sample and out-of-sample 
#' case selections.
#' \item The \code{ranks} type shows the sorted performance ranks results.
#' \item The \code{selection} type shows the case selection frequencies.
#' }
#' @param x a \code{pbo} object as returned by \code{\link{pbo}}.
#' @param data should not be used
#' @param plotType one of \code{cscv}, \code{degradation}, \code{dominance},
#' \code{pairs}, \code{ranks} or \code{selection}.  
#' @param col_bar histogram bar fill color
#' @param col_line density plot line color
#' @param col_sd1 color of two first-order stochastic dominance lines
#' @param col_sd2 color of the single second-order stochastic dominance line
#' @param xlab x-axis label, default computed if not provided
#' @param ylab y-axis label, default computed if not provided
#' @param main plot title, default computed if not provided
#' @param lwd line width, default 1, passed to panels and legends
#' @param lty_sd line type array for stochastic dominance plot, 
#' e.g. c(2,3,5)
#' @param ylab_left dominance plot left-hand axis label
#' @param ylab_right dominance plot right-hand axis label
#' @param increment stochastic dominance distribution generator increment, 
#' e.g. 0.1 steps
#' @param osr_threshold out-of-sample rank threshold for filtering, default 0
#' @param sel_threshold selection frequency threshold for filtering, default 0
#' @param show_eqn whether to show the line equation annotation, default TRUE
#' @param show_threshold whether to show the probability of loss annotation, 
#' default TRUE
#' @param show_config whether to show the study dimension annotations, 
#' default TRUE
#' @param show_grid whether to show the panel grid, 
#' default TRUE
#' @param show_prob whether to show the probability value in dominance plot, 
#' default TRUE
#' @param show_rug whether to show scatter rugs near the axes, 
#' default TRUE
#' @param ... other parameters passed to \code{\link[lattice]{xyplot}} 
#' or its panels
#' @importFrom lattice xyplot
#' @importFrom lattice panel.xyplot
#' @importFrom lattice panel.rug
#' @importFrom lattice panel.grid
#' @importFrom lattice panel.lmline
#' @importFrom lattice panel.abline
#' @export
#' @keywords PBO, CSCV
#' @seealso pbo, histogram.pbo, xyplot.pbo
xyplot.pbo <- function(x,
                       data=NULL,
                       plotType="cscv",
                       show_eqn=TRUE,
                       show_threshold=TRUE,
                       show_config=TRUE,
                       show_rug=TRUE,
                       show_prob=TRUE,
                       show_grid=TRUE,
                       increment=0.01,
                       osr_threshold=0,
                       sel_threshold=0,
                       xlab,
                       ylab,
                       main,
                       lwd=1,
                       ylab_left,
                       ylab_right,
                       col_bar,
                       col_line,
                       col_sd1="#3366cc",
                       col_sd2="#339999",
                       lty_sd=c(1,2,4),
                       ...)
{
  # confirm plot type specified
  ptypes = c('cscv','degradation','dominance','pairs','ranks','selection')
  if ( ! plotType %in% ptypes )
    stop(paste("xyplot with 'pbo' object argument 'plotType' must be one of",
               toString(ptypes)))

  # advise ignoring data
  if (!is.null(match.call()$data))
    warning("xyplot explicit 'data' specification ignored; using 'pbo' object")

  # reference to object for use within panels
  p <- x
  
  # cscv plot
  if (plotType == "cscv") {

    if (missing(xlab))
      xlab='CSCV Case'
    if (missing(ylab))
      ylab='Selected Study (N)'
    if (missing(main))
      main=bquote(paste("IS/OOS Study Selection by CSCV Case (OOS Rank > ",
                        .(osr_threshold),
                        ')',
                        sep='' ))

    osr <- NULL # only to appease check
    y <- data.frame(cbind(nis=as.numeric(p$results[,'n*']),
                          noos=as.numeric(p$results[,'n_max_oos']),
                          osr=as.numeric(p$results[,'os_rank'])))

    rv = xyplot(noos + nis ~ 1:nrow(y),
                data=y,
                subset=osr>osr_threshold,
                main=main,
                xlab=xlab,
                ylab=ylab,
                lwd=lwd,
                panel = function(x,...) {
                  panel.xyplot(x,...)
                  if ( show_grid )
                    panel.grid(-1,-1,...)
                  if ( show_rug )
                    panel.rug(x,...)
                  if (show_config)
                    pbo_show_config(p)
                },
                ...
    )
  }


  # performance degradation plot
  if (plotType == "degradation") {

    if (missing(main))
      main="OOS Performance Degradation"
    if (missing(xlab))
      xlab="R (IS)"
    if (missing(ylab))
      ylab="R (OOS)"
    if (missing(col_bar))
      col_bar="#cc99cc"
    if (missing(col_line))
      col_line="#3366cc"

    # plot Rn pairs
    cloud_span <- c(signif(min(p$rn_pairs),-3),
                    signif(max(p$rn_pairs),3)) # axis range

    rv = xyplot(p$rn_pairs$Rbn ~ p$rn_pairs$Rn,
                main = main,
                xlab = xlab,
                ylab = ylab,
                xlim = cloud_span,
                ylim = cloud_span,
                lwd = lwd,
                panel = function(x, ...){
                  panel.xyplot(x,col=col_bar,...)
                  panel.lmline(x,col=col_line,...)
                  panel.abline(v=p$threshold,type="l",lty=3)
                  panel.abline(h=p$threshold,type="l",lty=3)
                  if ( show_rug ) {
                    panel.rug(x,col=col_bar,...)
                  }
                  ya <- unit(1, "npc") - unit(3, "mm")
                  if ( show_eqn ) {
                    grid.text(label = bquote(R_OOS == .(p$intercept) (R_IS) + .(p$slope) + err ~~ AdjR^2 == .(p$ar2)),
                              x = unit(0, "npc") + unit(3, "mm"),
                              y = ya,
                              just = "left",
                              gp=gpar(col=col_line))
                  }
                  if (show_config)
                    pbo_show_config(p)
                  if (show_threshold) {
                    if ( p$threshold == 1 ) { # ugly but ifelse won't work on bquote
                      grid.text(label = bquote(P(R_OOS<1) ==  .(p$below_threshold)),
                                x = unit(1, "npc") - unit(3, "mm"),
                                y = ya - unit(10,"mm"),
                                just = "right",
                                gp=gpar(col=col_bar))
                    } else {
                      grid.text(label = bquote(P(R_OOS<0) ==  .(p$below_threshold)),
                                x = unit(1, "npc") - unit(3, "mm"),
                                y = ya - unit(10,"mm"),
                                just = "right",
                                gp=gpar(col=col_bar))
                    }
                  }
                },
                ...
    )
  }

  # stochastic dominance plot
  if ( plotType == "dominance") {

    if (missing(main))
      main="Stochastic Dominance"
    if (missing(ylab_left))
      ylab_left="Frequency"
    if (missing(ylab_right))
      ylab_right="2nd Ord. Stochastic Dominance"

    # uses n* items from R-bar for one line, and all n items from R-bar for the other line
    # create cumulative distribution functions for each data set,
    # then generate samples to plot
    y <- seq(min(p$rn_pairs$Rbn),
             max(p$rn_pairs$Rbn),
             increment) # reasonable R range for evaluation
    erbn <- ecdf(p$rn_pairs$Rbn) # optimized
    erb <- ecdf(sapply(1:ncol(p$combos),
                       function(i) p$results[[i,2]])) # non-optimized (all)
    sorted <- data.frame(cbind(sort(erbn(y)),sort(erb(y))))
    sorted$sd2 <- sorted$X2 - sorted$X1
    colnames(sorted) <- c("Rbn","Rb","SD2")
    colors = c(col_sd1,col_sd1,col_sd2)

    x1 = xyplot(Rbn + Rb ~ y,
                data = sorted,
                type="l",
                lty=lty_sd,
                lwd=lwd,
                col=colors,
                main=main,
                ylab=ylab_left,
                xlab=expression(bar(R)[n^textstyle("*")] ~~ plain(vs.) ~~ bar(R)),
                key=list(columns=3,
                         lines=list(col=colors,
                                    lty=lty_sd,
                                    lwd=lwd),
                         text=list(names=c("Optimized (L)",
                                           "Non-Optimized (L)",
                                           "SD2 (R)"))
                ),

                panel = function(x, ...){
                  panel.xyplot(x,...)
                  panel.abline(v=p$threshold,type="l",lty=3)
                  if (show_grid)
                    panel.grid(-1,-1)
                  if (show_prob) {
                    grid.text(label = expression(paste(italic(Prob),
                                                       group("[",bar(R)[n^textstyle("*")] >= x,"]"),
                                                       " > ",
                                                       italic(Prob),
                                                       group("[",bar(R) >= x,"]"))),
                              x = unit(0, "npc") + unit(3, "mm"),
                              y = unit(0, "npc") + unit(3, "mm"),
                              just = "left"
                    )
                  }
                  if (show_config)
                    pbo_show_config(p)
                },
                ...
    )
    x2 = xyplot(SD2 ~ y,
                data=sorted,
                type="l",
                lwd=lwd,
                lty=ifelse(length(lty_sd>2),lty_sd[3],lty_sd),
                col=colors[3],
                ylab=ylab_right,
                panel = function(x, ...){
                  panel.xyplot(x,...)
                  panel.abline(h=0,type="l",lty=3)
                  grid.text(label = expression(italic(SD2) >= 0),
                            x = unit(1, "npc") - unit(3, "mm"),
                            y = unit(0, "npc") + unit(3, "mm"),
                            just = "right"
                  )
                },
                ...
    )
    rv = doubleYScale(x1,
                      x2,
                      add.ylab2=TRUE,
                      use.style=FALSE
    )
  }


  if (plotType == "pairs") {

    if (missing(xlab))
      xlab='IS Selection (N)'
    if (missing(ylab))
      ylab='OOS Counterpart Selection (N)'
    if (missing(main))
      main= bquote(paste("IS/OOS Study Selection Performance (OOS Rank > ",
                         .(osr_threshold),
                         ')',
                         sep='' ))

    # x <- p$results
    y <- data.frame(cbind(nis=as.numeric(p$results[,'n*']),
                          noos=as.numeric(p$results[,'n_max_oos']),
                          osr=as.numeric(p$results[,'os_rank'])))

    rv = xyplot(noos ~ nis,
                data=y,
                subset=osr>osr_threshold,
                xlab=xlab,
                ylab=ylab,
                main=main,
                lwd=lwd,
                panel = function(x,...) {
                  panel.xyplot(x,...)
                  if ( show_grid )
                    panel.grid(-1,-1,...)
                  if ( show_rug )
                    panel.rug(x,...)
                  if (show_config)
                    pbo_show_config(p)
                },
                ...
    )
  }


  if (plotType == "ranks") {

    if (missing(xlab))
      xlab='Selected IS Study (N)'
    if (missing(ylab))
      ylab='OOS Rank'
    if (missing(main))
      main=bquote(paste("Selected IS Study Performance OOS (OOS Rank > ",
                        .(osr_threshold),
                        ')',
                        sep='' ))

    # x <- p$results
    y <- data.frame(cbind(nis=as.numeric(p$results[,'n*']),
                          noos=as.numeric(p$results[,'n_max_oos']),
                          osr=as.numeric(p$results[,'os_rank'])))

    rv = xyplot(osr ~ nis,
                data=y,
                subset=osr>osr_threshold,
                xlab=xlab,
                ylab=ylab,
                lwd=lwd,
                main=main,
                horizontal=FALSE,
                panel = function(x,...) {
                  panel.xyplot(x,...)
                  if ( show_grid )
                    panel.grid(-1,-1,...)
                  if ( show_rug )
                    panel.rug(x,...)
                  if (show_config)
                    pbo_show_config(p)
                },
                ...
    )
  }

  if ( plotType == "selection") {

    if (missing(xlab))
      xlab="Sorted Study Number (N)"
    if (missing(ylab))
      ylab="IS Selection Frequency"
    if (missing(main))
      main= bquote(paste("IS Study Selection (Frequency > ",
                         .(sel_threshold),
                         ')',
                         sep='' ))

    # x <- p$results
    Freq <- NULL # only to appease check
    ns <- as.integer(p$results[,'n*']) # n_star result in-sample
    tns <- data.frame(table(ns)) # for frequency counts
    tns$ns <- reorder(tns$ns,-tns$Freq) # sorted by decreasing frequency

    rv = xyplot(Freq ~ ns,
                data=tns,
                subset=Freq>sel_threshold,
                xlab=xlab,
                ylab=ylab,
                main=main,
                horizontal=FALSE,
                panel = function(x,...) {
                  panel.xyplot(x,...)
                  if (show_grid)
                    panel.grid(-1,-1,...)
                  if (show_rug)
                    panel.rug(x=NULL,...) # show only y-axis rug
                  if (show_config)
                    pbo_show_config(p)
                },
                ...
    )
  }

  # returns the plot, flushing the graphics
  rv
}
