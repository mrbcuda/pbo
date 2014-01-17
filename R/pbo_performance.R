#' Draws an annotated dot plot of study selection sorted by selection frequency.
#' @param data a list as returned by pbo()
#' @param main a title string
#' @param xlab an x-axis label string
#' @param ylab a y-axis label string
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_grid whether to show the background grid
#' @param ... other parameters as passed to dotplot()
pbo_performance_dot <- function(data,
                                main="IS Study Selection (Non-Zero Cases)",
                                xlab="Sorted Study Number (N)",
                                ylab="IS Selection Frequency",
                                show_config=TRUE,
                                show_grid=FALSE,
                                ...) {
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  x <- data$results
  ns <- as.integer(x[,'n*']) # n_star result in-sample
  tns <- data.frame(table(ns)) # for frequency counts
  tns$ns <- reorder(tns$ns,-tns$Freq) # sorted by decreasing frequency
  
  dotplot(Freq ~ ns,
          data=tns,
          xlab=xlab,
          ylab=ylab,
          main=main,
          horizontal=FALSE,
          panel = function(x,...) {
            panel.xyplot(x,...)
            if (show_grid) {
              panel.grid(-1,-1,col="lightgray",...)
            }
            if (show_config) {
              grid.text(label=data$test_config,
                        x = unit(1,"npc") - unit(3,"mm"),
                        y = unit(1,"npc") - unit(3,"mm"),
                        just="right")
            }
          },
          ...
  )
}

#' Draws an annotated XY plot of study selection sorted by selection frequency.
#' @param data a list as returned by pbo()
#' @param main a title string
#' @param xlab an x-axis label string
#' @param ylab a y-axis label string
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_grid whether to show the background grid
#' @param ... other parameters as passed to xyplot()
pbo_performance_xy <- function(data,
                               main="IS Study Selection (Non-Zero Cases)",
                               xlab="Sorted Study Number (N)",
                               ylab="IS Selection Frequency",
                               show_config=TRUE,
                               show_grid=FALSE,
                               ...) {
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  x <- data$results
  ns <- as.integer(x[,'n*']) # n_star result in-sample
  tns <- data.frame(table(ns)) # for frequency counts
  tns$ns <- reorder(tns$ns,-tns$Freq) # sorted by decreasing frequency
  
  xyplot(Freq ~ ns,
         data=tns,
         xlab=xlab,
         ylab=ylab,
         main=main,
         horizontal=FALSE,
         panel = function(x,...) {
           panel.xyplot(x,...)
           if (show_grid) {
             panel.grid(-1,-1,col="lightgray",...)
           }
           if (show_config) {
             grid.text(label=data$test_config,
                       x = unit(1,"npc") - unit(3,"mm"),
                       y = unit(1,"npc") - unit(3,"mm"),
                       just="right")
           }
         },
         ...
  )
}

#' Draws a scatter XY plot of study selection pairs with OOS performance 
#' exceeding a given threshold.
#' @param data a list as returned by pbo()
#' @param osr_threshold out-of-sample rank inclusion threshold, default 0
#' @param xlab an x-axis label string
#' @param ylab a y-axis label string
#' @param show_rug whether to show the rug plot along the axes, default TRUE
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_grid whether to show the background grid
#' @param pch array of plotting characters, default c(5,6)
#' @param col array of plotting colors, default c(4,6)
#' @param ... other parameters as passed to xyplot()
pbo_performance_pairs <- function(data,
                                  osr_threshold=0,
                                  xlab='IS Selection (N)',
                                  ylab='OOS Counterpart Selection (N)',
                                  show_rug = TRUE,
                                  show_config = TRUE,
                                  show_grid = FALSE,
                                  pch=c(0,3),
                                  col=c(4,6),
                                  ...)
  {
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  x <- data$results
  y <- data.frame(cbind(nis=as.numeric(x[,'n*']),
                        noos=as.numeric(x[,'n_max_oos']),
                        osr=as.numeric(x[,'os_rank'])))
  
  xyplot(noos ~ nis, 
         data=y, 
         subset=osr>osr_threshold,
         xlab=xlab,
         ylab=ylab,
         pch=pch,
         col=col,
         main= bquote(paste("IS/OOS Study Selection Performance (OOS Rank >",
                           .(osr_threshold),
                           ')',
                           sep='' )),
         key = list(text=list(c("OOS","IS")),
                    points=pch,
                    col=col, #trellis.par.get()$superpose.symbol$col[1:2],
                    pch=pch,
                    columns=2),
         panel = function(x,...) {
           panel.xyplot(x,...)
           if (show_grid) {
             panel.grid(-1,-1,col="lightgray")
           }
           if ( show_rug ) {
             panel.rug(x,...)
           }
           if (show_config) {
             grid.text(label=data$test_config,
                       x = unit(1,"npc") - unit(3,"mm"),
                       y = unit(1,"npc") - unit(3,"mm"),
                       just="right")
           }
         },
         ...
  )
}


#' Draws a scatter XY plot showing selected study by CSCV case, with out-of-sample rank 
#' exceeding a given threshold.
#' @param data a list as returned by pbo()
#' @param osr_threshold out-of-sample rank inclusion threshold, default 0
#' @param xlab an x-axis label string
#' @param ylab a y-axis label string
#' @param show_rug whether to show the rug plot along the axes, default TRUE
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_grid whether to show the background grid
#' @param pch array of plotting characters, default c(0,3)
#' @param col array of plotting colors, default c(4,6)
#' @param ... other parameters as passed to xyplot()
pbo_performance_cases <- function(data,
                                  osr_threshold=0,
                                  xlab='CSCV Case',
                                  ylab='Selected Study (N)',
                                  show_rug = TRUE,
                                  show_config = TRUE,
                                  show_grid = FALSE,
                                  pch=c(0,3),
                                  col=c(4,6),
                                  ...)
{
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  x <- data$results
  y <- data.frame(cbind(nis=as.numeric(x[,'n*']),
                        noos=as.numeric(x[,'n_max_oos']),
                        osr=as.numeric(x[,'os_rank'])))
  
  xyplot(noos + nis ~ 1:nrow(y), 
         data=y, 
         subset=osr>osr_threshold,
         xlab=xlab,
         ylab=ylab,
         pch=pch,
         col=col,
         main=bquote(paste("IS/OOS Study Selection by CSCV Case (OOS Rank >",
                           .(osr_threshold),
                           ')',
                           sep='' )),
         key = list(text=list(c("OOS","IS")),
                    points=pch,
                    col=col, # trellis.par.get()$superpose.symbol$col[1:2],
                    pch=pch,
                    columns=2),
         panel = function(x,...) {
           panel.xyplot(x,...)
           if (show_grid) {
             panel.grid(-1,-1,col="lightgray")
           }
           if ( show_rug ) {
             panel.rug(x,...)
           }
           if (show_config) {
             grid.text(label=data$test_config,
                       x = unit(1,"npc") - unit(3,"mm"),
                       y = unit(1,"npc") - unit(3,"mm"),
                       just="right")
           }
         },
         ...
  )
}


#' Draws a scatter XY plot showing OOS performance rank by CSCV case, with out-of-sample rank 
#' exceeding a given threshold.
#' @param data a list as returned by pbo()
#' @param osr_threshold out-of-sample rank inclusion threshold, default 0
#' @param xlab an x-axis label string
#' @param ylab a y-axis label string
#' @param show_rug whether to show the rug plot along the axes, default TRUE
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_grid whether to show the background grid
#' @param ... other parameters as passed to xyplot()
pbo_performance_ranks <- function(data,
                                  osr_threshold=0,
                                  xlab='Selected IS Study (N)',
                                  ylab='OOS Rank',
                                  show_rug = TRUE,
                                  show_config = TRUE,
                                  show_grid = FALSE,
                                  ...)
{
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  x <- data$results
  y <- data.frame(cbind(nis=as.numeric(x[,'n*']),
                        noos=as.numeric(x[,'n_max_oos']),
                        osr=as.numeric(x[,'os_rank'])))
  
  xyplot(osr ~ nis, 
         data=y,
         subset=osr>osr_threshold,
         xlab=xlab,
         ylab=ylab,
         main=bquote(paste("Selected IS Study Performance OOS (OOS Rank >",
                           .(osr_threshold),
                           ')',
                           sep='' )),
         horizontal=FALSE,
         panel = function(x,...) {
           panel.xyplot(x,...)
           if (show_grid) {
             panel.grid(-1,-1,col="lightgray",...)
           }
           if ( show_rug ) {
             panel.rug(x,...)
           }
           if (show_config) {
             grid.text(label=data$test_config,
                       x = unit(1,"npc") - unit(3,"mm"),
                       y = unit(1,"npc") - unit(3,"mm"),
                       just="right")
           }
         },
         ...
  )
}

