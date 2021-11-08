####################################
##### Simon Zuzek - 2021-10-01 #####
#### Useful plotting functions #####

## Saving a plot
# ggsave("path/name.pdf", plot = p, width = 4,
#        height = 3.3, scale = 2.2)



#' Change the labels of discrete scales manually
#'
#' Implemented as part of nicePlot functions. Not meant for direct use.
#'
#' @param p A ggplot2-plot
#' @param linename,colorname,fillname The names of the scales
#' @param linetypelabels,colorlabels,filllabels Vectors of labels, one for each
#'
#' @return A ggplot2-plot
#'
niceDiscreteLabels = function(p, linename = NULL, linetypelabels = NULL,
                              colorname = NULL, colorlabels = NULL,
                              fillname = NULL, filllabels = NULL) {
  if (!is.null(linetypelabels)) p = p + ggplot2::scale_linetype_discrete(name = linename, labels = linetypelabels)
  if (!is.null(colorlabels)) p = p + ggplot2::scale_color_discrete(name = colorname, labels = colorlabels)
  if (!is.null(filllabels)) p = p + ggplot2::scale_fill_discrete(name = fillname, labels = filllabels)
  p
}



#' Nice line plot, possibly with many groups
#'
#' @param data A dataset
#' @param x horizontal axis variable
#' @param y vertical axis variable
#' @param group,color,linetype Character. Variables to group on. With changing linetype or color
#' @param line_size Numeric. General size of lines
#' @param xlims,ylims Numeric vectors of 2 elements. Axis limits
#' @param title Plot title
#' @param xlab,ylab Axis labels
#' @param linename,colorname Optional variable names for groups to be displayed on legend
#' @param linetypelabels,colorlabels Optional labels for grouos to be displayed on legend
#' @param legendpos Legend position. Options: “left”,“top”, “right” (default), “bottom”
#'
#' @return A ggplot2-plot
#' @export
#'
#' @examples
#' niceLinePlot(ggplot2::mpg, "displ", "hwy", color = "class", linetype = "class",  ylim = c(10,50), line_size = 1)
#'
#' # Changes to the label variables can be done directly
#' niceLinePlot(datasets::mtcars, "disp", "hp", color = "vs", colorlabels = c("V-shaped", "straight"), colorname = "Engine")
niceLinePlot = function(data, x, y, group = NULL, color = NULL, linetype = NULL,
                        line_size = 0.7, xlims = NULL, ylims = NULL, title = "", xlab = x, ylab = y,
                        linename = linetype, colorname = color,
                        linetypelabels = NULL, colorlabels = NULL,
                        legendpos = "right") {
  x = rlang::sym(x)
  y = rlang::sym(y)
  data.table::setDT(data)

  if (!is.null(color)) {
    data[, color] = as.factor(data[, color])
    color = rlang::sym(color) }
  if (!is.null(linetype)) {
    data[, linetype] = as.factor(data[, linetype])
    linetype = rlang::sym(linetype) }
  if (!is.null(group)) {
    data[, group] = as.factor(data[, group])
    group = rlang::sym(group) }

  p = ggplot2::ggplot(data = data, ggplot2::aes(x = !!x, y = !!y, group = !!group, color = !!color, linetype = !!linetype)) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::labs(color = colorname, linetype = linename) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position= legendpos)

  if (!is.null(ylims)) p = p + ggplot2::ylim(ylims)
  if (!is.null(xlims)) p = p + ggplot2::xlim(xlims)


  # change labels if needed.
  p = niceDiscreteLabels(p, linename = linename, colorname = colorname,
                         linetypelabels = linetypelabels, colorlabels = colorlabels)

  return(p)
}



#' Nice histogram
#'
#' @param data A dataset
#' @param x horizontal axis variable
#' @param color,fill Character. Variables to group on. With changing fill or border color
#' @param xlims,ylims Numeric vectors of 2 elements. Axis limits
#' @param title Plot title
#' @param binwidth Numeric. Width of bins
#' @param alpha Opacity of fill color
#' @param xlab,ylab Axis labels. Ylab default: "density"
#' @param fillname,colorname Optional variable names for groups to be displayed on legend
#' @param filllabels,colorlabels Optional labels for grouos to be displayed on legend
#' @param legendpos Legend position. Options: “left”,“top”, “right” (default), “bottom”
#'
#' @return A ggplot2-plot
#' @export
#'
#' @examples
#' niceHistogram(ggplot2::mpg, "displ", binwidth = 0.3, color = "class", fill = "class")
niceHistogram = function(data, x, fill = NULL, color = NULL, alpha = 0.3, binwidth = NULL,
                         xlims = NULL, ylims = NULL, title = "", xlab = x, ylab = 'density',
                         colorname = color, fillname = fill,
                         colorlabels = NULL, filllabels = NULL,
                         legendpos = "right") {
  x = rlang::sym(x)
  data.table::setDT(data)

  if (!is.null(color)) {
    data[, color] = as.factor(data[, color])
    color = rlang::sym(color) }
  if (!is.null(fill)) {
    data[, fill] = as.factor(data[, fill])
    fill = rlang::sym(fill) }

  p = ggplot2::ggplot(data, ggplot2::aes(x = !!x, color = !!color, fill = !!fill)) +
    ggplot2::geom_histogram(alpha = alpha, binwidth = binwidth, position="identity") +
    ggplot2::labs(color = colorname, fill = fillname) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position= legendpos)

  if (!is.null(ylims)) p = p + ggplot2::ylim(ylims)
  if (!is.null(xlims)) p = p + ggplot2::xlim(xlims)

  # change labels if needed.
  p = niceDiscreteLabels(p, fillname =  fillname, colorname = colorname,
                         filllabels = filllabels, colorlabels = colorlabels)

  return(p)
}



#' Nice density plot
#'
#' @param data A dataset
#' @param x horizontal axis variable
#' @param color,fill Character. Variables to group on. With changing fill or border color
#' @param xlims,ylims Numeric vectors of 2 elements. Axis limits
#' @param title Plot title
#' @param weight Character. Optional variable with weights.
#' @param alpha Opacity of fill color
#' @param bw Numeric. Bandwidth. Controls the smoothness of the plot
#' @param xlab,ylab Axis labels. Ylab default: "density"
#' @param fillname,colorname Optional variable names for groups to be displayed on legend
#' @param filllabels,colorlabels Optional labels for grouos to be displayed on legend
#' @param legendpos Legend position. Options: “left”,“top”, “right” (default), “bottom”
#'
#' @return A ggplot2-plot
#' @export
#'
#' @examples
#' niceDensity(ggplot2::mpg, "displ", bw = 0.2, color = "class", fill = "class")
niceDensity = function(data, x, fill = NULL, weight = 1, color = NULL, alpha = 0.3, bw = NULL,
                       xlims = NULL, ylims = NULL, title = "", xlab = x, ylab = 'density',
                       colorname = color, fillname = fill,
                       colorlabels = NULL, filllabels = NULL,
                       legendpos = "right") {
  x = rlang::sym(x)
  data.table::setDT(data)

  if (weight != 1) weight = rlang::sym(weight)

  if (!is.null(color)) {
    data[, color] = as.factor(data[, color])
    color = rlang::sym(color) }
  if (!is.null(fill)) {
    data[, fill] = as.factor(data[, fill])
    fill = rlang::sym(fill) }

  p = ggplot2::ggplot(data, ggplot2::aes(x = !!x, color = !!color, fill = !!fill, weight = !!weight))

  if (!is.null(bw)) { # custom bandwidth
    p = p +   ggplot2::geom_density(ggplot2::aes(y = ..density..), alpha = alpha, bw = bw, position="identity")}
  else { # standard bandwidth
    p = p +   ggplot2::geom_density(ggplot2::aes(y = ..density..), alpha = alpha, position="identity")}

  p = p +  ggplot2::labs(color = colorname, fill = fillname) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position= legendpos)

  if (!is.null(ylims)) p = p + ggplot2::ylim(ylims)
  if (!is.null(xlims)) p = p + ggplot2::xlim(xlims)

  # change labels if needed.
  p = niceDiscreteLabels(p, fillname =  fillname, colorname = colorname,
                         filllabels = filllabels, colorlabels = colorlabels)

  return(p)
}



#' Binscatter
#'
#' Group variable on the horizontal axis into equal-sized bins and calculate group means
#' for each bin.
#'
#' Backend code source: https://github.com/maximilianeber/binscatter (Maximilian Eber)
#'
#' @param data A dataset
#' @param x horizontal axis variable
#' @param y vertical axis variable
#' @param bins Numeric. Number of bins (default: 20)
#' @param type Type of geom to plot ("point" (default), "pointrange", "line")
#' @param color Character. Variables to group on. With changing color
#' @param xlims,ylims Numeric vectors of 2 elements. Axis limits
#' @param title Plot title
#' @param xlab,ylab Axis labels
#' @param colorname Optional variable names for groups to be displayed on legend
#' @param colorlabels Optional labels for grouos to be displayed on legend
#' @param legendpos Legend position. Options: “left”,“top”, “right” (default), “bottom”
#'
#' @return A ggplot2-plot
#' @export
#'
#' @examples
#' niceBinscatter(ggplot2::mpg, "displ", "hwy", color = "class", type = "pointrange")
niceBinscatter = function(data, x, y, color = NULL, type = "pointrange", bins = 20,
                          xlims = NULL, ylims = NULL, title = "", xlab = x, ylab = y,
                          colorname = color, colorlabels = NULL,
                          legendpos = "right") {
  x = rlang::sym(x)
  y = rlang::sym(y)
  data.table::setDT(data)

  if (!is.null(color)) color = rlang::sym(color)
  p = ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, color = !!color)) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title) +
    ggplot2::layer(
      stat = StatBinscatter, data = data, geom = type,
      position = "identity", inherit.aes = TRUE,
      params = list(na.rm = TRUE, bins = bins)) +
    ggplot2::theme_classic()

  if (!is.null(ylims)) p = p + ylim(ylims)

  # change labels if needed.
  p = niceDiscreteLabels(p, colorname = colorname, colorlabels = colorlabels)

  return(p)
}






