#'@ Drops axis from ggplot object
drop_axis <- function (axis = "both")
{
  if (axis == "y") {
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(
        colour = NULL,
        size = NULL,
        linetype = NULL,
        lineend = NULL
      ),
      axis.line.y = ggplot2::element_blank()
    )
  }
  else if (axis == "x") {
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(
        colour = NULL,
        size = NULL,
        linetype = NULL,
        lineend = NULL
      ),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  else if (axis == "neither") {
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(
        colour = NULL,
        size = NULL,
        linetype = NULL,
        lineend = NULL
      ),
      axis.line.y = ggplot2::element_line(
        colour = NULL,
        size = NULL,
        linetype = NULL,
        lineend = NULL
      )
    )
  }
  else if (axis == "both") {
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  else {
    stop(
      "Invalid \"axis\" argument. Valid arguments are: ",
      "\"x\", \"y\", \"both\", and \"neither.\"",
      call. = FALSE
    )
  }
}

#'@ Forces expansion of axis limits so that bars reach 0
#'
#'@param type Is axis continuous or discrete?
#'@param axis Which axis?
#'@param ... Other arguments to be passed to the scale_* argument
fix_bars <- function (type = "continuous", axis = "y", ...)
{
  if (axis == "y" && type == "continuous") {
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.001)), ...)
  }
  else if (axis == "y" && type == "discrete") {
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0, 0.001)), ...)
  }
  else if (axis == "x" && type == "continuous") {
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.001)), ...)
  }
  else if (axis == "x" && type == "discrete") {
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0, 0.001)), ...)
  }
}
