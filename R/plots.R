#' Plot the first two principle components
#'
#' Given a data.frame, plot the first two principle components and label them
#'
#'
#' @param df a data.frame with columns referring to different experimental conditions
#' @param groups group by which to color columns by
#' @param useColLabels if true, print the column names on the plot
#' @param centerDf center df in prcomp
#' @param scaleDf scale df in prcomp
#' @return a ggplot object
#' @export
#' @examples
#' require(graphics)
#'
#' head(USArrests)
#' pcaPlot(USArrests)
#' pcaPlot(USArrests[,c('Murder', 'Assault', 'Rape')])
#' pcaPlot(USArrests, c('crime', 'crime', 'pop', 'crime'))
#' pcaPlot(USArrests, c('crime', 'crime', 'pop', 'crime'), F)
pcaPlot <- function(df, groups = NULL, useColLabels = TRUE,
                    centerDf = TRUE, scaleDf = FALSE)
{
    if (class(df) != "data.frame")
    {
        df <- as.data.frame(df)
        if (class(df) != "data.frame")
            stop("Error: pcaPlot requires a data.frame")
    }

    if ((length(groups) != ncol(df)) & (!is.null(groups)))
        stop("Error: length(groups) != ncol(df)")

    pcaRes <- prcomp(df, center = centerDf, scale. = scaleDf)

    df <- data.frame(pcaRes$rotation[,1:2], lab = colnames(df))

    plt <- NULL
    if (!is.null(groups))
    {
        df <- data.frame(df, groups = groups)
        plt <- ggplot(df, aes(PC1, PC2, colour = groups, label = lab)) 
    }
    else
    {
        plt <- ggplot(df, aes(PC1, PC2, label = lab)) 
    }

    if (useColLabels)
        plt + geom_text()
    else
        plt + geom_point()
}

#' Conditional boxplots
#'
#' Plot boxplots of y conditional on the quantile of x
#'
#' @param df a data.frame with variables of interest
#' @param y string representing the column name in df
#' @param x string representing the column to condition on
#' @param binSize how much spacing in the quantiles to include
#' @param yQuantile if TRUE, also plot y's quantiles
#' @return a ggplot object
#' @export
conditionalBoxplots <- function(df, y, x, binSize = 0.05, yQuantile = FALSE)
{
    df$xQuant <- ecdf(df[,x])(df[,x])
    df$xGroup <- cut(df$xQuant, seq(0, 1, by = binSize))
    yTitle <- y
    if (yQuantile)
    {
        df$yQuant <- ecdf(df[,y])(df[,y])
        df$yGroup <- cut(df$yQuant, seq(0, 1, by = binSize))
        yTitle <- paste0(yTitle, ' quantile')
        y <- 'yGroup'
    }
    plt <- ggplot(df, aes_string('xGroup', y, group = 'xGroup', fill = 'xGroup'))
    plt <- plt +  geom_boxplot()
    plt <- plt + xlab(paste0(x,' quantile'))
    plt <- plt + ylab(yTitle)
    plt <- plt + guides(fill=guide_legend(title="quantile"))
    plt
}
