library(tidyverse)

generate_raster <- function(rows,table_columns,graph_columns,width_graph) {
  distance_table_graph <- 0.5
  margin <- 0.0

  height <- rows+1+2*margin #rows+botom+top
  width <- length(table_columns)+width_graph+distance_table_graph+2*margin
  graph_start <- length(table_columns)+distance_table_graph
  graph_end <- length(table_columns)+distance_table_graph+width_graph

  raster <- list("height"=height,
                 "width"=width,
                 "graph_start" = graph_start,
                 "graph_end" = graph_end)

  raster
}

plot_mapper <- function(plot.start,
                        plot.end,
                        plot.range,
                        log.range) {
  mapper <- NULL
  a <- plot.range[1]
  b <- plot.range[2]
  s <- plot.start
  e <- plot.end

  if(log.range) {
    mapper <- function(x) {
      x[x < a & x > b] <- NA

      x <- log10(x)
      a <- log10(a)
      b <- log10(b)
      m <- (e-s)/(b-a)
      q <-0.5*(e+s-m*(b+a))

      y <- x*m+q

      y
    }
  } else {
    mapper <- function(x) {
      x[x < a & x > b] <- NA

      m <- (e-s)/(b-a)
      q <-0.5*(e+s-m*(b+a))

      y <- x*m+q

      y
    }
  }
  mapper
}

ggforest <- function(data,table.columns,graphic.columns = c("estimate","lower","upper"),
                     color.columns = NULL,
                     xlab = "Hazard Ratio",
                     log.range = T,
                     x.breaks = c(0.1,1,10),
                     raster.variable,
                     over.flow = "arrow") {

  data[["row"]] <- nrow(data):1
  text_data <- select(data,c(table.columns,"row"))
  graphic_data <- select(data,c(graphic.columns,color.columns,"row"))

  if(x.breaks == "dynamic") {
    min_break <- max(min(round(graphic_data[["lower"]],2)),0.01)
    max_break <- min(max(round(graphic_data[["upper"]],2)),100)

    x.breaks <- c(min_break,max_break,1) %>% unique() %>% sort()
    if(min_break > 1 || max_break < 1) {
      x.breaks <- c(min_break,max_break) %>% unique() %>% sort()
    }
  }

  plot_range <- c(min(x.breaks),max(x.breaks))

  raster <- generate_raster(nrow(data),table.columns,plot.columns,2)

  mapper <- plot_mapper(raster$graph_start,raster$graph_end,c(min(x.breaks),max(x.breaks)),log.range)



  raster.groups <- text_data %>% select(c(raster.variable,row))
  raster.groups[["yinter"]] <- 0
  for(i in 1:(length(raster.groups[[raster.variable]])-1)) {
    if(raster.groups[[raster.variable]][i] != raster.groups[[raster.variable]][i+1]) {
      raster.groups$yinter[i] <- raster.groups$row[i]-0.5
    }
  }
  raster.groups %>% filter(yinter > 0) -> raster.groups
  #work on text data
  text_data <- data.frame(lapply(text_data, as.character), stringsAsFactors=FALSE)


  column.map <- 1:length(table.columns)
  names(column.map) <- table.columns

  #turn all columns into characters
  text_data <- data.frame(lapply(text_data, as.character), stringsAsFactors=FALSE)

  text_data_zeroth_row <- table.columns

  text_data <- rbind(c(text_data_zeroth_row,nrow(text_data)+1),text_data)

  text_data %>% tidyr::gather(key = "column",value="text",-row) -> text_data

  text_data %>% mutate(position.y = row %>% as.numeric(),
                       position.x = column.map[column]) -> text_data

  #graphic_data
  if(is.null(color.columns)) {
    graphic_data[["estimate.color"]] <- "black"
    graphic_data[["conf.color"]] <- "black"
  }

  #deal with overflow
  if(over.flow == "arrow") {
    graphic_data %>% mutate(arrow_left = if_else(lower < min(x.breaks),T,F),
                            arrow_right = if_else(upper > max(x.breaks),T,F)) %>%
      mutate(lower = if_else(lower < min(x.breaks),min(x.breaks),lower),
             estimate = if_else(estimate < min(x.breaks),min(x.breaks),estimate),
             estimate = if_else(estimate > max(x.breaks),max(x.breaks),estimate),
             upper = if_else(upper > max(x.breaks),max(x.breaks),upper)) -> graphic_data

    #careful u should also do something about the estimate!!!
  } else if(over.flow == "cut") {
    graphic_data %>% mutate(lower = if_else(lower < min(x.breaks),min(x.breaks),lower),
                            estimate = if_else(estimate < min(x.breaks),min(x.breaks),estimate),
                            estimate = if_else(estimate > max(x.breaks),max(x.breaks),estimate),
                            upper = if_else(upper > max(x.breaks),max(x.breaks),upper)) -> graphic_data
  } else if(over.flow == "ignore") {
    #do nothig, can cause nonplots
  }

  for(column in graphic.columns) {
    graphic_data[[column]] <- mapper(graphic_data[[column]])
  }

  theme_table <- theme(panel.background = element_blank(),
                       panel.grid.major = element_blank(),
                       legend.position = "none",
                       panel.border = element_blank(),
                       axis.text = element_blank(),
                       axis.title = element_blank(),
                       axis.ticks = element_blank(),
                       axis.line = element_blank(),
                       plot.margin = unit(c(0,0,0,0), "lines"))
  #initiate plot
  plt <- ggplot()+theme_table

  #set limits
  plt <- plt+xlim(c(0.75,raster$width))+ylim(c(-0.5,raster$height))

  #set raster
  plt <- plt+geom_segment(aes(x=mapper(1),xend=mapper(1),y=raster$height-0.5,yend=0.5),linetype=2)+
    geom_hline(aes(yintercept=raster$height-0.5))+
    geom_hline(data=raster.groups,aes(yintercept=yinter),color="grey")

  #set axis
  plt <- plt + geom_segment(aes(x=raster$graph_start,xend=raster$graph_end,y=0.5,yend=0.5))+
    geom_segment(aes(x=mapper(x.breaks),xend=mapper(x.breaks),y=0.5,yend=0.2))+
    geom_text(aes(x=mapper(x.breaks),y=-0.5,label = as.character(x.breaks)))+
    geom_text(aes(x=(raster$graph_end+raster$graph_start)/2,y=raster$height,label = xlab))

  #add table
  plt <- plt +
    geom_text(data = text_data,
              aes(x=position.x,
                  y=position.y,
                  label = text))

  #add plot
  if(over.flow == "arrow") {
    graphic_data_left_arrow <- graphic_data %>% filter(arrow_left)
    graphic_data_left <- graphic_data %>% filter(!arrow_left)
    graphic_data_right_arrow <- graphic_data %>% filter(arrow_right)
    graphic_data_right <- graphic_data %>% filter(!arrow_right)

    plt <- plt + geom_segment(data=graphic_data_left,
                                aes(x=lower,xend=estimate,y=row,yend=row,color=conf.color))

    plt <- plt + geom_segment(data=graphic_data_left_arrow,
                              aes(x=estimate,xend=lower,y=row,yend=row,color=conf.color),
                              arrow=arrow(length=unit(0.30,"cm"),type="closed"))

    plt <- plt + geom_segment(data=graphic_data_right,
                              aes(x=upper,xend=estimate,y=row,yend=row,color=conf.color))

    plt <- plt + geom_segment(data=graphic_data_right_arrow,
                              aes(x=estimate,xend=upper,y=row,yend=row,color=conf.color),
                              arrow=arrow(length=unit(0.30,"cm"),type="closed"))
  } else if(over.flow == "cut") {
    plt <- plt + geom_segment(data=graphic_data,
                              aes(x=lower,xend=upper,y=row,yend=row,color=conf.color))
  } else if(over.flow == "ignore") {
    #not yet supported
  }

  plt <- plt + geom_point(data = graphic_data,
               aes(x=estimate,y=row,color=estimate.color),size=2)

  plt
}
