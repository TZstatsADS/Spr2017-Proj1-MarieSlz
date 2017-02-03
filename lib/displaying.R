
#### function wordcloud.comparison ####
# compares the wordclouds of the 1st speech and the 2nd speech of a given president
wordcloud.comparison <- function (dtm.tidy, president) {
  # Worcloud
  term1 <- paste0(president,"-1")
  term2 <- paste0(president,"-2")
  selected.data1 <- dtm.tidy[dtm.tidy$document==term1,]
  selected.data2 <- dtm.tidy[dtm.tidy$document==term2,]
  
  layout(matrix(c(1,3,2,4), nrow=2, ncol=2), heights=c(0.5, 3))
  #layout.show(4)
  par(mar=c(1,0,0,0)) #margins
  plot.new()
  text(x=0.5, y=0.5, cex=2, "1st Term", font=4)
  plot.new()
  text(x=0.5, y=0.5, cex=2, "2nd Term", font=4)
  wordcloud(selected.data1$word, selected.data1$count,
            main = "1st Term",
            scale=c(5,0.5),
            max.words=70,
            min.freq=1,
            random.order=FALSE,
            rot.per=0.0,
            use.r.layout=T,
            random.color=FALSE,
            colors=brewer.pal(9,"Blues"))
  wordcloud(selected.data2$word, selected.data2$count,
            main = "2nd Term",
            scale=c(5,0.5),
            max.words=70,
            min.freq=1,
            random.order=FALSE,
            rot.per=0.0,
            use.r.layout=T,
            random.color=FALSE,
            colors=brewer.pal(9,"Greens"))
  # display.brewer.all()
}

#### function ggplot.flow ####
# takes the corpus of text and the name of a president and display the overall sentiment
# .. (positive, negative) over time during the speech
ggplot.flow <-function(corpus, president){
  speech.flow.1 <- flow(corpus, president, 1)
  speech.flow.1$term <- "Term 1"
  speech.flow.2 <- flow(corpus, president, 2)
  speech.flow.2$term <- "Term 2"
  
  speech.flow <- rbind(speech.flow.1, speech.flow.2)
  speech.flow$colour <- ifelse(speech.flow$score < 0, "negative","positive")
  
  g <- ggplot(speech.flow, aes(section, score, fill=colour)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values=c(positive="steelblue",negative="firebrick1")) +
    facet_wrap(~ term, nrow = 2) +
    xlab("Speech") +
    ylab("") +
    ggtitle(president)
  
  return (g)
}

#### function grid_arrange_shared_legend ####
# Takes ggplot graphs and combine the legend
# Created by: Eduardo García-Portugués
# https://github.com/tidyverse/ggplot2/wiki/share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}