
# Часть первая ------------------------------------------------------------

if (!require("gsheet")){
  install.packages("gsheet")
} else{
  library(gsheet)
}


url <- "docs.google.com/spreadsheets/d/165sp-lWd1L4qWxggw25DJo_njOCvzdUjAd414NSE8co/edit#gid=1439079331"

df <- read.csv(text = gsheet2text(url, format='csv'),
            header = T,
            stringsAsFactors = FALSE)

cols <- c(names(df))
df <-  df[, !(cols %in% cols[5])] # странно импортировался header колонки good, 
                                  # потому удаляю её через имя внутри скрипта
df[5] <- as.integer(unlist(df[5])) 
df <- na.omit(df) 


areas <- unlist(unique(df[1]))
out <- data.frame()

for (val in areas){  # удаляем повторяющиеся слова внутри областей и задаем цвета
  tmp <- df[df[1] == val,]
  tmp <- tmp[!duplicated(tmp$keyword),]
  tmp$color <- seq(1, nrow(tmp))
  colors_tmp <- c("darkblue", "green", "red", "yellow")
  for (cluster in unique(tmp$cluster) ){
    color <- sample(colors_tmp, 1)
    tmp[tmp$cluster == cluster, ]$color <- color
    colors_tmp <- colors_tmp[!colors_tmp == color]
  }
  out <- rbind(out,tmp)
}

# rm(list = c("tmp", "cols", "areas", "colors_tmp", "color", "val", "cluster"))

write.csv(out, "./export.csv", row.names = F)


# Часть вторая ------------------------------------------------------------


library(ggplot2)

if (!require("ggplot2")){
  install.packages("ggplot2")
} else{
  library(ggplot2)
}

if (!require("ggrepel")){
  install.packages("ggrepel")
} else{
  library(ggrepel)
}
 
for (val in areas){
  tmp <- out[out[1] == val,]
  tmp$keyword <- as.character(gsub("(\\w+\\s\\w+)\\s", "\\1\n", tmp$keyword))
  tmp$cluster_name <- as.character(tmp$cluster_name)
  int <- c(min(tmp$x) - 4.5, max(tmp$x) + 4.5, min(tmp$y) - 4.5, max(tmp$y) + 4.5)
  colors <- list()
  p <- ggplot(NULL, aes(x, y)) 
  for (clust in unique(tmp$cluster)){
    points <- as.data.frame(tmp[tmp[2] == clust,])
    scaled_size <- points$count / 500
    p <- p + 
      geom_point(data = points, 
                  aes(fill = unique(cluster_name)),
                  size = scaled_size + 3, pch = 21, color = "black") +
      geom_text(data = points, 
                      aes(label = as.vector(keyword)),
                      size = scaled_size + 1.5 ,
                      color = "black")
    colors <- c(unique(points$color), colors)
  }
  p <- p +
    ggtitle(paste("Область", val)) +
    scale_fill_discrete( type =  rev(unlist(colors)), name = "Кластеры") +
    xlim(int[1], int[2]) +
    ylim(int[3], int[4]) +
    theme_light() +
    theme(axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.box.background = element_rect(colour = "black")) 
  
  ggsave(paste0(val, ".png"), width = 2000, height = 1500, units = "px", dpi = 180)
}

