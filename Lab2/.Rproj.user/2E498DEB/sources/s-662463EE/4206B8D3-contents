library(dplyr)
library(ggplot2)
drg.data <-read.csv('DRG_data.csv') %>%
          group_by(DRG.Definition)%>%
          mutate(DRG =strsplit(as.character(DRG.Definition),split =' ')[[1]][1])%>%
          ungroup()


#' Boxplot for DRG code
#'
#' @param y1
#'
#' @return plot
#' @export
#'
#' @examples  drg.boxplot("Average.Total.Payments")
#'
drg.boxplot = function(y1){
  if(y1 == "Average.Medicare.Payments"){
  ggplot(drg.data,aes(x = DRG,y = Average.Medicare.Payments))+
    scale_y_continuous(trans='log10')+
    geom_boxplot(outlier.shape = NA)+
    theme(axis.text.x =element_text(angle = 90,size = 6,hjust = 1))+
    ylab("Log Average Medicare Payments")+xlab('DRG Code')+
    ggtitle("Average Medicare Payments for Hospitals by DRG Code")
  }
  else if(y1 == "Average.Total.Payments"){
    ggplot(drg.data,aes(x = DRG,y = Average.Total.Payments))+
      scale_y_continuous(trans='log10')+
      geom_boxplot(outlier.shape = NA)+
      theme(axis.text.x =element_text(angle = 90,size = 6,hjust = 1))+
      ylab("Log Average Total Payments")+xlab('DRG Code')+
      ggtitle("Average Total Payments for Hospitals by DRG Code")
  }
}

drg.boxplot("Average.Total.Payments")


