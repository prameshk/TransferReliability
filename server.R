##Copy 12/21/2016 2:07 PM
# This script is for direction # This is updated on 08/13/2016
library(ggplot2)
library(leaflet)
library(reshape)
transferData <- read.csv("testData.csv")

server <- function(input, output, session) {
  
  selectedData <-  reactive({
    if(input$ptime == 'All day'){
      transferData
    }
    else if (input$ptime == 'pm peak'){
      transferData[transferData$stopFromSchedDeptTime >= 54000 & transferData$stopFromSchedDeptTime <= 66600, ]
    }
    else if (input$ptime == 'am peak'){
      transferData[transferData$stopFromSchedDeptTime >= 21600 & transferData$stopFromSchedDeptTime <= 32400, ]
    }
    else if (input$ptime == 'am non-peak'){
      transferData[(transferData$stopFromSchedDeptTime >= 0 & transferData$stopFromSchedDeptTime <= 21600) | (transferData$stopFromSchedDeptTime >= 34200 & transferData$stopFromSchedDeptTime <= 43200), ]
    }
    else {
      transferData[(transferData$stopFromSchedDeptTime >= 43200 & transferData$stopFromSchedDeptTime <= 54000) | (transferData$stopFromSchedDeptTime >= 66600 & transferData$stopFromSchedDeptTime <= 100000), ]
    }
    
    
  })
  
  
  #output$plot1 <- renderPlot({
  #  pie(table(b$transfers), labels = names(table(b$transfers)))
  observeEvent(input$method, {
    if (input$method == 'System-level'){
      observeEvent(input$routes, {
        if(input$routes == 'All routes for analysis'){
          trans  <-  reactive({
            t <- as.data.frame(table(selectedData()$transfer))
            colnames(t) <- c('group', 'Freq')
            levels(t$group) <- c("Failed Transfers", "Successful Transfers")
            t
            
          })
          
          
          
          output$piePlot <- renderPlot({
            
            ggplot(trans(), aes(x = 1, y = Freq, fill = group))+geom_col(position = 'stack', 
                                                                         show.legend = F)+ geom_text(aes(label = paste(group, ': ', Freq, "(", round(Freq*100/sum(Freq), 1), "%)")), 
                                                                                                     position = position_stack(vjust = .5)) + coord_polar(theta = "y") +  theme_void() + ggtitle("Share of successful and failed transfers")
            
            
          })
          waitTimeData <- reactive({
            waitData <- selectedData()
            waitData[waitData$transfer == FALSE, ]$act_wait_time <- waitData[waitData$transfer == FALSE, ]$act_wait_time_failed_transfer
            waitData <- waitData[waitData$act_wait_time>0, ] 
            agg <- aggregate(act_wait_time~sched_wait_time+transfer, waitData, mean)
            agg <- cast(agg, sched_wait_time~transfer, value = "act_wait_time")
            agg2 <- aggregate(X~sched_wait_time+transfer, waitData, length)
            agg2 <- cast(agg2, sched_wait_time~transfer, value = "X")
            colnames(agg2) <- c("sched_time", "Failed", "Success")
            colnames(agg) <- c("sched_time", "FailedTime", "SuccessTime")
            agg2$Success <- agg2$Success/ (agg2$Success + agg2$Failed)
            agg2$Failed <- 1 - agg2$Success 
            #agg$numTransfers <- agg2$X
            agg$exp_wait_time <- agg2$Failed*agg$FailedTime + agg2$Success*agg$SuccessTime
            agg$Failed <-  agg2$Failed*100 
            agg$Success <-  agg2$Success*100 
            agg
          })
          
          output$MAP1 <- renderPlot({
            ggplot(waitTimeData(), aes(sched_time/60)) + 
              geom_line(aes(y = Success, colour = "% Success")) +  geom_point(aes(y = Success))+
              geom_line(aes(y = Failed, colour = "% Failed")) +  geom_point(aes(y = Failed)) + theme_minimal() + 
              theme(legend.position="bottom") + xlab("Scheduled wait time (min)") + 
              scale_x_continuous(breaks = round(seq(0, 10, by = 0.5)))+
              ylab("Percent transfers") + ggtitle("% of successful and failed transfer wrt scheduled wait time") 
            
          })
          
          output$MAP2 <- renderPlot({
            min <- waitTimeData()[round(waitTimeData()$exp_wait_time/60) == min(round(waitTimeData()$exp_wait_time/60)), ]$sched_time/60
            act <- min(round(waitTimeData()$exp_wait_time/60))
            ggplot(waitTimeData(), aes(sched_time/60)) + 
              geom_line(aes(y = round(exp_wait_time/60)), color = "green") +  geom_point(aes(y = round(exp_wait_time/60)))+
              annotate(geom = "point", x = min, y = act , shape=23, fill="blue", color="darkred", size=3)+theme_minimal()+
              theme(legend.position="bottom") + xlab("Scheduled wait time (min)") + 
              scale_x_continuous(breaks = round(seq(0, 10, by = 0.5)))+
              ylab("Expected wait time (min)") + ggtitle("Expected versus scheduled wait time(min)") 
            
          })
          
          
          
          
          output$distPlot1 <- renderPlot({
            ggplot(selectedData()[selectedData()$transfer == TRUE, ], aes(x = sched_wait_time/60, y = 
                                                                            act_wait_time/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
              
              geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") +
              scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
              labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of successful transfers")  + theme_bw() + theme(legend.position="bottom")+
              geom_abline(intercept=0, slope=1, color = "blue")
            
          })
          output$distPlot2 <- renderPlot({
            ggplot(selectedData()[selectedData()$transfer == FALSE & selectedData()$act_wait_time_failed_transfer > 0 , ], aes(x = sched_wait_time/60, y = 
                                                                                                                                 act_wait_time_failed_transfer/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
              geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") + 
              scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
              labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of failed transfers") + theme_bw() + theme(legend.position="bottom")+
              geom_abline(intercept=0, slope=1, color = "blue")
            
            
            
          })
          
          
          output$table <- renderTable({
            links <- aggregate(X~stopFromName+stop_name, selectedData(), length)
            colnames(links) <- c("From Stop", "To Stop", "# of transfer links")
            links
          })
          
          
        }
        else if(input$routes == 'Two routes only'){
          
          output$fromDir <- renderUI({
            selectInput('Direction1', 'From route Direction', choices = unique(selectedData()[selectedData()$routeFrom == input$routeFrom, ]$stopFromDir) )
            
          })
          # Based on route you choice this will create the directions available for that route.0
          output$toDir <- renderUI({
            
            selectInput('Direction2', 'To route Direction', choices = unique(selectedData()[selectedData()$route_short_id == input$routeTo, ]$dir))
            
            
          })
          output$hmap <- renderUI({
            
            radioButtons('heatTransfer','Select heatmap for', c('Failed transfers', 'Successful transfers'))
            
            
          })
          
          
          twoRouteData <- reactive({
            selectedData()[selectedData()$routeFrom == input$routeFrom & selectedData()$route_short_id == input$routeTo & selectedData()$stopFromDir == input$Direction1 & selectedData()$dir == input$Direction2, ]
            
          })
          
          
          
          
          trans  <-  reactive({
            t <- as.data.frame(table(twoRouteData()$transfer))
            colnames(t) <- c('group', 'Freq')
            levels(t$group) <- c("Failed Transfers", "Successful Transfers")
            t
            
          })
          
          output$piePlot <- renderPlot({
            
            ggplot(trans(), aes(x = 1, y = Freq, fill = group))+geom_col(position = 'stack', 
                                                                         show.legend = F)+ geom_text(aes(label = paste(group, ': ', Freq, "(", round(Freq*100/sum(Freq), 1), "%)")), 
                                                                                                     position = position_stack(vjust = .5)) + coord_polar(theta = "y") +  theme_void() +
              ggtitle("Share of successful and failed transfers")
            
            
          })
          
          
          
          
          output$distPlot1 <- renderPlot({
            ggplot(twoRouteData()[twoRouteData()$transfer == TRUE, ], aes(x = sched_wait_time/60, y = 
                                                                            act_wait_time/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
              
              geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") +
              scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
              labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of successful transfers")  + theme_bw() + theme(legend.position="bottom")+
              geom_abline(intercept=0, slope=1, color = "blue")
            
          })
          output$distPlot2 <- renderPlot({
            ggplot(selectedData()[twoRouteData()$transfer == FALSE & twoRouteData()$act_wait_time_failed_transfer > 0 , ], aes(x = sched_wait_time/60, y = 
                                                                                                                                 act_wait_time_failed_transfer/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
              geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") + 
              scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
              labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of failed transfers") + theme_bw() + theme(legend.position="bottom")+
              geom_abline(intercept=0, slope=1, color = "blue")
            
            
          })
          
          
          waitTimeData <- reactive({
            waitData <- twoRouteData()
            waitData[waitData$transfer == FALSE, ]$act_wait_time <- waitData[waitData$transfer == FALSE, ]$act_wait_time_failed_transfer
            waitData <- waitData[waitData$act_wait_time>0, ] 
            agg <- aggregate(act_wait_time~stopFromName+stop_name+transfer, waitData, mean)
            agg <- cast(agg, stopFromName+stop_name~transfer, value = "act_wait_time")
            agg2 <- aggregate(X~stopFromName+stop_name+transfer, waitData, length)
            agg2 <- cast(agg2, stopFromName+stop_name~transfer, value = "X")
            colnames(agg2) <- c("stopFrom", "stopTo","Failed", "Success")
            colnames(agg) <- c("stopFrom", "stopTo","FailedTime", "SuccessTime")
            agg2$Success <- agg2$Success/ (agg2$Success + agg2$Failed)
            agg2$Failed <- 1 - agg2$Success 
            #agg$numTransfers <- agg2$X
            agg$expectedWaitTime <- round((agg2$Failed*agg$FailedTime + agg2$Success*agg$SuccessTime)/60)
            agg$Failed <-  agg2$Failed*100 
            agg$Success <-  agg2$Success*100 
            agg <- agg[is.na(agg$expectedWaitTime) == FALSE, ]
            agg
          })
          
          
          
          output$table <- renderTable({
            links <- aggregate(X~stopFromName+stop_name, twoRouteData(), length)
            colnames(links) <- c("From Stop", "To Stop", "# of transfer links")
            links
            
          })
          
          output$MAP1 <- renderPlot({
            if(input$heatTransfer== 'Failed transfers'){
              ggplot(waitTimeData(), aes(stopFrom, stopTo)) + 
                geom_tile(aes(fill = Failed), colour = "white") + xlab("FromStop") + ylab("ToStop") + ggtitle("Heatmap of percentage of failed transfers") + scale_fill_gradient(low="yellow", high="red") + labs(color='% of failed transfers') +
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
              
            }
            else{
              
              ggplot(waitTimeData(), aes(stopFrom, stopTo)) + 
                geom_tile(aes(fill = Success), colour = "white") + xlab("FromStop") + ylab("ToStop") + ggtitle("Heatmap of percentage of successful transfers") + scale_fill_gradient(low="yellow", high="red") + labs(color='% of successful transfers')+
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
              
            }
            
          })
          output$MAP2 <- renderPlot({
            ggplot(waitTimeData(), aes(stopFrom, stopTo)) + 
              geom_tile(aes(fill = expectedWaitTime), colour = "white") + xlab("FromStop") + ylab("ToStop") + ggtitle("Heatmap of average actual wait time (min) for transfers") + scale_fill_gradient(low="yellow", high="red")  + 
              labs(color='Expected wait time (min)')+
              theme(axis.text.x = element_text(angle = 60, hjust = 1))
            
            
            
          })
          
          
          
        }
      })
    }
    #######################################################################################################################
    # Single route data
    else{
      output$dir <- renderUI({
        selectInput('direction', 'Select direction', choices = unique(selectedData()[selectedData()$route_short_id == input$route, ]$dir) )
        
      })
      output$heats <- renderUI({
        
        radioButtons('heatTransfer','Select heatmap of % of transfers ', c('Failed transfers', 'Successful transfers'))
        
        
      })
      
      singleRouteData <- reactive({
        selectedData()[selectedData()$route_short_id == input$route & selectedData()$dir == input$direction, ]
        
      })
      
      
      trans  <-  reactive({
        t <- as.data.frame(table(singleRouteData()$transfer))
        colnames(t) <- c('group', 'Freq')
        levels(t$group) <- c("Failed Transfers", "Successful Transfers")
        t
        
      })
      
      output$piePlot <- renderPlot({
        
        ggplot(trans(), aes(x = 1, y = Freq, fill = group))+geom_col(position = 'stack', 
                                                                     show.legend = F)+ geom_text(aes(label = paste(group, ': ', Freq, "(", round(Freq*100/sum(Freq), 1), "%)")), 
                                                                                                 position = position_stack(vjust = .5)) + coord_polar(theta = "y") +  theme_void() +
          ggtitle("Share of successful and failed transfers")
        
        
      })
      
      
      
      
      
      
      output$distPlot1 <- renderPlot({
        ggplot(singleRouteData()[singleRouteData()$transfer == TRUE, ], aes(x = sched_wait_time/60, y = 
                                                                              act_wait_time/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
          
          geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") +
          scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
          labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of successful transfers")  + theme_bw() + theme(legend.position="bottom")+
          geom_abline(intercept=0, slope=1, color = "blue")
      })
      output$distPlot2 <- renderPlot({
        ggplot(selectedData()[singleRouteData()$transfer == FALSE & singleRouteData()$act_wait_time_failed_transfer > 0 , ], aes(x = sched_wait_time/60, y = 
                                                                                                                                   act_wait_time_failed_transfer/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
          geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") + 
          scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
          labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of failed transfers") + theme_bw() + theme(legend.position="bottom")+
          geom_abline(intercept=0, slope=1, color = "blue")
        
      })
      
      waitTimeData <- reactive({
        waitData <- singleRouteData()
        waitData[waitData$transfer == FALSE, ]$act_wait_time <- waitData[waitData$transfer == FALSE, ]$act_wait_time_failed_transfer
        waitData <- waitData[waitData$act_wait_time>0, ] 
        agg <- aggregate(act_wait_time~stopFromName+stop_name+transfer, waitData, mean)
        agg <- cast(agg, stopFromName+stop_name~transfer, value = "act_wait_time")
        agg2 <- aggregate(X~stopFromName+stop_name+transfer, waitData, length)
        agg2 <- cast(agg2, stopFromName+stop_name~transfer, value = "X")
        colnames(agg2) <- c("stopFrom", "stopTo","Failed", "Success")
        colnames(agg) <- c("stopFrom", "stopTo","FailedTime", "SuccessTime")
        agg2$Success <- agg2$Success/ (agg2$Success + agg2$Failed)
        agg2$Failed <- 1 - agg2$Success 
        #agg$numTransfers <- agg2$X
        agg$expectedWaitTime <- round((agg2$Failed*agg$FailedTime + agg2$Success*agg$SuccessTime)/60)
        agg$Failed <-  agg2$Failed*100 
        agg$Success <-  agg2$Success*100 
        agg <- agg[is.na(agg$expectedWaitTime) == FALSE, ]
        agg
      })
      
      

      
      
      
      output$table <- renderTable({
        links <- aggregate(X~routeFrom+stopFromName+stop_name, singleRouteData(), length)
        colnames(links) <- c("From route", "From Stop", "To Stop", "# of transfer links")
        links
      })
      
      output$MAP1 <- renderPlot({
        if(input$heatTransfer== 'Failed transfers'){
          ggplot(waitTimeData(), aes(stopFrom, stopTo)) + 
            geom_tile(aes(fill = Failed), colour = "white") + xlab("FromStop") + ylab("ToStop") + ggtitle("Heatmap of percentage of failed transfers") + scale_fill_gradient(low="yellow", high="red") + labs(color='% of failed transfers') +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
          
        }
        else{

          ggplot(waitTimeData(), aes(stopFrom, stopTo)) + 
            geom_tile(aes(fill = Success), colour = "white") + xlab("FromStop") + ylab("ToStop") + ggtitle("Heatmap of percentage of successful transfers") + scale_fill_gradient(low="yellow", high="red") + labs(color='% of successful transfers')+
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
          
        }
        
      })
      output$MAP2 <- renderPlot({
        ggplot(waitTimeData(), aes(stopFrom, stopTo)) + 
          geom_tile(aes(fill = expectedWaitTime), colour = "white") + xlab("FromStop") + ylab("ToStop") + ggtitle("Heatmap of average actual wait time (min) for transfers") + scale_fill_gradient(low="yellow", high="red")  + 
          labs(color='Expected wait time (min)')+
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
        
        
        
      })
      
      
    }
  })
}



