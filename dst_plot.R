library(dplyr)
library(stringr)
library(ggplot2)


############ settings ##############
#maze4 <- c('maze4_path_weight100_weight30.csv','Maze4 - 201801162151 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv') #Maze4 - 201801162151 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv')
maze4 <- c('maze4_path_weight100_weight30.csv','maze4 - 201801222157 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv')
maze5 <- c('maze5_path_weight100_weight30.csv','nxcs.testbed.maze5_weighted_sum - 201801222158 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv') #Maze5 - 201801162151 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv')
maze6 <- c('maze6_path_weight100_weight30.csv','maze6 - 201801222201 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv') #Maze6 - 201801151104 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv.csv')
dst <- c('dst_path.csv','nxcs.testbed.dst_weighted_sum - 201803311404 - Trial 0 - TRIAL_NUM - 25000 - TEST.csv')
dst2 <- c('dst_path.csv','nxcs.testbed.dst_weighted_sum - 201804182045 - Trial 0 - TRIAL_NUM - 25000 - TEST.csv')

#dst3 <- c('dst_path.csv','nxcs.testbed.dst_weighted_sum - 201805212152 - Trial 0 - TRIAL_NUM - 150000 - TEST.csv')

dst3 <- c('dst_path.csv','nxcs.testbed.dst_weighted_sum - 201805262102 - Trial 0 - TRIAL_NUM - 150000 - TEST.csv')

dst32 <- c('dst_path.csv','nxcs.testbed.dst_weighted_sum - 201805262103cp2 - Trial 0 - TRIAL_NUM - 150000 - TEST.csv')

dst33 <- c('dst_path.csv','nxcs.testbed.dst_weighted_sum - 201805262103cp1 - Trial 0 - TRIAL_NUM - 150000 - TEST.csv')


upperBound <- 6000
# traceWeightFilter <- c('0.040000|0.960000'
#                        , '0.480000|0.520000'
#                        #, '0.520000|0.480000'
#                        , '0.960000|0.040000') #c('0.000000|1.000000', '0.560000|0.440000', '1.000000|0.000000')
# 
# plot.labels <- list(expression(paste(lambda[1],'=0.04, 0.96','  ',sep=''))
#                     , expression(paste(lambda[2],'=0.48, 0.52','  ',sep=''))
#                     #, expression(paste(lambda[3],'=0.52, 0.48',sep=''))
#                     , expression(paste(lambda[3],'=0.96, 0.04','  ',sep='')) )


traceWeightFilter <- c("0.000000|1.000000", 
                       "0.040000|0.960000", 
                       "0.080000|0.920000", 
                       "0.120000|0.880000", 
                       "0.160000|0.840000"
                       , "0.200000|0.800000", 
                       "0.240000|0.760000", 
                       "0.280000|0.720000", 
                       "0.320000|0.680000", 
                       "0.360000|0.640000"
                       , "0.400000|0.600000", 
                       "0.440000|0.560000", 
                       "0.480000|0.520000", 
                       "0.520000|0.480000", 
                       "0.560000|0.440000"
                       , "0.600000|0.400000", 
                       "0.640000|0.360000", 
                       "0.680000|0.320000", 
                       "0.720000|0.280000", 
                       "0.760000|0.240000"
                       , "0.800000|0.200000", 
                       "0.840000|0.160000", 
                       "0.880000|0.120000", 
                       "0.920000|0.080000", 
                       "0.960000|0.040000"
                       , "1.000000|0.000000"
)

plot.upperBound <- 6000
plot.traceWeightFilter <- c(#"0.000000|1.000000", 
  "0.040000|0.960000", 
  #"0.080000|0.920000", 
  # "0.120000|0.880000", 
  # "0.160000|0.840000"
  # , "0.200000|0.800000", 
  # "0.240000|0.760000", 
  # "0.280000|0.720000", 
  # "0.320000|0.680000", 
  # "0.360000|0.640000"
  # , "0.400000|0.600000", 
  # "0.440000|0.560000", 
  "0.480000|0.520000", 
  "0.520000|0.480000", 
  # "0.560000|0.440000"
  # , "0.600000|0.400000", 
  # "0.640000|0.360000", 
  # "0.680000|0.320000", 
  # "0.720000|0.280000", 
  # "0.760000|0.240000"
  # , "0.800000|0.200000", 
  # "0.840000|0.160000", 
  # "0.880000|0.120000", 
  # "0.920000|0.080000", 
  "0.960000|0.040000"
  # , "1.000000|0.000000"
)

plot.labels <- list(#expression(paste(lambda[0],'=0.0, 1.0','  ',sep=''))
  #, expression(paste(lambda[1],'=0.11, 0.89','  ',sep=''))
  expression(paste(lambda^1,'=0.04, 0.96','  ',sep=''))
  #, expression(paste(lambda[3],'=0.33, 0.67','  ',sep=''))
  , expression(paste(lambda^2,'=0.48, 0.52','  ',sep=''))
  , expression(paste(lambda^3,'=0.52, 0.48','  ',sep=''))
  #, expression(paste(lambda[6],'=0.67, 0.33','  ',sep=''))
  , expression(paste(lambda^4,'=0.96, 0.04','  ',sep=''))
  #, expression(paste(lambda[8],'=0.89, 0.11','  ',sep=''))
  #, expression(paste(lambda[9],'=1.0, 0.0','  ',sep='')) 
)

plot.targetReward <- c('200.000000|400.000000')

##################
mazeToRun <- dst3





############# begin to read result #############
#setwd("/Users/773742/Documents/CEC2018/DST2018/")
setwd("C:/Users/martin.xie/Downloads/Result/dst3/")

# targetSteps <- read.csv(file = mazeToRun[1], header = TRUE, sep = ",", stringsAsFactors = FALSE)
# targetId <- paste(targetSteps$open, targetSteps$final, paste(as.character(targetSteps$step), '', sep = ''), sep = '*')
# targetSteps <- cbind(targetSteps, targetId)
# 

#setwd("/Users/773742/Documents/CEC2018/DST2018/")
raw.data1 <- read.csv(file =   mazeToRun[2] #Train - 201801141417 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv.csv"
                     , header = TRUE, sep = ","
                     , stringsAsFactors = FALSE
                     , row.names=NULL)
raw.data2 <- read.csv(file =   dst32[2] #Train - 201801141417 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv.csv"
                     , header = TRUE, sep = ","
                     , stringsAsFactors = FALSE
                     , row.names=NULL)
raw.data3 <- read.csv(file =   dst33[2] #Train - 201801141417 - Trial 0 - TRIAL_NUM - 6000 - TEST.csv.csv"
                     , header = TRUE, sep = ","
                     , stringsAsFactors = FALSE
                     , row.names=NULL)

raw.data <- rbind(raw.data,raw.data2)
raw.data <- rbind(raw.data,raw.data3)


#h5data <- head(raw.data,10000)

data <- raw.data %>% 
  select(TrailNumber, Timestamp, TargetWeight, TraceWeight, obj_r1, OpenState, FinalState, steps, hyperVolumn, path) %>%
  filter(TraceWeight %in% traceWeightFilter
         , Timestamp <= upperBound)

#write.csv(data, file = paste('trim_2_',mazeToRun[2]),row.names=FALSE)

## release memory
#rm(raw.data)

################ check if uid in final state pair ###############
uid <- paste(data$OpenState, data$FinalState, data$steps, sep = "*")
data <- cbind(data, uid)
####match by preset open/final/step
#data$match <- ifelse(data$uid %in% targetSteps$targetId, 1, 0)
rm(uid)
####match by preset open/final/step
data$match <- ifelse(nchar(data$path)>160,0,1)

################ calculate match rate ###############
result <- data %>%
  group_by(TrailNumber, Timestamp,TargetWeight,TraceWeight ) %>%
  summarise(groupRow = n()
            , matchCount = sum(match)
            , matchRate =matchCount/groupRow 
            , hyperVolumn = mean(hyperVolumn))

# uniqTrail <- unique(result$TrailNumber)
# pall <- rep(NULL, nrow(uniqTrail))
# pdata <- NULL
# 
# for (i in uniqTrail) {
#     pdata <- result %>%
#         filter(TrailNumber == i
#         #, TraceWeight == '5.000000|5.000000'
#         #, TraceWeight == uniqWeight[i] #' 0.000000|1.000000'
#         )
#     ggplot(pdata, aes(x = Timestamp, y = matchRate, group = TraceWeight, color = TraceWeight, linetype = TraceWeight) )+
#       geom_line() +
#       labs(title = paste('Trail', i,sep=' ')) 
# }

#ggplot(pdata, aes(x = Timestamp, y = matchRate, group = TraceWeight, color = c('#41ae76', '#ef6548', '#4292c6'))) +
#    geom_line()



################ calculate mean match rate and hyper volume ###############
retdata <- result %>%
  group_by(Timestamp, TargetWeight, TraceWeight) %>%
  summarise(matchRateAvg = mean(matchRate) 
            , hyperVolumnAvg = mean(hyperVolumn)
            , maxmr = max(matchRate)
            , minmr = min(matchRate)
            , maxhv = max(hyperVolumn)
            , minhv = min(hyperVolumn))


plt <- ggplot(retdata, aes(x = Timestamp, y = matchRateAvg, group = TraceWeight, color = TraceWeight, linetype = TraceWeight)) +
  geom_line()



########### plot begin ###########
theme_set(theme_classic(base_size = 9))


lty = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
lshp = c(1, 2, 3, 4, 5, 6, 7, 8, 9,10)
cbbPalette = c('#e41a1c', '#377eb8', '#4daf4a'
               , '#984ea3', '#ff7f00', '#66ff66'
               , '#a65628', '#f781bf', '#000000'
               ,'#f781bf')


################ plot data ###############
plot.data <- retdata %>% filter(TraceWeight  %in% plot.traceWeightFilter
                                , Timestamp <= plot.upperBound
                                #, TargetWeight %in% plot.targetReward
)



#write.csv(plot.data, file = paste('plot_',mazeToRun[2]),row.names=FALSE)

################ plot hyper volume ###############
phv <- ggplot(data = plot.data, aes(
  x = Timestamp,
  y = hyperVolumnAvg,
  colour = TraceWeight,
  group = TraceWeight,
  linetype = TraceWeight
)) +
  geom_line() +
  #geom_ribbon(aes(ymin = minhv, ymax = maxhv, fill = TraceWeight), alpha = 0.2) +
  labs(x = 'Number of Learning Problems\n(a)', y = NULL) +
  ggtitle("THV") +
  theme(axis.title.y = element_text(size = rel(1.1), face = "bold"), axis.title.x = element_text(size = rel(1.1), face = "bold"), title = element_text(size = rel(1.1), face = 'bold')) +
  theme(legend.text = element_text(size = rel(1.5), face = "bold")) +
  theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.63, 0.15))
  theme(legend.position = 'bottom') + theme(panel.grid.major = element_line(size = 0.01, linetype = 'dotted',
                                                                            colour = "black"),
                                            panel.grid.minor = element_line(size = 0.001, linetype = 'dotted',
                                                                            colour = "black")) +
  theme(legend.background = element_rect(fill = alpha('gray', 0.05))) +
  theme(axis.text.x = element_text(size = rel(1.4)),
        axis.text.y = element_text(size = rel(1.4)),
        axis.line.x = element_line(size = rel(0.4),colour = 'black',linetype = 'solid'),
        axis.line.y = element_line(size = rel(0.4),colour = 'black',linetype = 'solid'),
        axis.title = element_text(size = rel(1.2), face = "bold")) +
  scale_linetype_manual(values = lty, guide = "none") +
  scale_colour_manual(values = cbbPalette, labels = plot.labels) +
  guides(colour=guide_legend(override.aes=list(linetype=1:length(plot.traceWeightFilter))))


################ plot match rate ###############
pmr <- ggplot(data = plot.data, aes(
  x = Timestamp,
  y = matchRateAvg,
  colour = TraceWeight,
  group = TraceWeight,
  linetype = TraceWeight)) +
  geom_line() +
  #geom_ribbon(aes(ymin = minmr, ymax = maxmr, fill = TraceWeight), alpha = 0.2) +
  labs(x = 'Number of Learning Problems\n(b)', y = NULL) +
  ggtitle("% OP") +
  theme(axis.title.y = element_text(size = rel(1.1), face = "bold"), axis.title.x = element_text(size = rel(1.1), face = "bold"), title = element_text(size = rel(1.1), face = 'bold')) +
  theme(legend.text = element_text(size = rel(1), face = "bold")) +
  theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.63, 0.15))
  theme(legend.position = 'bottom') + theme(panel.grid.major = element_line(size = 0.01, linetype = 'dotted',
                                                                            colour = "black"),
                                            panel.grid.minor = element_line(size = 0.001, linetype = 'dotted',
                                                                            colour = "black")) +
  theme(legend.background = element_rect(fill = alpha('gray', 0.05))) +
  theme(axis.text.x = element_text(size = rel(1.4)),
        axis.text.y = element_text(size = rel(1.4)),
        axis.line.x = element_line(size = rel(0.4),colour = 'black',linetype = 'solid'),
        axis.line.y = element_line(size = rel(0.4),colour = 'black',linetype = 'solid'),
        axis.title = element_text(size = rel(1.2), face = "bold")) +
  scale_linetype_manual(values = lty) +
  scale_colour_manual(values = cbbPalette)


################ plot arrange plots into one ###############
library(gridExtra)

g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(phv)

p3 <- grid.arrange(arrangeGrob(phv + theme(legend.position = "none"),
                               pmr + theme(legend.position = "none"),
                               nrow = 1),
                   mylegend, nrow = 2, heights = c(5, 1) )

p3


