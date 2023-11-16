my_data <- read.csv(file.choose())
my_data <- PlantGrowth

#sample_data
set.seed(1234)
dplyr::sample_n(my_data, 10)

levels(my_data$group)
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

#Box_plots
#++++++++++++++++++++
#Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

#Mean_plots
#++++++++++++++++++++
#Plot weight by group
#Add error bars: mean_se
#(other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

#Box_plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
#plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI")

#Analysis_of_variance
res.aov <- aov(weight ~ group, data = my_data)
#Summary_analysis
summary(res.aov)

print("As the p-value is less than the significance level 0.05,
we can conclude that there are significant differences between
the groups highlighted with '*' in the model summary.")

print("Null hypothesis: There is no significant difference on weight
      between treatment groups.")
