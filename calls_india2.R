# Packages
library(RColorBrewer)
library(ggridges)
library(cowplot)
library(ggplot2)
library(mapview)
library(sp)
library(e1071)
library(caret)
library(randomForest)
library(naivebayes)

# Loading dataset

calls <- read.csv("MyCall_Data_December_2020.csv", header = TRUE, sep = ",")

str(calls)

#### Descirptive analysis ####

# Unique data Rating

c(unique(calls["Rating"]))

# Unique data Call drop category

c(unique(calls["Call.Drop.Category"]))

# Unique data State Name

c(unique(calls["State.Name"]))

# Histogram of ratings

tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = calls, mapping = aes(x = Rating)) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Rating") +
  ggtitle("Rating of the calls") +
  tema

a

# Frequency of the states

freq_states <- data.frame(cbind(Frequency = table(calls$State.Name), Percent = prop.table(table(calls$State.Name)) * 100))
freq_states
str(freq_states)

# Histogram of number of calls by state

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
a <- ggplot(data = freq_states, mapping = aes(x = Frequency, y = row.names(freq_states))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_states), color = row.names(freq_states)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Number of calls by state") +
  tema
a

# Frequency of network

freq_network <- data.frame(cbind(Frequency = table(calls$Network.Type), Percent = prop.table(table(calls$Network.Type)) * 100))
freq_network
str(freq_network)

# Histogram of most used networks in the calls

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
b <- ggplot(data = freq_network, mapping = aes(x = Frequency, y = row.names(freq_network))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_network), color = row.names(freq_network)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Most used networks in the calls") +
  tema
b

# Frequency of traveling in the calls

freq_travelling <- data.frame(cbind(Frequency = table(calls$In.Out.Travelling),
                                    Percent = prop.table(table(calls$In.Out.Travelling)) * 100))
freq_travelling

# Histogram of travelling

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
c <- ggplot(data = freq_travelling, mapping = aes(x = Frequency, y = row.names(freq_travelling))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_travelling), color = row.names(freq_travelling)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Indoor or outdoor when calling") +
  tema
c

# Frequency of call drop category

freq_category <- data.frame(cbind(Frequency = table(calls$Call.Drop.Category),
                                    Percent = prop.table(table(calls$Call.Drop.Category)) * 100))
freq_category

# Histogram of calls category

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
d <- ggplot(data = freq_category, mapping = aes(x = Frequency, y = row.names(freq_category))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_category), color = row.names(freq_category)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Category of the quality of the calls") +
  tema
d

plot_grid(a, b, c, d, ncol=2, nrow=2)

#### Exlporatory analysis ####

# Latitude and longitude data
calls_map <- as.data.frame(calls)

coordinates(calls_map) <- ~ Longitude + Latitude
proj4string(calls_map) <- "+init=epsg:4326"

mapview(calls_map, zcol = "Operator", burst = TRUE)

#### Pre processing ####

# Removing NA values 

calls <- na.omit(calls)

# Transforming Rating variable into factor

calls$Rating <- factor(calls$Rating)

# Dividing the data into training and test

indexes <- sample(1:nrow(calls), size = 0.7 * nrow(calls))
train.data.calls <- calls[indexes,]
test.data.calls <- calls[-indexes,]
class(train.data.calls)
class(test.data.calls)

str(train.data.calls)

prop.table(table(train.data.calls$Rating)) * 100

#### Machine learning ####

# Machine learning svm model 89% accuracy
ma_model_calls <- svm(Rating ~. ,data = train.data.calls)
summary(ma_model_calls)
print(ma_model_calls)

# Predictions of model

pred_model_calls <- predict(ma_model_calls, test.data.calls)



table(pred_model_calls, test.data.calls$Rating)

confusionMatrix(pred_model_calls, test.data.calls$Rating)

# Naive bayes model 84% accuracy

ma_model_calls <- naiveBayes(Rating~., train.data.calls)


# Predictions of model

pred_model_calls2 <- predict(ma_model_calls, test.data.calls)



table(pred_model_calls2, test.data.calls$Rating)

confusionMatrix(pred_model_calls2, test.data.calls$Rating)

# predictive rating model plot

pred_model_calls_plot <- as.data.frame(pred_model_calls)
names(pred_model_calls_plot) <- c("Rating")

names(pred_model_calls_plot)


tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
e <- ggplot(data = pred_model_calls_plot , mapping = aes(x = as.numeric(Rating))) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Rating") +
  ggtitle("Rating preditive model") +
  tema

e

# Real rating data plot

options(repr.plot.width=14, repr.plot.height=6)
f <- ggplot(data = test.data.calls , mapping = aes(x = as.numeric(Rating))) +
  geom_histogram(fill = "red", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Rating") +
  ggtitle("Rating real data") +
  tema

f

plot_grid(e, f, ncol = 2, nrow = 1)