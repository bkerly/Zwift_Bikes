library(tidyverse)
library(readxl)
library(tidyverse)

sheets <- excel_sheets("Zwift Speed Tests.xlsx") 

sheets <- sheets %>%
  data.frame() %>%
  filter(!(. %in% c("Sand & Sequoias","Bologna TT","Gaps",
                             "OLD Bologna TT","OLD Jungle Loop",
                             "OLD Richmond","Sprints (Richmond)",
                             "Watopia 2016","Watopia 2017","Watopia 2018")
           ) 
         )%>%
           unlist()

sheet_importer <- function(sheet){
  read_excel(path = "Zwift Speed Tests.xlsx",
             sheet = sheet
  ) %>%
    mutate(sheet_name = sheet) %>%
    return()
}

for(i in 1:length(sheets)){
 assign(paste0(sheets[i]),data.frame(sheet_importer(sheets[i])))
}


# Clean up each sheet -----------------------------------------------------

sheet_cleaner <- function(df){
  df %>%
  transmute(
    Weight = Weight,
    Watts = Watts,
    Bike = Bike,
    Wheels = Wheels,
    Time = as.interval(ymd_hms("1899-12-31 00:00:00"), ymd_hms(Time)) %>%
      as.duration(),
    Course = sheet_name) %>%
    return()
}


`Jungle Circuit` <- mutate(`Jungle Circuit`,Time = Full.Course)
`Big Foot Hills` <- mutate(`Big Foot Hills`,Time = Full.Lap)
`Everything Bagel` <- mutate(`Everything Bagel`,Time = Full.Lap)
`Mighty Metro` <- mutate(`Mighty Metro`,Time = Full.Lap)
`Richmond` <- mutate(`Richmond`,Time = Full.Lap)
`Volcano Climb` <- mutate(`Volcano Climb`,Time = Full.Lap)
`Watopia Hilly` <- mutate(`Watopia Hilly`,Time = Full.Lap)
`Watopia Hilly Reverse` <- mutate(`Watopia Hilly Reverse`,Time = Full.Lap)
`Casse-Pattes` <- mutate(`Casse-Pattes`,Time = Full.Lap)



output <- data.frame()
for(i in 1:length(sheets)){
  output<-get(sheets[i]) %>%
    sheet_cleaner() %>%
    bind_rows(output)
  
  print(paste(i, sheets[i]))
}

model1_data <- output %>%
  filter(!str_detect(Bike,pattern = "OLD ")) %>%
  filter(!str_detect(Wheels,pattern = "OLD ")) %>%
  mutate(Bike = as.factor(Bike)) %>%
  mutate(Wheels = as.factor(Wheels)) %>%
  mutate(Course = as.factor(Course)) %>%
  mutate(WpKg = Watts / Weight)


# Regressions! ------------------------------------------------------------

model1 <- lm(data = model1_data,
             formula = Time ~ 
               Bike + 
               Wheels + 
               Course + 
               Weight + 
               WpKg)

summary(model1)


# Need to standardize! ----------------------------------------------------

output %>% count(Course) %>% arrange(-desc(n))

std_model_data <- output %>%
  mutate(std_weight = (Weight == 75),
         std_watts = (Watts == 300),
         std_bike = (Bike == "Zwift Aero"),
         std_wheels = (Wheels == "32mm Carbon"),
         std_course_flat = (Course == "Tempus Fugit"),
         std_course_climb  = (Course == "Alpe")
         )

std_bike_time_flat <- std_model_data %>%
  filter(std_weight & std_watts  & std_wheels & std_course_flat) %>%
  group_by(Bike) %>%
  summarize(
    bike_flat_time = mean(Time)
  ) %>%
  ungroup()

std_bike_time_climb <- std_model_data %>%
  filter(std_weight & std_watts  & std_wheels & std_course_climb) %>%
  group_by(Bike) %>%
  summarize(
    bike_climb_time = mean(Time)
  ) %>%
  ungroup()

std_wheel_time_flat <- std_model_data %>%
  filter(std_weight & std_watts  & std_bike & std_course_flat) %>%
  group_by(Wheels) %>%
  summarize(
    wheel_flat_time = mean(Time)
  ) %>%
  ungroup()

std_wheel_time_climb <- std_model_data %>%
  filter(std_weight & std_watts  & std_bike & std_course_climb) %>%
  group_by(Wheels) %>%
  summarize(
    wheel_climb_time = mean(Time)
  ) %>%
  ungroup()

std_course_time <- std_model_data %>%
  group_by(Course) %>%
  summarize(
    course_time = mean(Time,na.rm=TRUE)
  ) %>%
  ungroup()

std_model_data <- std_model_data %>%
  left_join(std_bike_time_flat) %>%
  left_join(std_bike_time_climb) %>%
  left_join(std_wheel_time_flat) %>%
  left_join(std_wheel_time_climb) %>%
  left_join(std_course_time) %>%
  filter(std_weight,std_watts)


# Try another regression, maybe? ------------------------------------------

model2 <- lm(data = std_model_data,
             formula = Time ~ 
               bike_flat_time +
               bike_climb_time +
               wheel_flat_time+
               wheel_climb_time +
               course_time)

summary(model2)

# OK, just the flat and the climb time ------------------------------------

std_flat_data <- std_model_data %>%
  filter(std_course_flat)

flat_model <- lm(data = std_flat_data,
                 formula = Time ~ 
                   bike_flat_time +
                   bike_climb_time +
                   wheel_flat_time+
                   wheel_climb_time )

summary(flat_model)

flat_error_graph <- std_flat_data %>%
  filter(complete.cases(.)) %>%
  mutate(predicted_time = predict(newdata=.,flat_model)) %>%
  ggplot(aes(x=Time,y=predicted_time)) +
  geom_point()

flat_error_graph

std_climb_data <- std_model_data %>%
  filter(std_course_climb)

climb_model <- lm(data = std_climb_data,
                 formula = Time ~ 
                   bike_flat_time +
                   bike_climb_time +
                   wheel_flat_time+
                   wheel_climb_time )

summary(climb_model)

climb_error_graph <- std_climb_data %>%
  filter(complete.cases(.)) %>%
  mutate(predicted_time = predict(newdata=.,climb_model)) %>%
  ggplot(aes(x=Time,y=predicted_time)) +
  geom_point()

climb_error_graph


# Make all the predictions ------------------------------------------------

all_frame_wheels <- expand.grid(std_model_data$Bike %>% unique(),
                                std_model_data$Wheels %>% unique()) %>%
  data.frame()  %>%
  `colnames<-`(c("Bike","Wheels")) %>%
  left_join(std_bike_time_flat) %>%
  left_join(std_bike_time_climb) %>%
  left_join(std_wheel_time_flat) %>%
  left_join(std_wheel_time_climb) %>%
  left_join(`Tempus Fugit` %>%
              select(Bike,Type),
            by = "Bike")

all_frame_wheels_predict <- all_frame_wheels %>%
  mutate(pred_flat = predict(newdata=.,flat_model),
         pred_climb = predict(newdata=.,climb_model)) %>%
  filter(complete.cases(.))

all_frame_wheels_predict %>%
  ggplot(aes(x=pred_flat,y=pred_climb,color = Type)) +
  geom_point()


# Make a Shiny Mini-App ---------------------------------------------------
# The idea is that you can filter on level and dollars, then select some wheels and frames and you get a graph of which is fastest.
# And maybe we can try to implement it on different courses?

#https://stackoverflow.com/questions/42122414/constraining-a-shiny-app-input-based-on-another-input

#https://shiny.rstudio.com/reference/shiny/1.5.0/checkboxgroupinput

library(shiny)

ui <- fluidPage(
  sidebarPanel(
  selectInput(inputId = "frametype", label = "Frame Type",choices = c("Any",all_frame_wheels_predict$Type %>% unique())),
  checkboxGroupInput(inputId = "frame", label = "Frame"),
  checkboxGroupInput(inputId = "wheel", label = "Wheel", choices = all_frame_wheels_predict$Wheels %>% unique() %>% sort(),
                     selected = (all_frame_wheels_predict$Wheels %>% unique())[1:4])
  ),
  mainPanel(
    plotOutput("Plot")
  )
)

# Notice the session argument to be passed to updateSliderInput
server <- function(input, output, session) {
  observe({
    frame_options <- all_frame_wheels_predict %>%
      select(Bike,Type) %>%
      filter(if(input$frametype == "Any") TRUE else
        Type == input$frametype) %>%
      select(Bike) %>%
      unlist() %>%
      as.character() %>%
      unique() %>%
      sort()
    
    updateCheckboxGroupInput(session,
                             "frame",
                             choices = frame_options,
                             selected = frame_options[1:4])
    
      
  })
  
  output$Plot <- renderPlot({
    
    all_frame_wheels_predict %>%
       filter(Bike %in% input$frame,
              Wheels %in% input$wheel) %>%
      ggplot(aes(x=pred_flat,y=pred_climb,color = Bike)) +
      geom_point(aes(shape = Wheels),size = 3)+
      xlab("Climb Time")+
      ylab("Flat Time")
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

