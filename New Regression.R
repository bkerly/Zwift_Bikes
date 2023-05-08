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

tron_flat <- std_model_data %>%
  filter(Bike == "Tron (Concept Z1)",
         Course == "Tempus Fugit") %>%
  group_by(Bike) %>%
  summarize(bike_flat_time = mean(Time),
            wheel_flat_time = mean(Time))

tron_climb <- std_model_data %>%
  filter(Wheels == "Tron",
         Course == "Alpe") %>%
  group_by(Wheels) %>%
  summarize(bike_climb_time = mean(Time,na.rm = TRUE),
            wheel_climb_time = mean(Time,na.rm = TRUE))

tron_data <- bind_cols(tron_flat,tron_climb)

rm(tron_flat,tron_climb)

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
  filter(complete.cases(.)) %>%
# Add in Tron
  bind_rows(tron_data %>%
              mutate(pred_flat =  bike_flat_time,
                     pred_climb=  bike_climb_time,
                     Type = "Standard") 
  )

all_frame_wheels_predict %>%
  ggplot(aes(x=pred_flat,y=pred_climb,color = Type)) +
  geom_point()


# Add Additional Routes ---------------------------------------------------


library(randomForest)

course_model <- randomForest(formula = (Time - course_time) ~ 
                               Course + 
                               course_time + 
                               bike_flat_time +
                               bike_climb_time +
                               wheel_flat_time +
                               wheel_climb_time ,
                             data = std_model_data %>%
                               ungroup() %>%
                               filter(complete.cases(.)),
                             ntree = 10000
    )

summary(course_model)

predict(course_model)

varImpPlot(course_model)
rf_predictions <- std_model_data %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  mutate(rf_pred = predict(course_model)) 

g <- ggplot(rf_predictions, aes(x=(Time - course_time),y=rf_pred,color = Course,size=course_time)) +
  geom_point()+
  geom_abline() +
  xlim(-100,100) + 
  ylim(-100,100) 
  plotly::ggplotly(g)
  

# Maybe separate models for each course? ----------------------------------


  
course_models <- std_model_data %>%
    filter(Time < 50000) %>%
  filter(complete.cases(.)) %>%
    group_by(Course) %>%
    reframe(course_model = broom::tidy(lm(formula = Time ~ 
                                  bike_flat_time +
                                  bike_climb_time +
                                  wheel_flat_time +
                                  wheel_climb_time))) %>%
    ungroup() %>%
    mutate(term = course_model$term,
           estimate = course_model$estimate) %>%
    select(-course_model) %>%
    pivot_wider(names_from = term, values_from =estimate, names_prefix = "pred.") %>% 
    rename(pred.intercept = `pred.(Intercept)`) %>%
    mutate(across(everything(),~replace_na(.,0)))

course_model_predict <- std_model_data %>%
  filter(Time < 50000) %>%
  filter(complete.cases(.)) %>%
  left_join(course_models, by = "Course") %>%
  mutate(prediction = pred.intercept + 
           (bike_flat_time * pred.bike_flat_time) + 
           (bike_climb_time * pred.bike_climb_time) +
           (wheel_flat_time * pred.wheel_flat_time) +
           (wheel_climb_time * pred.wheel_climb_time)) %>%
  mutate(error = sqrt((Time - prediction)^2))

# See how it performed
ggplot(course_model_predict) +
  geom_point(aes(x=Time,y=error,color = Course))

# Now do it for all of the combinatons!
all_frame_wheels_course_predict <- expand.grid(std_model_data$Bike %>% unique(),
                                           std_model_data$Wheels %>% unique(),
                                           std_model_data$Course %>% unique()) %>%
  data.frame()  %>%
  `colnames<-`(c("Bike","Wheels","Course")) %>%
  left_join(std_bike_time_flat) %>%
  left_join(std_bike_time_climb) %>%
  left_join(std_wheel_time_flat) %>%
  left_join(std_wheel_time_climb) %>%
  left_join(`Tempus Fugit` %>%
              select(Bike,Type),
            by = "Bike")

all_frame_wheels_course_predict <- all_frame_wheels_course_predict %>%
  left_join(course_models, by = "Course") %>%
  filter(complete.cases(.)) %>%
  mutate(prediction = pred.intercept + 
           (bike_flat_time * pred.bike_flat_time) + 
           (bike_climb_time * pred.bike_climb_time) +
           (wheel_flat_time * pred.wheel_flat_time) +
           (wheel_climb_time * pred.wheel_climb_time)
         )


  
  
# Make a Shiny Mini-App ---------------------------------------------------
# The idea is that you can filter on level and dollars, then select some wheels and frames and you get a graph of which is fastest.
# And maybe we can try to implement it on different courses?

#https://stackoverflow.com/questions/42122414/constraining-a-shiny-app-input-based-on-another-input

#https://shiny.rstudio.com/reference/shiny/1.5.0/checkboxgroupinput

library(shiny)

ui <- fluidPage(
  sidebarPanel(
  selectInput(inputId = "frametype", label = "Frame Type",choices = c("Any",all_frame_wheels_course_predict$Type %>% unique())),
  selectInput(inputId = "course", label = "Course",choices = c(all_frame_wheels_course_predict$Course %>% unique()),
              selected = "Surrey Hills"),
  checkboxGroupInput(inputId = "frame", label = "Frame"),
  checkboxGroupInput(inputId = "wheel", label = "Wheel", choices = all_frame_wheels_course_predict$Wheels %>% unique() %>% sort(),
                     selected = (all_frame_wheels_predict$Wheels %>% unique())[1:4])
  ),
  
  mainPanel(
    plotOutput("Plot"),
    fluidRow(
      column(width = 6,
    plotOutput("Plot2")),
    column(width = 6,plotOutput("Plot3"))
    )
  )
)

# Notice the session argument to be passed to updateSliderInput
server <- function(input, output, session) {
  observe({
    frame_options <- all_frame_wheels_course_predict %>%
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
    
    all_frame_wheels_course_predict %>%
       filter(Bike %in% input$frame,
              Wheels %in% input$wheel,
              Course %in% input$course) %>%
      ggplot(aes(x=Bike,y=prediction,color = Wheels)) +
      geom_point(aes(shape = Wheels),size = 6)+
      theme_bw() +
      xlab("Bike")+
      ylab("Predicted Time (s)")+
      labs(title = "Predicted Course Time for Selected Frame and Wheel Combiantions")+
      scale_color_viridis_d()
    
  })
  
  output$Plot2 <- renderPlot({
    
    all_frame_wheels_course_predict %>%
      filter(Course %in% input$course) %>%
      select(-pred.intercept) %>%
      rename("Wheel Aero" = pred.wheel_flat_time,
             "Wheel Weight" = pred.wheel_climb_time,
             "Bike Aero" = pred.bike_flat_time,
             "Bike Weight" = pred.bike_climb_time) %>%
      pivot_longer(cols = (`Bike Aero`:`Wheel Weight`), names_to = "variable", values_to = "value") %>%
      select(Course,variable,value) %>%
      unique() %>%
      ggplot(aes(x=variable,y=value,fill = variable)) +
      geom_bar(stat="identity")  +
      coord_flip() +
      xlab("") +
      ylab("Variable Coefficients")+
      theme_bw() +
      scale_fill_grey()+
      theme(legend.position = "none")+
      labs(title = "Model Coefficients",
           subtitle = "Greater positive coefficients imply greater effect of each factor")
    
  })
  
  output$Plot3 <- renderPlot({
    
    std_model_data %>%
      filter(complete.cases(.)) %>%
      count(Course) %>%
      mutate(selected = (Course %in% input$course)) %>%
      arrange(n) %>%
      ggplot(aes(x=Course,y=n,fill = selected)) +
      geom_bar(stat="identity")  +
      coord_flip() +
      xlab("") +
      ylab("Number of Observations")+
      theme_bw() +
      scale_fill_manual(values = c("black","red"))+
      theme(legend.position = "none") +
      labs(title = "Number of Observatons by Course",
            subtitle  = "More observations increases prediction certainty")
      

    
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

