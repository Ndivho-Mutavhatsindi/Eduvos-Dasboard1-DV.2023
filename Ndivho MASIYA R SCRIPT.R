# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)

# Load dataset
df <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)

# Selecting relevant columns as per the scenario
gradsurv <- df %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform, WebFramework, Industry, AISearch, AITool, Employment)

# Convert blank values to NA
gradsurv[gradsurv == ""] <- NA

# Handling missing values - Replace NA with "Unknown" for categorical variables
gradsurv <- gradsurv %>%
  mutate(across(where(is.character), ~replace_na(., "Unknown")))

# Standardizing campus names (merging similar campuses)
gradsurv <- gradsurv %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban Campus", "Umhlanga Campus") ~ "Umhlanga Campus",
    Campus %in% c("Mbombela Campus", "Nelspruit Campus") ~ "Mbombela Campus",
    Campus %in% c("Port Elizabeth Campus", "Nelson Mandela Bay Campus") ~ "Port Elizabeth Campus",
    TRUE ~ Campus
  ))

# Subsetting the top 5 campuses with the most responses
top_campuses <- gradsurv %>%
  count(Campus, sort = TRUE) %>%
  slice(1:5) %>%
  pull(Campus)

gradsurv <- gradsurv %>%
  filter(Campus %in% top_campuses)

# Generating descriptive statistics
summary_stats <- gradsurv %>%
  summarise(
    total_responses = n(),
    unique_study_fields = n_distinct(StudyField),
    unique_roles = n_distinct(Role),
    unique_industries = n_distinct(Industry),
    employed_count = sum(Employment != "Unemployed", na.rm = TRUE),
    employment_rate = (employed_count / total_responses) * 100
  )

print("Summary Statistics:")
print(summary_stats)

# Counting the most-used programming languages
top_proglang <- gradsurv %>%
  separate_rows(ProgLang, sep = ";") %>%
  count(ProgLang, sort = TRUE)

print("Top Programming Languages:")
print(head(top_proglang, 5))

# Counting the most-used databases
top_databases <- gradsurv %>%
  separate_rows(Databases, sep = ";") %>%
  count(Databases, sort = TRUE)

print("Top Databases:")
print(head(top_databases, 5))

# Counting the most-used web frameworks
top_webframeworks <- gradsurv %>%
  separate_rows(WebFramework, sep = ";") %>%
  count(WebFramework, sort = TRUE)

print("Top Web Frameworks:")
print(head(top_webframeworks, 5))

# Save the cleaned dataset
write.csv(gradsurv, "cleaned_graduate_survey.csv", row.names = FALSE)


# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Load dataset
df <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)

# Selecting relevant columns
gradsurv <- df %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform, WebFramework, Industry, AISearch, AITool, Employment)

# Convert blank values to NA
gradsurv[gradsurv == ""] <- NA

# Handling missing values - Replace NA with "Unknown" for categorical variables
gradsurv <- gradsurv %>%
  mutate(across(where(is.character), ~replace_na(., "Unknown")))

# Standardizing campus names (merging similar campuses)
gradsurv <- gradsurv %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban Campus", "Umhlanga Campus") ~ "Umhlanga Campus",
    Campus %in% c("Mbombela Campus", "Nelspruit Campus") ~ "Mbombela Campus",
    Campus %in% c("Port Elizabeth Campus", "Nelson Mandela Bay Campus") ~ "Port Elizabeth Campus",
    TRUE ~ Campus
  ))

# Subsetting the top 5 campuses with the most responses
top_campuses <- gradsurv %>%
  count(Campus, sort = TRUE) %>%
  slice(1:5) %>%
  pull(Campus)

gradsurv <- gradsurv %>%
  filter(Campus %in% top_campuses)

# Function to split and count occurrences for a given column
count_top_tools <- function(df, column_name) {
  df %>%
    separate_rows(!!sym(column_name), sep = ";") %>%
    count(!!sym(column_name), sort = TRUE)
}

# Apply function to relevant columns
top_proglang <- count_top_tools(gradsurv, "ProgLang")
top_databases <- count_top_tools(gradsurv, "Databases")
top_platforms <- count_top_tools(gradsurv, "Platform")
top_webframeworks <- count_top_tools(gradsurv, "WebFramework")
top_aisearch <- count_top_tools(gradsurv, "AISearch")
top_aitools <- count_top_tools(gradsurv, "AITool")

# Display results
print("Top Programming Languages:")
print(top_proglang)
print("Top Databases:")
print(top_databases)
print("Top Platforms:")
print(top_platforms)
print("Top Web Frameworks:")
print(top_webframeworks)
print("Top AI Search Tools:")
print(top_aisearch)
print("Top AI Tools:")
print(top_aitools)

# **I. Visualization - Top Tools Used by Graduates**
ggplot(top_proglang[1:10,], aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Programming Languages Used by Graduates", x = "Programming Language", y = "Count") +
  theme_minimal()

# **II. Most Popular Industries by Study Field**
industry_distribution <- gradsurv %>%
  separate_rows(Industry, sep = ";") %>%
  count(StudyField, Industry, sort = TRUE)

ggplot(industry_distribution, aes(x = Industry, y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top Industries by Study Field", x = "Industry", y = "Number of Graduates")

# **III. Top Job Roles by Study Field**
role_distribution <- gradsurv %>%
  count(StudyField, Role, sort = TRUE)

print("Top Job Roles by Study Field:")
print(role_distribution)

ggplot(role_distribution, aes(x = Role, y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Number of Graduates")

# **IV. Employment Rate by Study Field**
employment_rate <- gradsurv %>%
  group_by(StudyField) %>%
  summarise(
    Total_Graduates = n(),
    Employed = sum(Employment %in% c("Employed, full-time", "Employed, part-time", 
                                     "Self-employed", "Independent contractor, freelancer", 
                                     "Entrepreneur"), na.rm = TRUE),
    Employment_Rate = (Employed / Total_Graduates) * 100
  )

# Plot the employment rate
ggplot(employment_rate, aes(x = StudyField, y = Employment_Rate, fill = StudyField)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)")



library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(plotly)

# Load Data
df <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)

# Convert column names to lowercase
colnames(df) <- tolower(colnames(df))  

df <- df %>%
  rename(
    employment_status = employment,
    salary = convertedcompyearly,
    industry = industry,
    campus = campus,
    study_field = studyfield,
    education_level = edulevel,
    programming_languages = proglang,
    databases = databases,
    platform = platform,
    web_framework = webframework,
    ai_search = aisearch,
    ai_tool = aitool
  )

df[is.na(df)] <- "Unknown"  # Handle missing values

# Filter relevant columns
gradsurv <- df %>%
  select(campus, study_field, branch, role, education_level, programming_languages, databases, platform, web_framework, industry, ai_search, ai_tool, employment_status, salary, age)

# Standardize Campus Names
gradsurv <- gradsurv %>% mutate(campus = case_when(
  campus %in% c("Durban Campus", "Umhlanga Campus") ~ "Umhlanga Campus",
  campus %in% c("Mbombela Campus", "Nelspruit Campus") ~ "Mbombela Campus",
  campus %in% c("Port Elizabeth Campus", "Nelson Mandela Bay Campus") ~ "Port Elizabeth Campus",
  TRUE ~ campus
))

# Select top 5 campuses
top_campuses <- gradsurv %>%
  count(campus, sort = TRUE) %>%
  slice(1:5) %>%
  pull(campus)

gradsurv <- gradsurv %>% filter(campus %in% top_campuses)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Graduate Survey Dashboard"),
  dashboardSidebar(
    selectInput("study_field", "Select Study Field:", choices = c("All", unique(df$study_field)), selected = "All"),
    selectInput("campus", "Select Campus:", choices = c("All", unique(df$campus)), selected = "All"),
    selectInput("tech_tool", "Select Tech Tool:", choices = c("All", "Programming Languages", "Databases", "Web Frameworks", "AI Tools"), selected = "All")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Demographics", fluidRow(
        box(plotOutput("ageDist"), width = 6),
        box(plotOutput("studyFieldDist"), width = 6),
        box(plotOutput("campusDist"), width = 6)
      )),
      tabPanel("Employment", fluidRow(
        box(plotOutput("employmentStatus"), width = 6),
        box(plotOutput("salaryDist"), width = 6),
        box(plotOutput("salaryByField"), width = 6)
      )),
      tabPanel("Tech Tools", fluidRow(
        box(plotlyOutput("techToolDist"), width = 6)
      )),
      tabPanel("Industry", fluidRow(
        box(plotOutput("industryDist"), width = 6)
      )),
      tabPanel("Data Table", fluidRow(
        box(DTOutput("dataTable"), width = 12)
      ))
    )
  )
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    data <- gradsurv
    if (input$study_field != "All") {
      data <- data[data$study_field == input$study_field, ]
    }
    if (input$campus != "All") {
      data <- data[data$campus == input$campus, ]
    }
    data
  })
  
  output$techToolDist <- renderPlotly({
    data <- filteredData()
    tool_column <- switch(input$tech_tool,
                          "Programming Languages" = "programming_languages",
                          "Databases" = "databases",
                          "Web Frameworks" = "web_framework",
                          "AI Tools" = "ai_tool",
                          NULL)
    
    if (!is.null(tool_column)) {
      plot_data <- data %>%
        count(!!sym(tool_column), sort = TRUE) %>%
        top_n(10)
      
      p <- ggplot(plot_data, aes(x = reorder(!!sym(tool_column), n), y = n, fill = !!sym(tool_column))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Top 10", input$tech_tool), x = input$tech_tool, y = "Count")
      
      ggplotly(p)
    }
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData())
  })
}

# Run App
shinyApp(ui = ui, server = server)



