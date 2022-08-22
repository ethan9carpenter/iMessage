library(shiny, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(DBI, warn.conflicts = F)
library(DT, warn.conflicts = F)
library(shinyWidgets, warn.conflicts = F)
options(shiny.maxRequestSize = 256*1024^2)

file.copy('~/Library/Messages/chat.db', 'chat.db', overwrite = T)
conn <- dbConnect(RSQLite::SQLite(), 'chat.db')

contacts <- read.csv('contacts.csv', check.names=F) %>%
  select(name=`Display Name`, `Home Phone`, `Mobile Phone`, `E-mail Address`) %>%
  pivot_longer(
    c(`Home Phone`, `Mobile Phone`, `E-mail Address`),
    names_to=NULL,
    values_to='phone'
  ) %>%
  filter(
    phone != ''
  ) %>%
  mutate(
    phone=ifelse(
      str_detect(phone, '@'),
      phone,
      phone %>%
        str_remove_all('[ ]|[)]|[(]|[-]|[+]') %>%
        paste0('+1', .) %>%
        str_replace('\\+11', '+1')
    )
  )

chat_handle_join <- dbGetQuery(conn, '
SELECT 
  chat_id, 
  handle_id 
FROM 
  chat_handle_join
')

group_chats <- chat_handle_join %>%
  count(chat_id) %>%
  filter(
    n > 1
  ) %>%
  select(chat_id) %>%
  mutate(
    is_gc=T
  )

handle <- dbGetQuery(conn, '
SELECT
  ROWID AS handle_id,
  id
FROM
  handle
') %>%
  add_row(handle_id=0, id='+14109922955')

messages <- dbGetQuery(conn, '
SELECT 
  text,
  handle_id,
  date,
  is_from_me,
  group_title,
  other_handle,
  chat_id
FROM 
  message LEFT JOIN chat_message_join ON message.ROWID = chat_message_join.message_id
WHERE
  account NOT LIKE "%5carpenter@comcast.net%"
  AND text NOT LIKE "%”%"
') %>%
  mutate(
    date=as.POSIXct(date/1e9, origin='2001-01-01'),
    is_from_me=as.logical(is_from_me)
  )

group_chat_dt <- dbGetQuery(
  conn,
  '
  SELECT
    text,
    message.date,
    chat_id,
    handle.id AS contact_id,
    handle_id
  FROM
    message 
    LEFT JOIN chat_message_join ON message.rowid = chat_message_join.message_id
    LEFT JOIN handle ON message.handle_id = handle.rowid
  WHERE
    account NOT LIKE "%5carpenter@comcast.net%"
  '
) %>%
  mutate(
    date=as.POSIXct(date/1e9, origin='2001-01-01'),
    n_words=map_int(str_split(text, ' '), length),
    contact_id=ifelse(handle_id == 0, '+14109922955', contact_id)
  ) %>%
  left_join(contacts, by=c('contact_id'='phone')) %>%
  mutate(
    name=ifelse(is.na(name), contact_id, name)
  )

dbDisconnect(conn)

gc_texts <- messages %>%
  filter(
    handle_id == 0
  ) %>%
  select(-handle_id) %>%
  left_join(chat_handle_join, by = c("chat_id"))

messages <- messages %>%
  filter(
    handle_id != 0
  ) %>%
  rbind(gc_texts) %>%
  left_join(handle, 'handle_id') %>%
  left_join(contacts, by=c('id'='phone')) %>%
  left_join(group_chats, by='chat_id') %>%
  mutate(
    is_gc=ifelse(is.na(is_gc), F, T)
  )

ui <- fluidPage(
  titlePanel("Explore your Texts!"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        'date_slider',
        'Dates',
        min=min(messages$date),
        max=max(messages$date),
        value = c(min(messages$date), max(messages$date)),
        timeFormat='%F'
      ),
      selectInput(
        'duration_input',
        label='Duration',
        choices=c(
          'Minute'='minute',
          'Hour'='hour',
          '2 Hours'='2 hours',
          'Day'='day',
          'Week'='week',
          'Month'='month',
          'Year'='year',
          'All-Time'='all-time'
        ),
        selected='day'
      ),
      switchInput(
        'group_chat_switch',
        'Include Group Chats?',
        onLabel='Yes',
        offLabel='No',
        value=T
      ),
      fileInput(
        "file_input", 
        "chat.db",
        multiple = F,
        accept = c('.db')
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Leaderboard',
          DT::dataTableOutput('leaderboard')
        ),
        tabPanel(
          'Group Chats',
          selectizeInput(
            'group_chat_people',
            label='Who?',
            choices=unique(group_chat_dt$name),
            multiple=T
          ),
          switchInput(
            'exclusive_group_chat_switch',
            'Exclusive?',
            onLabel='Yes',
            offLabel='No',
            value=F
          ),
          plotOutput('group_chat_plot')
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$leaderboard <- DT::renderDataTable({
    dt <- messages %>%
      filter(
        date >= input$date_slider[1],
        date <= input$date_slider[2],
        ! (!input$group_chat_switch & is_gc)
      ) 
    
    if (input$duration_input == 'all-time'){
      dt <- dt %>%
        mutate(
          date=max(date)
        )
    } else {
      dt <- dt %>%
        mutate(
          date=lubridate::floor_date(date, input$duration_input)
        ) 
    }
    
    dt %>%
      count(is_from_me, name, date, id, handle_id) %>%
      pivot_wider(
        names_from = is_from_me,
        values_from=n
      ) %>%
      mutate(
        name=ifelse(is.na(name), id, name)
      ) %>%
      select(name, date, sent=`TRUE`, received=`FALSE`) %>%
      mutate(
        across(c(sent, received), ~ifelse(is.na(.x), 0, .x)),
        total=sent+received,
        ratio=sent/received
      ) %>%
      arrange(-total) %>%
      DT::datatable(
        
      ) %>%
      formatRound(
        c('sent', 'received', 'total'),
        0
      ) %>%
      formatRound(
        c('ratio'),
        2
      ) %>%
      formatDate(
        'date'
      )
  })
  
  output$group_chat_plot <- renderPlot({
    group_chat_dt %>%
      group_by(chat_id) %>%
      filter(
        date >= input$date_slider[1],
        date <= input$date_slider[2],
        all(input$group_chat_people %in% name),
        !input$exclusive_group_chat_switch | all(name %in% input$group_chat_people)
      ) %>%
      ungroup() %>%
      group_by(name) %>%
      summarize(
        avg_words=mean(n_words),
        n=n(),
        .groups='drop'
      ) %>%
      ggplot(aes(reorder(name, -n), n)) +
      geom_bar(stat='identity') +
      theme(
        axis.text.x = element_text(angle=-45)
      ) +
      labs(
        x='Name',
        y='Number of Messages',
        caption='Excluding reactions'
      )
  })
}

shinyApp(ui = ui, server = server)
