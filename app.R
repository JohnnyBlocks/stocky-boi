# Header ------------------------------------------------------------------
#
# Stocky_Boi  - A bot to watch Amazon Stock and alert you 
#
# Credits: https://github.com/ebeneditos/telegram.bot
# 
#


# Imports -----------------------------------------------------------------

library(telegram.bot)
library(xml2)
library(rvest)
library(stringr)
library(readr)
library(dplyr)
library(shiny)
library(shinythemes)
library(shinycssloaders)


# Declarations ------------------------------------------------------------

pageTitle   <- "Stocky Boi - Amazon Telegram Bot"
pageAbrev   <- "Stocky Boi"


# UI ----------------------------------------------------------------------

ui <- shinyUI(fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 8px solid #8e8e8;}")),
    ),
    theme = shinytheme('yeti'),
    titlePanel = pageAbrev,
    title = pageAbrev,
    
    tags$style(
        type='text/css', ".control-label { font-size: 16px;}  .selectize-input { font-weight: bolder; }"
    )
))



# Server ------------------------------------------------------------------

server <- function(input, output) {

    # Updater Object ----------------------------------------------------------
    
    updater <- Updater(token = bot_token("Stocky_Boi"))
    
    
    
    
    # Command: Hello ----------------------------------------------------------
    
    hello <- function(bot, update){
        bot$sendMessage(chat_id = update$message$chat_id,
                        text = sprintf("Hello %s!", update$message$from$first_name))
    }
    hello_handler <- CommandHandler("hello", hello)
    updater <- updater + hello_handler
    
    
    
    # Main Program ------------------------------------------------------------
    
    bot$sendMessage(chat_id =  Sys.getenv("R_TELEGRAM_ME"),
                    text = "I'm a Start!")
    
    #rds database check here
    
    
    updater$start_polling()
    
    bot$sendMessage(chat_id =  Sys.getenv("R_TELEGRAM_ME"),
                    text = "I died!")
    
    
    
    
    
    
    
    
    
    
    create_bot_data <- function(){
        bot_data <- data.frame(
            "update_id" = 1:2,
            "message_id" = c(1,1),
            
            "user_name" =c("YoYoPete","YoYoPete"),
            "date_added" = c(Sys.time(),Sys.time()),
            "product_url" = c('https://www.amazon.com/dp/B08HB9TCVG',
                              'https://www.amazon.com/Mario-Kart-Live-Circuit-Mario-Nintendo/dp/B08H9KGMWK'),
            "product_name" = c('Mario Game & Watch','Mario Kart Live Circuit Mario Edition'),
            "last_status" = c('a','a')
            ,stringsAsFactors = TRUE)
        bot_data$product_url <- lapply(bot_data$product_url, as.character)
        bot_data <- bot_data[-c(1,2),]
        write_rds(bot_data,'botdata.rds')
    }
    
    check_stock <- function(items_names,items_urls){
        for (i in 1:length(items_urls)){
            url <- items_urls[i]
            stock <- read_html(url) %>%
                html_nodes('div#availability') %>%
                html_text() %>%
                str_replace_all("[\n]" , "") %>%
                str_replace("We don't know when or if this item will be back in stock.","")
            alert <- str_c(items_names[i],stock,sep=" - ") %>%
                str_c(items_urls[i],sep="\n")
            cat(alert)
            if(grepl("In Stock", stock, fixed=TRUE) == TRUE || grepl("Pre-order now.", stock, fixed=TRUE) == TRUE){
                bot$sendMessage(alert)
            }
        }
    }
    
    item_list <- function(chat_id,user_id){
        #add @ username from user_id
        bot_data <- read_rds('botdata.rds')
        nrow(bot_data)
        if(nrow(bot_data)==0) {
            message <- "There are no products being watched...\nAdd one with the **/watch** command"
        } else {
            message <- 'ID - PRODUCT - LAST STATUS'
            for (i in 1:nrow(bot_data)){
                item = str_c('[',bot_data$product_name[i],']','(',bot_data$product_url[i],')')
                row <- str_c(
                    which(bot_data$update_id == bot_data$update_id[i]),
                    item,
                    bot_data$last_status[i],
                    sep=" - "
                )
                message <- str_c(message,row,sep='\n')
            }
        }
        bot$sendMessage(chat_id,message,parse_mode="Markdown",disable_web_page_preview=TRUE)
    }
    
    
    # if(file.exists('botdata.rds')==FALSE){
    #   create_bot_data()
    # }
    
    
    #bot <- Bot(token = Sys.getenv("R_TELEGRAM_BOT"))
    
    #This should be dynamic based on the chat ID of where it came from - add that to the bot_data
    #my_id <- Sys.getenv("R_TELEGRAM_ME")
    
    
    
    
    #bot_data <- read_rds('botdata.rds')
    #new_data <- bot$getUpdates()
    
    #print(new_data[1])
    
    #work <- anti_join(new_data, bot_data, by = c("update_id" = "update_id"),copy=TRUE)
    
    #for (i in 1:nrow(work)) {
    #  print(work[i])
    # message_command <- !is.null(unlist(work$message$entities[i]))
    # message_user <- work$message$from$username[i]
    # message_time <- as.POSIXct(work$message$date[i], origin="1970-01-01")
    # message_text <- work$message$text[i]
    #
    # if(!is.null(unlist(work$message$entities[i]))==TRUE){
    #  action <- toupper(substr(message_text,2,11))
    #  if(action=='LIST'){
    #    item_list()
    #  } else if (action=='WATCH HTTP'){
    #   # print('  ADD URL')
    #  } else if (action=='DELETE'){
    #   # print('  REMOVE ITEM')
    #  }
    # }
    #
    
    #ADD Item - Ask if public or private alert (allow to cancel) option 1-3
    
    
    #check_stock(items_names,items_urls)
    
    #item_list(my_id,NULL)
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
