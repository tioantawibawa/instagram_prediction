library(shiny)
library(shinydashboard)
library(instaloadeR)
library(reticulate)
library(lubridate)
library(stringr)
library(scorecard)
library(textclean)
library(tokenizers)
library(katadasaR)
library(tm)
library(e1071)
library(jsonlite)
library(stringr)
library("jpeg")
library(tidyr)
library(utf8)
library(data.table)
source("helper/instagram.R")
# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(
        title = strong("Instagram Posting Prediction")
        
    ),
    dashboardSidebar(
        sidebarMenu(
            tags$br(),
            #     menuItem("Overview", tabName = "overview", icon = icon("compass")),
            menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tabItems(
            #  tabItem("overview",
            #          box(width = 12,
            #              fileInput("raw", "Upload File:",
            #                        accept = c(
            #                          "text/csv",
            #                          "text/comma-separated-values,text/plain",
            #                          ".csv")),
            #              tableOutput("table")
            #              )),
            tabItem("analysis",
                    htmlOutput("banner"),
                    textInput("text1", label = "Input Instagram's Account:", value = "Text Here"),
                    textInput("text2", label = "Input Your Caption:", value = "Text Here",width = '75%'),
                    textInput("text3", label = "Input Your Posting Time:", value = "Hour Only (24h format, ex:4,5,19,20)"),
                    textInput("text4", label = "Input Your Posting Date:", value = "Date Only (YYYY-MM-DD)"),
                    textInput("text5", label = "Input Your User (tag):", value = "Users that you want to tag"),
                    selectInput("variable", "Select Your Posting Type:",
                                c("Picture" = "Picture",
                                  "Video" = "Video")),
                    actionButton("addrow", "Process"),
                    fluidRow(
                    h4(""),
                    htmlOutput("banner1"),
                    h4(""),
                    htmlOutput("hasil"))
            )
        ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values <- reactiveValues(addrow=FALSE)
    newEntry <- observeEvent(input$addrow, {
    values$addrow=TRUE
    userinstra <- as.character(input$text1)
    url_awal <- str_glue("http://www.instagram.com/{userinstra}/?__a=1")
      show_condition <- function(code) {
        tryCatch(code,
                 error = function(c) "error",
                 warning = function(c) "warning",
                 message = function(c) "message"
        )
      }
      json_awal <- show_condition(fromJSON(url_awal))
    
      df_instagram <- instagram(input$text1)
      
      df_instagram <- as.data.frame(df_instagram)  
      
   if( json_awal[1]=="error" | json_awal[1]=="warning") { 
        output$hasil <- renderText({
            print("User tidak ditemukan / user di-private")
        }) }
    else if(is.na(as.numeric(input$text3))==TRUE | is.na(ymd(input$text4))==TRUE ) {
        output$hasil <- renderText({
            print("Tolong Periksa Inputan")
        })  
    }
    else {
        
        
        use_python(py_config()$python)
        
        install_instaloadeR()
        
        init_instaloadeR()
        
        tioantawibawa <- insta_posts(query = tolower(input$text1), 
                                     scope = "username",
                                     max_posts = 150, 
                                     scrape_comments = F)
        tioantawibawa <- as.data.frame(tioantawibawa)

    ############################ data untuk Modelling ################################
        #possible parameter
        #1. Caption
        caption_txt = tioantawibawa$body
        #2. Timestamp
        time_posting <- strftime(from_unix(tioantawibawa$timestamp), format="%H")
        time_posting <- as.numeric(time_posting)
        time_posting_cat <- ifelse(time_posting>4 & time_posting<=10,"pagi",
                                   ifelse(time_posting>10 & time_posting<=14,"siang",
                                          ifelse(time_posting>14 & time_posting<= 18.30,"sore","malam")))
        date_posting = as.Date(from_unix(tioantawibawa$timestamp))
        date_posting <- lubridate::ymd(date_posting)
        date_posting_cat <- ifelse(day(date_posting)<= 7,"minggu1",
                                   ifelse(day(date_posting)<= 14,"minggu2",
                                          ifelse(day(date_posting)<= 21,"minggu3","minggu4")))
        #3. Tipe posting
        tipe_posting <- tioantawibawa$type
        #4. count hastagh use
        hashtags_count <- ifelse(tioantawibawa$hashtags=="",0,
                                 ifelse(tioantawibawa$hashtags!="" & str_count(tioantawibawa$hashtags, ",") <= 1,1,
                                        str_count(tioantawibawa$hashtags, ",")+1))
        
        #5. hastag apa saja yang pernah digunakan
        hashtags <- tioantawibawa$hashtags
        #6. count user tag use
        usertags_count <- ifelse(tioantawibawa$usertags=="",0,
                                 ifelse(tioantawibawa$usertags!="" & str_count(tioantawibawa$usertags, ",") <= 1,1,
                                        str_count(tioantawibawa$usertags, ",")+1))
        
        #7. user siapa aja yang pernah di tag
        usertags <- tioantawibawa$usertags
        #8. count user mention use
        mentioned_count <- ifelse(tioantawibawa$mentioned=="",0,
                                  ifelse(tioantawibawa$mentioned!="" & str_count(tioantawibawa$mentioned, ",") <= 1,1,
                                         str_count(tioantawibawa$mentioned, ",")+1))
        #9. user siapa aja yang pernah di mention
        mentioned <- tioantawibawa$mentioned
        #10. Status
        label <- ifelse(tioantawibawa$num_likes >= mean(tioantawibawa$num_likes),"good","bad")
        
        #jadikan satu df
        df <- data.frame(caption_txt,tipe_posting,date_posting,date_posting_cat,
                         time_posting,time_posting_cat,hashtags_count,mentioned_count,
                         usertags_count,hashtags,usertags,mentioned,label)
   ###########################################################
        
    #######################################api instagram######################
    IV <- woebin(df_instagram[,c("time","video","jenisupdate","hari","status")],"status")
    rekomendasijam <- as.character(IV$time[order(IV$time$badprob,decreasing = T),2][1])
    rekomendasijam <- gsub("['['\\)]","",x = rekomendasijam)
    tips1 <- paste0("1. waktu posting lebih baik antara jam ",unlist(str_split(rekomendasijam,pattern = ","))[1],
                             " sampai jam ",unlist(str_split(rekomendasijam,pattern = ","))[2],".")
    rekoemndasijenisupdate <- as.character(IV$jenisupdate[order(IV$jenisupdate$badprob,decreasing = T),2][1])
    rekoemndasijenisupdate <- gsub("['['\\)\\%]","",x = rekoemndasijenisupdate)
    tips2 <- paste0("2. jenis posting lebih baik dalam bentuk ",rekoemndasijenisupdate,".")
    rekomendasihari <- as.character(IV$hari[order(IV$hari$badprob,decreasing = T),2][1])
    rekomendasihari <- gsub("['['\\)\\%]","",x = rekomendasihari)
    tips3 <- paste0("3. Hari untuk posting lebih baik pada ",rekomendasihari,".")
    
    #$############### rekomendasi kata2 yang muncul #################
    file_stop <- file("helper/id.stopwords.02.01.2016.txt",open = "r")
    id_stopwords <- readLines(file_stop)
    close(file_stop)
    id_stopwords = c(id_stopwords, "amp")
    stopwordku <- read.table("helper/id.stopwords.02.01.2016.txt")
    tambahan <- c(paste0("",input$text1),"ini","untuk","nih","mestinya","lho","klo","kalo","Tks","yg","dpt","and","the","not","you","for","but","suka","with","just","all")
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    caption_txt <- df_instagram$caption
    caption_txt <- sapply(caption_txt,function(row) iconv(row, "latin1", "ASCII", sub=""))
    caption_txt <- caption_txt %>% 
        replace_html() %>% # replace html with blank 
        replace_url() 
    caption_txt <- caption_txt %>% 
        replace_emoji(.) %>% 
        replace_html(.)
    caption_txt <- caption_txt %>% 
        replace_tag(caption_txt, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
        replace_hash(caption_txt, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags
    
    caption_txt <- gsub( "\n"," ",caption_txt)
    
    stemming <- function(x){
        paste(lapply(x,katadasar),collapse = " ")}
    caption_txt <- lapply(tokenize_words(caption_txt[]), stemming)
    corpusbri <- Corpus(VectorSource(caption_txt))
    corpusbri <- tm_map(corpusbri, content_transformer(removeURL))
    corpusbri <- tm_map(corpusbri, removePunctuation)
    corpusbri <- tm_map(corpusbri,content_transformer(tolower))
    corpusbri <- tm_map(corpusbri, removeWords, as.character(stopwordku$V1))
    corpusbri <- tm_map(corpusbri, removeWords, tambahan)
    corpusbri <- tm_map(corpusbri, removeWords, id_stopwords)
    corpusbri <- tm_map(corpusbri, removeNumbers)
    corpusbri <- tm_map(corpusbri, stripWhitespace)
    #corpusbri <- tm_map(corpusbri, stemDocument)
    doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpusbri)))
    tdmtio = TermDocumentMatrix(corpusbri)
    term.freq <- rowSums(as.matrix(tdmtio))
    term.freq2 <- subset(term.freq, term.freq > as.numeric(head(sort(term.freq,decreasing=TRUE), n = 20)[20]))
    kata <- names(term.freq2)
    kata_final <- "-"
    for (i in 1:length(kata)) {
        kata_final <- paste0(kata[i]," , ",kata_final)
    }
    kata_final <- gsub(" , -","",kata_final)
    tips4 <- paste0("4. topik/kata yang sebaiknya digunakan :",kata_final)
    
############################modelling#####################
    caption_txt <- sapply(df$caption_txt,function(row) iconv(row, "latin1", "ASCII", sub=""))
    caption_txt <- caption_txt %>% 
        replace_html() %>% # replace html with blank 
        replace_url() 
    caption_txt <- caption_txt %>% 
        replace_emoji(.) %>% 
        replace_html(.)
    caption_txt <- caption_txt %>% 
        replace_tag(caption_txt, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
        replace_hash(caption_txt, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags
    
    caption_txt <- gsub( "\n"," ",caption_txt)
    
    stemming <- function(x){
        paste(lapply(x,katadasar),collapse = " ")}
    caption_txt <- lapply(tokenize_words(caption_txt[]), stemming)
    
    corpusbri <- Corpus(VectorSource(caption_txt))
    corpusbri <- tm_map(corpusbri, content_transformer(removeURL))
    corpusbri <- tm_map(corpusbri, removePunctuation)
    corpusbri <- tm_map(corpusbri,content_transformer(tolower))
    corpusbri <- tm_map(corpusbri, removeWords, as.character(stopwordku$V1))
    corpusbri <- tm_map(corpusbri, removeWords, tambahan)
    corpusbri <- tm_map(corpusbri, removeWords, id_stopwords)
    corpusbri <- tm_map(corpusbri, removeNumbers)
    corpusbri <- tm_map(corpusbri, stripWhitespace)
    dtm_tm <- DocumentTermMatrix(corpusbri)
    #inspect(dtm_tm)
    
    
    set.seed(100)
    split_75 <- sample(nrow(dtm_tm), nrow(dtm_tm)*0.75)
    df_train <- dtm_tm[split_75, ]
    df_test <- dtm_tm[-split_75, ]
    
    train_labels <- df[split_75, "label"]
    test_labels <- df[-split_75, "label"]
    prop.table(table(train_labels))
    
    #To reduce noise, we want to train our naive bayes classifier using only words that appear in at least 20 messages (~0.36%):
    
    set.seed(100)
    dtm_freq <- NA
    for (i in 1:10) {
        dtm_freq[i] <- length(findFreqTerms(dtm_tm, i))
        dtm_freq[i] <- unlist(dtm_freq[i])
    }
    dtm_freq_df <- data.frame(dtm_freq,c(1:10))
    dtm_freq_cek <- dtm_freq_df[dtm_freq_df$dtm_freq<=20,2]
    med <- ifelse(length(dtm_freq_cek)==0, 10,dtm_freq_df[1])
#    med <- dtm_freq_df[1]
    dtm_freq <- findFreqTerms(dtm_tm, med)
    # take a look at some of the words that will be used as predictors in our classifier:
    
    df_train <- df_train[,dtm_freq]
    df_train <- as.data.frame(as.matrix(df_train), stringsAsFactors=False)
    df_test <- df_test[,dtm_freq]
    df_test <- as.data.frame(as.matrix(df_test), stringsAsFactors=False)
    
    # hastag
    hashtags <- gsub(","," ", hashtags)
    hashtags <- ifelse(hashtags=="","",paste0(hashtags,"_hashtags"))
    corpushastag <- Corpus(VectorSource(hashtags))
    dtm_hastag <- DocumentTermMatrix(corpushastag)
    df_hashtag <- as.data.frame(as.matrix(dtm_hastag), stringsAsFactors=False)
    df_train_h <- df_hashtag[split_75, ]
    df_test_h <- df_hashtag[-split_75, ]
    
    # usertags
    usertags <- gsub(","," ", usertags)
    usertags <- ifelse(usertags=="","",paste0(usertags,"_usertag"))
    corpususer <- Corpus(VectorSource(usertags))
    dtm_user <- DocumentTermMatrix(corpususer)
    df_user <- as.data.frame(as.matrix(dtm_user), stringsAsFactors=False)
    df_train_u <- df_user[split_75, ]
    df_test_u <- df_user[-split_75, ]
    
    # mentioned
    mentioned <- gsub(","," ", mentioned)
    mentioned <- ifelse(mentioned=="","",paste0(mentioned,"_usermention"))
    corpusmention <- Corpus(VectorSource(mentioned))
    dtm_mention <- DocumentTermMatrix(corpusmention)
    df_mention <- as.data.frame(as.matrix(dtm_mention), stringsAsFactors=False)
    df_train_m <- df_mention[split_75, ]
    df_test_m <- df_mention[-split_75, ]
    
    # rest of parameter
    df_rest <- df[,c("tipe_posting","date_posting_cat","time_posting_cat","hashtags_count","usertags_count","mentioned_count")]
    df_rest_train <- df_rest[split_75, ]
    df_rest_test <- df_rest[-split_75, ]
    
    all_data_train <- cbind(df_train,df_train_h,df_train_u,df_train_m)
    all_data_test <- cbind(df_test,df_test_h,df_test_u,df_test_m)
    # Takes an input, "x" and assign x to a 1 or 0
    bernoulli_conv <- function(x){
        x <- as.factor(as.numeric(x > 0))
    }
    
    
    train_bn <- apply(all_data_train, 2, bernoulli_conv)
    train_bn <- cbind(train_bn,df_rest_train)
    test_bn <- apply(all_data_test, 2, bernoulli_conv)
    test_bn <- cbind(test_bn,df_rest_test)
    num_train <- colSums(apply(train_bn, 2, as.numeric))
    num_test <- colSums(apply(test_bn, 2, as.numeric))
    
    isnta_model <- naiveBayes(train_bn, train_labels, laplace = 1)
    insta_prediction <- predict(isnta_model, test_bn)
    akurasi <- sum(insta_prediction == test_labels)/length(test_labels)*100
    
    ###################implementasi######################################
    #implementation
    imp_caption_txt <- as.vector(input$text2)
    imp_time_posting <- ifelse(is.na(as.numeric(input$text3)),0,as.numeric(input$text3))
    imp_time_posting_cat <- ifelse(imp_time_posting>4 & imp_time_posting<=10,"pagi",
                                   ifelse(imp_time_posting>10 & imp_time_posting<=14,"siang",
                                          ifelse(imp_time_posting>14 & imp_time_posting<= 18.30,"sore","malam")))
    imp_date_posting <- "2020-12-07"
   # imp_date_posting <- ymd(imp_date_posting)
    hariini <- as.character(today())
    imp_date_posting <- ifelse(is.na(imp_date_posting)==TRUE,hariini,imp_date_posting)
    imp_date_posting_cat <- ifelse(day(imp_date_posting)<= 7,"minggu1",
                                   ifelse(day(imp_date_posting)<= 14,"minggu2",
                                          ifelse(day(imp_date_posting)<= 21,"minggu3","minggu4")))
    imp_tipe_posting <- input$variable
    
    capiton_split <- unlist(str_split(imp_caption_txt," "))
    imp_hashtags <- grep('#',x = capiton_split,fixed = T,value = T)
    imp_hashtags_count <- length(imp_hashtags)
    imp_hashtags <- gsub("#","",imp_hashtags)
    imp_hashtags <- paste0(imp_hashtags,"_hashtags")
    imp_hashtags_final <- NULL
    for (i in 1:length(imp_hashtags)) {
        imp_hashtags_final <- paste0(imp_hashtags[i]," ",imp_hashtags_final)
    }
    
    imp_usertags <-input$text5
    imp_usertags <- gsub("[@,]","",imp_usertags)
    imp_usertags <- unlist(str_split(imp_usertags," "))
    imp_usertags_count <- length(imp_usertags)
    imp_usertags <- paste0(imp_usertags,"_usertag")
    imp_usertags_final <- NULL
    for (i in 1:length(imp_usertags)) {
        imp_usertags_final <- paste0(imp_usertags[i]," ",imp_usertags_final)
    }
    
    imp_mentioned <- grep('@',x = capiton_split,fixed = T,value = T)
    imp_mentioned_count <- length(imp_mentioned)
    imp_mentioned <- gsub("@","",imp_mentioned)
    imp_mentioned <- paste0(imp_mentioned,"_usermention")
    imp_mentioned_final <- NULL
    for (i in 1:length(imp_mentioned)) {
        imp_mentioned_final <- paste0(imp_mentioned[i]," ",imp_mentioned_final)
    }
    
    
    df_final <- data.frame(imp_caption_txt,imp_tipe_posting,imp_time_posting_cat,imp_date_posting,imp_date_posting_cat,
                           imp_time_posting,imp_hashtags_count,imp_mentioned_count,
                           imp_usertags_count,imp_hashtags_final,imp_usertags_final,imp_mentioned_final)
    
    #Text mining
    # VCorpus requires a source object, which can be created using VectorSource
    caption_txt <- sapply(df_final$imp_caption_txt,function(row) iconv(row, "latin1", "ASCII", sub=""))
    caption_txt <- caption_txt %>% 
        replace_html() %>% # replace html with blank 
        replace_url() 
    caption_txt <- caption_txt %>% 
        replace_emoji(.) %>% 
        replace_html(.)
    caption_txt <- caption_txt %>% 
        replace_tag(caption_txt, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
        replace_hash(caption_txt, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags
    
    caption_txt <- gsub( "\n"," ",caption_txt)
    
    stemming <- function(x){
        paste(lapply(x,katadasar),collapse = " ")}

    caption_txt <- lapply(tokenize_words(caption_txt[]), stemming)
    
    corpusbri <- Corpus(VectorSource(caption_txt))
    corpusbri <- tm_map(corpusbri, content_transformer(removeURL))
    corpusbri <- tm_map(corpusbri, removePunctuation)
    corpusbri <- tm_map(corpusbri,content_transformer(tolower))
    corpusbri <- tm_map(corpusbri, removeWords, as.character(stopwordku$V1))
    corpusbri <- tm_map(corpusbri, removeWords, tambahan)
    corpusbri <- tm_map(corpusbri, removeWords, id_stopwords)
    corpusbri <- tm_map(corpusbri, removeNumbers)
    corpusbri <- tm_map(corpusbri, stripWhitespace)
    dtm_tm <- DocumentTermMatrix(corpusbri)
    dtm_freq <- NA
    for (i in 1:10) {
        dtm_freq[i] <- length(findFreqTerms(dtm_tm, i))
        dtm_freq[i] <- unlist(dtm_freq[i])
    }
    dtm_freq_df <- data.frame(dtm_freq,c(1:10))
    dtm_freq_df <- dtm_freq_df[dtm_freq_df$dtm_freq<=20,2]
    med <- dtm_freq_df[1]
    dtm_freq <- findFreqTerms(dtm_tm, med)
    df_model <- dtm_tm[,dtm_freq]
    df_model <- as.data.frame(as.matrix(dtm_tm), stringsAsFactors=False)
    
    hashtags <- gsub(","," ", df_final$imp_hashtags_final)
    corpushastag <- Corpus(VectorSource(hashtags))
    dtm_hastag <- DocumentTermMatrix(corpushastag)
    df_hashtag <- as.data.frame(as.matrix(dtm_hastag), stringsAsFactors=False)
    
    usertags <- gsub(","," ", df_final$imp_usertags_final)
    corpususer <- Corpus(VectorSource(usertags))
    dtm_user <- DocumentTermMatrix(corpususer)
    df_user <- as.data.frame(as.matrix(dtm_user), stringsAsFactors=False)
    
    mentioned <- gsub(","," ", df_final$imp_mentioned_final)
    corpusmention <- Corpus(VectorSource(mentioned))
    dtm_mention <- DocumentTermMatrix(corpusmention)
    df_mention <- as.data.frame(as.matrix(dtm_mention), stringsAsFactors=False)
    
    df_rest <- df_final[,c("imp_tipe_posting","imp_date_posting_cat","imp_time_posting_cat","imp_hashtags_count","imp_hashtags_count","imp_mentioned_count")]
    
    all_data <- data.frame(df_model,df_hashtag,df_user,df_mention, stringsAsFactors = T)
    
    df_bn <- data.frame(all_data,df_rest)
    
    spam_prediction <- predict(isnta_model, df_bn,type = "raw")
    
    
    ##################################
    output$outputtext1 <- renderText({
        input$text1
    })
    
    output$banner1 <- renderText({
        paste0("","<img src='",df_instagram$foto[1],"'>")
    })
    
    output$hasil <- renderUI({
        str1 <- "tips postingan instagram anda :"
        str2 <- paste0("Probabilitas dapat banyak LIKE dari caption diatas : ",round(spam_prediction[2],2)*100,"%, dengan akurasi : ", round(akurasi,2),"%")
    HTML(paste(paste0("Hi ",df_instagram[1,1]),"",str2,"",str1,tips1,tips2,tips3,tips4,sep = '<br/>'))
    })
    }})
    
}

# Run the application 
shinyApp(ui = ui, server = server)

