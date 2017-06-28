writeIRAPfull <- function(
                         IRAPname="IRAP", 
                         catCol="green",
                         poswords,
                         negwords,
                         tgtCol="black",
                         Awords, 
                         Bwords, 
                         qsf=FALSE,
                         qsfTemplate=NULL,
                         pause=250,
                         stimuliShowCount=12,
                         correct.error=TRUE,
                         showAlternateCategory=FALSE,
                         tooSlowMessageMS=2000,
                         tooSlowMessageShowTimeMS=600,
                         practiceSuccessThreasholdCorrect=0.80,
                         practiceSuccessThreasholdMedianMS=2000,
                         showPracticeStats=1
) {

    library(jsonlite)
  
    cat("writeIRAPfull\n")
  
    irapname <- IRAPname

    cat("Create param files\n")
    
    params <- {}
    params$forceErrorCorrection <- as.integer(correct.error)
    params$showAlternateCategory <- as.integer(showAlternateCategory)
    params$interQuestionDelay <- pause
    params$stimuliShowCount <- stimuliShowCount
    params$leftKeyChar <- "D"
    params$rightKeyChar <- "K"
    params$tooSlowMessageMS <- tooSlowMessageMS
    params$tooSlowMessageShowTimeMS <- tooSlowMessageShowTimeMS
    params$practiceSuccessThreasholdCorrect <- practiceSuccessThreasholdCorrect
    params$practiceSuccessThreasholdMedianMS <- practiceSuccessThreasholdMedianMS
    params$showPracticeStats <- showPracticeStats

    params$reverseAnswers <- 0
    params$practiceMode <- 0

    codePARAMS_test_pos <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")
    
    params$reverseAnswers <- 1
    codePARAMS_test_neg <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")

    params$reverseAnswers <- 0
    params$practiceMode <- 1
    codePARAMS_practice_pos <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")

    params$reverseAnswers <- 1
    codePARAMS_practice_neg <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")

    cat("Create stimuli files\n")
    
    posstim <- data.frame(stimulus=paste("<b style='color:",catCol,"'>",poswords,"</b>",sep=""),
                                         correct="NA", index=1)
    negstim <- data.frame(stimulus=paste("<b style='color:",catCol,"'>",negwords,"</b>",sep=""),
                                         correct="NA", index=2)
    
    Astim_tmp <- lapply(Awords, function(x) {
      data.frame(stimulus=paste("<b style='color:",tgtCol,"'>",x,"</b>",sep=""),correct="69")
    })
    Astim <- as.data.frame(do.call(rbind, Astim_tmp))
    Astim$index <- seq.int(nrow(Astim))+2

    Bstim_tmp <- lapply(Bwords, function(x) {
      data.frame(stimulus=paste("<b style='color:",tgtCol,"'>",x,"</b>",sep=""),correct="73")
    })
    Bstim <- as.data.frame(do.call(rbind, Bstim_tmp))
    Bstim$index <- seq.int(nrow(Bstim))+nrow(Astim)+2
    
    codeSTIM <- paste(
      "posstim =",toJSON(posstim),";\n",
      "negstim =",toJSON(negstim),";\n",
      "Astim =",toJSON(Astim),";\n",
      "Bstim =",toJSON(Bstim),";\n",
      sep="")

    cat("Reading static files\n")
    
    x <- system.file("codefiles", "codeA.js", package="irapgen")
    codeA <- readChar(x,file.info(x)$size) 
    
    x <- system.file("codefiles", "codeIMG.txt", package="irapgen")
    codeIMG <- readChar(x,file.info(x)$size) 
    
    x <- system.file("codefiles", "codeB.txt", package="irapgen")
    codeB <- readChar(x,file.info(x)$size) 
    
    x <- system.file("codefiles", "codeC.txt", package="irapgen")
    codeC <- readChar(x,file.info(x)$size) 
    
    # practice positive
    js_pp<-paste(codeA, codeIMG, codeB, codePARAMS_practice_pos, codeSTIM, codeC,sep="")
    
    # practice negative
    js_pn<-paste(codeA, codeIMG, codeB, codePARAMS_practice_neg, codeSTIM, codeC,sep="")
    
    # test positive
    js_tp<-paste(codeA, codeIMG, codeB, codePARAMS_test_pos, codeSTIM, codeC,sep="")
    
    # test negative
    js_tn<-paste(codeA, codeIMG, codeB, codePARAMS_test_neg, codeSTIM, codeC,sep="")

    cat("Create directories\n")
    
    d1 <- paste("1 ",irapname,"_pp", sep='')
    d2 <- paste("2 ",irapname,"_pn", sep='')
    d3 <- paste("3 ",irapname,"_tp", sep='')
    d4 <- paste("4 ",irapname,"_tn", sep='')
    
    dir.create(paste(getwd(),"/",d1,sep=""),showWarnings = FALSE)
    dir.create(paste(getwd(),"/",d2,sep=""),showWarnings = FALSE)
    dir.create(paste(getwd(),"/",d3,sep=""),showWarnings = FALSE)
    dir.create(paste(getwd(),"/",d4,sep=""),showWarnings = FALSE)

    cat("Copy html files\n")
    
    file.copy(system.file("codefiles", "html_template_practice.html", package="irapgen"), 
              paste(getwd(),"/",d1,"/Q1 h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_practice.html", package="irapgen"), 
              paste(getwd(),"/",d2,"/Q2 h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_test.html", package="irapgen"), 
              paste(getwd(),"/",d3,"/Q3 h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_test.html", package="irapgen"), 
              paste(getwd(),"/",d4,"/Q4 h.txt",sep="") )    
 
    cat("Create js files\n")
    
    writeChar(js_pp, paste(getwd(),"/",d1,"/Q1 J.txt",sep="") )
    writeChar(js_pn, paste(getwd(),"/",d2,"/Q2 J.txt",sep="") )
    writeChar(js_tp, paste(getwd(),"/",d3,"/Q3 J.txt",sep="") )
    writeChar(js_tn, paste(getwd(),"/",d4,"/Q4 J.txt",sep="") )


  ## if qsf argument is true, make a qsf file
  if(qsf==T){
    
    if (is.null(qsfTemplate)) {
      qsfTemplate=system.file("codefiles", "IRAP_V3.qsf", package="irapgen")
    }
    
    #code below uses lowercase
    irapname <- IRAPname
    
    #copy the template file to the wd
    file.copy(qsfTemplate, file.path(getwd()))
    
    filename = function() {
      paste('IRAP-', irapname, '.qsf', sep='')
    }
    
    library(jsonlite)
    
    q <- fromJSON(qsfTemplate)
    
    q$SurveyName <- irapname
    q$SurveyEntry$SurveyName <- irapname
    
    files=c(paste("1 ",irapname,"_pp", sep=''),
            paste("2 ",irapname,"_pn", sep=''),
            paste("3 ",irapname,"_tp", sep=''),
            paste("4 ",irapname,"_tn", sep=''))
    
    
    filecontent <- c()
    txtfiles <- list.files(path=files, pattern="*.txt", full.names=T, recursive=T)
    cat(toJSON(txtfiles))
    lapply(txtfiles, function(x) {
      cat(paste("reading file:",x,"\n"))
      t <- readChar(x,file.info(x)$size) # load file
      k <- gsub("^.*/(Q[0-4]) ([hJ]).*$", "\\1\\2", x)
      filecontent[[k]] <<- t
    })
    
    
    cat("Replacing html and Javascript content....",qsfTemplate,"\n")
    for (i in 1:length(q$SurveyElements$Payload)) {
      m <- 0
      if (is.list(q$SurveyElements$Payload[i][[1]])) {
        m <- length(grep("Q[0-4] [PT][PN]", q$SurveyElements$Payload[i][[1]]$DataExportTag))
      }
      if (!(m == 0)) {
        # q$SurveyElements$Payload[i][[1]]$DataExportTag
        qnumber <- gsub("^(Q[0-4]) [PT][PN]$", "\\1", q$SurveyElements$Payload[i][[1]]$DataExportTag)
        qnumberhtml <- paste(qnumber,'h',sep="")
        qnumberjs <- paste(qnumber,'J',sep="")
        paste(qnumberhtml,qnumberjs)
        q$SurveyElements$Payload[i][[1]]$QuestionText <- filecontent[[qnumberhtml]]
        q$SurveyElements$Payload[i][[1]]$QuestionJS <- filecontent[[qnumberjs]]
      } else {
        # if (exists("q$SurveyElements$Payload[i][[1]]$QuestionText") &&
        #     length(q$SurveyElements$Payload[i][[1]]$QuestionText)>0) {
        #   qtext <- q$SurveyElements$Payload[i][[1]]$QuestionText
        #   qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
        #   q$SurveyElements$Payload[i][[1]]$QuestionText <- qtext
        # }
        # if (exists("q$SurveyElements$Payload[i][[1]]$QuestionJS") &&
        #     length(q$SurveyElements$Payload[i][[1]]$QuestionJS)>0) {
        #   qtext <- q$SurveyElements$Payload[i][[1]]$QuestionJS
        #   qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
        #   q$SurveyElements$Payload[i][[1]]$QuestionJS <- qtext
        # }
        # if (exists("q$SurveyElements$Payload[i][[1]]$QuestionDescription") &&
        #     length(q$SurveyElements$Payload[i][[1]]$QuestionDescription)>0) {
        #   qtext <- q$SurveyElements$Payload[i][[1]]$QuestionDescription
        #   qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
        #   q$SurveyElements$Payload[i][[1]]$QuestionDescription <- qtext
        # }
        # if (exists("q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display") &&
        #     length(q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display)>0) {
        #   qtext <- q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display
        #   qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
        #   q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display <- qtext
        # }
        # if (exists("q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display") &&
        #     length(q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display)>0) {
        #   qtext <- q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display
        #   qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
        #   q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display <- qtext
        # }
      }
    }
    
    if (is.character(q$SurveyElements$Payload$DataExportTag)) {
      for (i in 1:length(q$SurveyElements$Payload$DataExportTag)) {
        m <- length(grep("Q[0-4] [PT][PN]", q$SurveyElements$Payload$DataExportTag[i]))
        if (!(m == 0)) {
          qnumber <- gsub("^(Q[0-4]) [PT][PN]$", "\\1", q$SurveyElements$Payload$DataExportTag[i])
          qnumberhtml <- paste(qnumber,'h',sep="")
          qnumberjs <- paste(qnumber,'J',sep="")
          paste(qnumberhtml,qnumberjs)
          q$SurveyElements$Payload$QuestionText[i] <- filecontent[[qnumberhtml]]
          q$SurveyElements$Payload$QuestionJS[i] <- filecontent[[qnumberjs]]
        }
      }
    }
    
    fname <- filename()
    cat(paste("Generating JSON....",fname,"\n"),sep="")
    qjson <- toJSON(q,null="null",auto_unbox=T)
    minify(qjson)
    write(qjson, filename())
  
    #remove template
    # file.remove("FullTemplate_-_For_Shiny_V9.qsf")
    
    #remove HTML and JavaScript folders if QSF
    unlink(files[1], recursive = T)
    unlink(files[2], recursive = T)
    unlink(files[3], recursive = T)
    unlink(files[4], recursive = T)
  }

}
