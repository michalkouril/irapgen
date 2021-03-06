#' 
#' 
#' @title writeIRAPfull
#' @param IRAPname IRAP name
#' @param catCol Category color
#' @param poswords poswords
#' @param negwords negwords
#' @param tgtCol Target color
#' @param Awords Awords
#' @param Bwords Bwords
#' @param qsf Generate QSF
#' @param qsfTemplate QSF template (if other than default)
#' @param pause pause
#' @param stimuliShowCount stimuliShowCount
#' @param stimuliShowShortPracticeCount stimuliShowShortPracticeCount
#' @param correct.error correct.error
#' @param tooSlowMessagePractice tooSlowMessagePractice
#' @param tooSlowMessageTest tooSlowMessageTest
#' @param tooSlowMessageMS tooSlowMessageMS
#' @param tooSlowMessageShowTimeMS tooSlowMessageShowTimeMS
#' @param practiceSuccessThreasholdCorrect practiceSuccessThreasholdCorrect
#' @param practiceSuccessThreasholdMedianMS practiceSuccessThreasholdMedianMS
#' @param showPracticeStats showPracticeStats
#' @param qsfQSSP qsfQSSP
#' @param qsfQSOP qsfQSOP
#' @param qsfQSP qsfQSP
#' @param qsfQOP qsfQOP
#' @param qsfQST qsfQST
#' @param qsfQOT qsfQOT
#' @import jsonlite
#' @examples 
#' writeIRAPfull(poswords=c("pos1"),negwords=c("neg1"),
#'               Awords=c("a1","a2","a3"),Bwords=c("b1","b2","b3"),
#'               qsf=TRUE)
#'
#' 
#' @export
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
                         stimuliShowCount=0,
                         stimuliShowShortPracticeCount=4,
                         correct.error=TRUE,
                         tooSlowMessagePractice=TRUE,
                         tooSlowMessageTest=FALSE,
                         tooSlowMessageMS=2000,
                         tooSlowMessageShowTimeMS=600,
                         practiceSuccessThreasholdCorrect=0.80,
                         practiceSuccessThreasholdMedianMS=2000,
                         showPracticeStats=1,
                         qsfQSSP="Q60 SSP",    # short SAME practice
                         qsfQSOP="Q57 SOP",    # short OPPOSITE practice
                         qsfQSP="Q4 SP",     # SAME practice
                         qsfQOP="Q6 OP",     # OPPOSITE practice
                         qsfQST="Q8 ST",     # SAME test
                         qsfQOT="Q10 OT"    # OPPOSITE test
                         
) {

    cat("writeIRAPfull\n")

    # FIXME: add allowed characters checking

    irapname <- IRAPname

    cat("Create param files\n")
    
    params <- {}
    params$forceErrorCorrection <- as.integer(correct.error)
    params$interQuestionDelay <- pause
    params$leftKeyChar <- "D"
    params$rightKeyChar <- "K"
    params$tooSlowMessageMS <- tooSlowMessageMS
    params$tooSlowMessageShowTimeMS <- tooSlowMessageShowTimeMS
    params$practiceSuccessThreasholdCorrect <- practiceSuccessThreasholdCorrect
    params$practiceSuccessThreasholdMedianMS <- practiceSuccessThreasholdMedianMS
    params$showPracticeStats <- showPracticeStats

    # Test blocks (not practice)
    params$stimuliShowCount <- stimuliShowCount
    params$practiceMode <- 0
    if (tooSlowMessageTest==FALSE) params$tooSlowMessageMS <- 0
    else params$tooSlowMessageMS <- tooSlowMessageMS
      
    params$reverseAnswers <- 0
    codePARAMS_test_pos <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")
    
    params$reverseAnswers <- 1
    codePARAMS_test_neg <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")

    # Practice blocks (not actual test)
    params$stimuliShowCount <- stimuliShowCount
    params$practiceMode <- 1
    if (tooSlowMessagePractice==FALSE) params$tooSlowMessageMS <- 0
    else params$tooSlowMessageMS <- tooSlowMessageMS
    
    params$reverseAnswers <- 0
    codePARAMS_practice_pos <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")

    params$reverseAnswers <- 1
    codePARAMS_practice_neg <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")

    # Short Practice Blocks
    params$stimuliShowCount <- stimuliShowShortPracticeCount
    
    params$reverseAnswers <- 0
    codePARAMS_short_practice_pos <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")
    
    params$reverseAnswers <- 1
    codePARAMS_short_practice_neg <- paste("initParams =",toJSON(params,auto_unbox = TRUE),";\n",sep="")
    
    
       
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
    
    # short practice positive
    js_spp<-paste(codeA, codeIMG, codeB, codePARAMS_short_practice_pos, codeSTIM, codeC,sep="")
    
    # short practice negative
    js_spn<-paste(codeA, codeIMG, codeB, codePARAMS_short_practice_neg, codeSTIM, codeC,sep="")
    
    # practice positive
    js_pp<-paste(codeA, codeIMG, codeB, codePARAMS_practice_pos, codeSTIM, codeC,sep="")
    
    # practice negative
    js_pn<-paste(codeA, codeIMG, codeB, codePARAMS_practice_neg, codeSTIM, codeC,sep="")
    
    # test positive
    js_tp<-paste(codeA, codeIMG, codeB, codePARAMS_test_pos, codeSTIM, codeC,sep="")
    
    # test negative
    js_tn<-paste(codeA, codeIMG, codeB, codePARAMS_test_neg, codeSTIM, codeC,sep="")

    cat("Create directory\n")
  
    prjDir <- irapname;
    dir.create(paste(getwd(),"/",prjDir,sep=""),showWarnings = FALSE)

    cat("Copy html files\n")
 
    file.copy(system.file("codefiles", "html_template_practice.html", package="irapgen"), 
              paste(getwd(),"/",prjDir,"/",qsfQSSP,"_h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_practice.html", package="irapgen"), 
              paste(getwd(),"/",prjDir,"/",qsfQSOP,"_h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_practice.html", package="irapgen"), 
              paste(getwd(),"/",prjDir,"/",qsfQSP,"_h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_practice.html", package="irapgen"), 
              paste(getwd(),"/",prjDir,"/",qsfQOP,"_h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_test.html", package="irapgen"), 
              paste(getwd(),"/",prjDir,"/",qsfQST,"_h.txt",sep="") )
    
    file.copy(system.file("codefiles", "html_template_test.html", package="irapgen"), 
              paste(getwd(),"/",prjDir,"/",qsfQOT,"_h.txt",sep="") )    
 
    cat("Create js files\n")
    
    writeChar(js_spp, paste(getwd(),"/",prjDir,"/",qsfQSSP,"_J.txt",sep="") )
    writeChar(js_spn, paste(getwd(),"/",prjDir,"/",qsfQSOP,"_J.txt",sep="") )
    writeChar(js_pp, paste(getwd(),"/",prjDir,"/",qsfQSP,"_J.txt",sep="") )
    writeChar(js_pn, paste(getwd(),"/",prjDir,"/",qsfQOP,"_J.txt",sep="") )
    writeChar(js_tp, paste(getwd(),"/",prjDir,"/",qsfQST,"_J.txt",sep="") )
    writeChar(js_tn, paste(getwd(),"/",prjDir,"/",qsfQOT,"_J.txt",sep="") )


  ## if qsf argument is true, make a qsf file
  if(qsf==T){
    
    if (is.null(qsfTemplate)) {
      qsfTemplate=system.file("codefiles", "IRAP_V13n_cb.qsf", package="irapgen")
    }

    cat(paste("Parse QSF template ", qsfTemplate, "\n"))
    
    #copy the template file to the wd
    file.copy(qsfTemplate, file.path(getwd()))
    
    filename = function() {
      paste('IRAP-', irapname, '.qsf', sep='')
    }
    
    q <- fromJSON(qsfTemplate, simplifyVector = FALSE)
    
    q$SurveyName <- irapname
    q$SurveyEntry$SurveyName <- irapname
    
    filecontent <- c()
    qsfPageReplaced <- c()

    for (qsfBlock in c(qsfQSSP, qsfQSOP, qsfQSP, qsfQOP, qsfQST, qsfQOT)) {
       qsfFile<-paste(qsfBlock,"_h.txt",sep="")
       qsfFileFullPath<-paste(getwd(),"/",prjDir,"/",qsfFile,sep="")
       filecontent[[qsfFile]]<-readChar(qsfFileFullPath, file.info(qsfFileFullPath)$size)

       qsfFile<-paste(qsfBlock,"_J.txt",sep="")
       qsfFileFullPath<-paste(getwd(),"/",prjDir,"/",qsfFile,sep="")
       filecontent[[qsfFile]]<-readChar(qsfFileFullPath, file.info(qsfFileFullPath)$size)
       
       qsfPageReplaced[[qsfBlock]] <- 0
    }
    
    
    cat("Replacing html and Javascript content....",qsfTemplate,"\n")
    for (qsfBlock in c(qsfQSSP, qsfQSOP, qsfQSP, qsfQOP, qsfQST, qsfQOT)) {
       for (i in 1:length(q$SurveyElements)) {
          m <- 0
          if (is.list(q$SurveyElements[[i]]$Payload)) {
             # cat(paste("Compare ",qsfBlock," and ",q$SurveyElements$Payload[i][[1]]$DataExportTag,sep=""))
             m <- length(grep(qsfBlock, q$SurveyElements[[i]]$Payload$DataExportTag))
          }
          if (!(m == 0)) {
             # q$SurveyElements$Payload[i][[1]]$DataExportTag
             # qnumber <- gsub("^(Q[0-4]) [PT][PN]$", "\\1", q$SurveyElements$Payload[i][[1]]$DataExportTag)
             cat(paste("Replacing ",qsfBlock,"\n",sep=""))
             qHTML <- paste(qsfBlock,'_h.txt',sep="")
             qJS <- paste(qsfBlock,'_J.txt',sep="")
             q$SurveyElements[[i]]$Payload$QuestionText <- filecontent[[qHTML]]
             q$SurveyElements[[i]]$Payload$QuestionJS <- filecontent[[qJS]]
             qsfPageReplaced[[qsfBlock]] <- 1
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
    }
    
    # for (qsfBlock in c(qsfQSSP, qsfQSOP, qsfQSP, qsfQOP, qsfQST, qsfQOT)) {
    #    if (is.character(q$SurveyElements$Payload$DataExportTag)) {
    #       for (i in 1:length(q$SurveyElements$Payload$DataExportTag)) {
    #          m <- length(grep(qsfBlock, q$SurveyElements$Payload$DataExportTag[i]))
    #          if (!(m == 0)) {
    #             cat(paste("Replacing ",qsfBlock,"\n",sep=""))
    #             qHTML <- paste(qsfBlock,'_h.txt',sep="")
    #             qJS <- paste(qsfBlock,'_J.txt',sep="")
    #             q$SurveyElements$Payload$QuestionText[i] <- filecontent[[qHTML]]
    #             q$SurveyElements$Payload$QuestionJS[i] <- filecontent[[qJS]]
    #             qsfPageReplaced[[qsfBlock]] <- 1
    #          }
    #       }
    #    }
    # }
    
    err <- 0
    for (i in names(qsfPageReplaced)) { 
      if (qsfPageReplaced[[i]] == 0) {
        cat(paste(i, " not found in qsf file\n"))
        err <- 1
      }
    }
    
    
    if (err == 0) {
      fname <- filename()
      cat(paste("Generating JSON....",fname,"\n"),sep="")
      qjson <- toJSON(q,null="null",auto_unbox=T)
      minify(qjson)
      write(qjson, filename())
    } else {
      cat("Error -- not all blocks have been replaced. Not generating JSON..")
      unlink(filename(), recursive = F)
    }
    
    #remove template
    # file.remove("FullTemplate_-_For_Shiny_V9.qsf")
    
    #remove HTML and JavaScript folders if QSF
    unlink(paste(getwd(),"/",prjDir, recursive = T))
  }

}
