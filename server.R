library(shiny)

mirrorView <- function(mat, mltc=1) {
  mirrorView <- matrix(0, 21,24)
  for (i in 1:21) {
    mirrorView[i, ] <- rev(mat[i, ])
  }
  if (mltc != 1) {
    mirrorView <- mirrorView[, c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11,
      14, 13, 16, 15, 18, 17, 20, 19, 22, 21, 24, 23)]
  }
  mirrorView
}

c64code <- function(mat, asis=TRUE, mltc=FALSE) {
  c64code <- matrix(0, 21, 3)
  if (!asis) {
    mat <- mirrorView(mat, mltc=mltc)
  }
  for (i in 1:21) {
    for (j in 1:8) {
      if (mat[i, j] == 1) {
        c64code[i, 1] <- c64code[i, 1] + 2^(8-j)
      }
      if (mat[i, j + 8] == 1) {
        c64code[i, 2] <- c64code[i, 2] + 2^(8-j)
      }
      if (mat[i, j + 16] == 1) {
        c64code[i, 3] <- c64code[i, 3] + 2^(8-j)
      }
    }
  }
  as.data.frame(
    matrix(sapply(c64code, function(x) as.integer(round(x, 0))), 21, 3))
}

#board <- matrix(sample(0:1, 24*21, replace=TRUE), 21, 24)
                                                             
shinyServer(function(input, output) {
  v <- reactiveValues(board=matrix(0, 21, 24), clicked=TRUE)
  
  data <- reactive({
    if (v$clicked) {
      board <<- v$board
      v$clicked <- FALSE
    }
    if (length(input$plot_click$x) > 0 && length(input$plot_click$y) > 0 && 
        input$plot_click$x >= 0.5 && input$plot_click$x <= 24.5 &&
        input$plot_click$y >= 0.5 && input$plot_click$y <= 21.5) {
      if (input$oneMlt == 1) {
        i <- floor(input$plot_click$y + 0.5)
        j <- floor(input$plot_click$x + 0.5)
      
        board[i, j] <<- (board[i, j] + 1) %% 2       
      } else {                 
        i <- floor(input$plot_click$y + 0.5)
        j <- floor(input$plot_click$x + 0.5)
        if (j %% 2 == 0) {
          j <- j - 1
        }
        tmp <- 0
        if (board[i, j] == 0 && board[i, j + 1] == 0) {
          tmp <- 1
          board[i, j] <<- 1
        }
        if (tmp == 0 && board[i, j] == 1 && board[i, j + 1] == 0) {
          tmp <- 1
          board[i, j] <<- 0
          board[i, j + 1] <<- 1
        }
        if (tmp == 0 && board[i, j] == 0 && board[i, j + 1] == 1) {
          tmp <- 1
          board[i, j] <<- 1
        }
        if (tmp == 0 && board[i, j] == 1 && board[i, j + 1] == 1) {
          board[i, j] <<- 0
          board[i, j + 1] <<- 0
        }
      }
    }
    
    board
  })
  
  output$plotSprite <- renderPlot({    
    par(mar=c(0.5, 0.5, 1.5, 0.5), mai=c(0.09, 0.09, 0.23, 0.09), 
      cex=0.9, bty="n", bg=input$backColor, col="tomato", col.main="black", 
      cex.lab=0.8)
    x <- 1:24
    y <- rep(12, length(x))
    plot(x, y, pch='.', xlab="", ylab="", main="", col=input$backColor, cex=1, 
      xlim=c(0.5, 24.5), ylim=c(21.5, 0.5), asp=1.1, lwd=1, xaxt="n", yaxt="n")

    abline(v=c(8.5, 16.5), lty=3, lwd=5)
    abline(h=seq(from=0.5, to=21.5, by=1), lty=3)

    if (input$oneMlt == 1) {
      abline(v=seq(from=0.5, to=24.5, by=1), lty=3)
    } else {
      abline(v=seq(from=0.5, to=24.5, by=2), lty=3)
    }
    
    text(rep(0.1, 21), 1:21, label=c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
      "21"), cex=0.8)
    text(rep(24.95, 21), 1:21, label=c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
      "21"), cex=0.8)
    text(1:24, rep(0.1, 24), label=c("128", "64", "32", "16", "8", "4", "2", 
      "1", "128", "64", "32", "16", "8", "4", "2", "1",
      "128", "64", "32", "16", "8", "4", "2", "1"), cex=0.8)
    text(1:24, rep(21.8, 24), label=c("128", "64", "32", "16", "8", "4", "2", 
      "1", "128", "64", "32", "16", "8", "4", "2", "1",
      "128", "64", "32", "16", "8", "4", "2", "1"), cex=0.8)    
    
    board <<- data()

    for (i in 1:21) {
      if (input$oneMlt == 1) {
        for (j in 1:24) {
          if (board[i, j] == 1) {
            rect(j-0.5, i-0.5, j+0.5, i+0.5, col=input$mainColor)
          }
        }
      } else {
        for (j in seq(1, 24, 2)) {
          if (board[i, j] == 1 && board[i, j + 1] == 0) {
            rect(j-0.5, i-0.5, j+1.5, i+0.5, col=input$mainColor)
          }
          if (board[i, j] == 0 && board[i, j + 1] == 1) {
            rect(j-0.5, i-0.5, j+1.5, i+0.5, col=input$mlt1Color)
          }
          if (board[i, j] == 1 && board[i, j + 1] == 1) {
            rect(j-0.5, i-0.5, j+1.5, i+0.5, col=input$mlt2Color)
          }
        }
      }
    }
  })
  
  output$tip <- renderText({
    if (input$oneMlt != 1) {
      paste("Click on the plot to change colors of two-bits blocks, i. e.", 
        "background -> main -> mlt. 1 -> mlt. 2 -> background -> ...")
    } else {
      "Click on the plot to turn on/off bits."
    }
  })
  
  output$c64code <- renderTable({
    c64code <- c64code(data(), mltc=input$oneMlt)
    names(c64code) <- rep("", 3)
    c64code
  })
  
  output$c64data <- renderTable({
    mat <- c64code(data(), mltc=input$oneMlt)
    startLine <- input$startLine
    c64data <- data.frame(v1=character())
    
    for (i in 1:7) {# i=1
      c64data <- rbind(c64data, data.frame(v1=paste(" data ", paste(paste( 
        mat[3*(i-1)+1, ], collapse=","), paste(mat[3*(i-1)+2, ], collapse=","), 
        paste(mat[3*(i-1)+3, ], collapse=",")), sep=""), 
        row.names=startLine + (i-1)))
    }
    names(c64data) <- ""
    c64data
  })
  
  observeEvent(input$clear, {
    v$board <- matrix(0, 21, 24)
    v$clicked <- TRUE
  })

  observeEvent(input$reverse, {
    v$board <- mirrorView(data(), mltc=input$oneMlt)
    v$clicked <- TRUE
  })
  
  output$image <- renderImage({
    fileImg <- ifelse(length(input$imageFile) > 0, 
      input$imageFile$datapath, "www/ninja.png")
    list(src=fileImg,
         contentType='image/png',
         width=500,
         height=400,
         alt="WTF???")
  }, deleteFile=FALSE)
  
  output$fileName <- renderText({
    paste("plik: ", input$imageFile, sep="")
  })
})

