library(shiny)
library(shinycssloaders)
library(fmsb)
library(stringr)
library(showtext)
library(png) 


showtext_auto()

shinyServer(function(input, output, session) {
  # 0. 準備------------------------------
  ## データを格納するReactive Values------------------------------
  dataset <- reactiveValues(data = NULL, name = NULL, Moji = NULL, image_paths = NULL)
  
  ## タブ移動の制御------------------------------
  observeEvent(input$go1, {
    updateTabsetPanel(session, "tabs", selected = "漢字グラフ")
  })
  
  
# 1. データ読み込み・画像生成・ラベル抽出処理------------------------------
    data_processed <- reactive({
    req(input$data1)
    inFile2 <- input$data1
    validate(need(str_sub(inFile2$datapath, start = -3) == "csv", "⚠️ CSVファイルをアップロードしてください。"))
    d <- tryCatch({
      read.csv(inFile2$datapath, header = TRUE, fileEncoding = "CP932", check.names = FALSE)
    }, error = function(e) {
      read.csv(inFile2$datapath, header = TRUE, fileEncoding = "UTF-8", check.names = FALSE)
    })
    
    validate(
      need(ncol(d) >= 7 && ncol(d) %% 2 != 0, "⚠️ 列数は7以上の奇数である必要があります。"),
      need(all(sapply(d[, -1], is.numeric)), "⚠️ 2列目以降は数値である必要があります。"),
      need(!any(is.na(d[, -1])), "⚠️ データに欠損値が含まれています。")
    )
    
    names_col <- d[, 1]
    values_df <- d[, -1]
    n_vars <- ncol(values_df) / 2
    
    var_names <- names(values_df)[c(1:n_vars) * 2 - 1]
    kanji_chars <- str_sub(var_names, 1, 1)
    
    ## ラベル抽出ロジック------------------------------
    data_cols <- names(values_df)
    clean_cols <- gsub("\\.\\d+$", "", data_cols)
    n_data_cols <- length(clean_cols)
    odd_names <- clean_cols[seq(1, n_data_cols, by = 2)]
    even_names <- clean_cols[seq(2, n_data_cols, by = 2)]
    
    find_common_part <- function(strs) {
      if(length(strs) == 0) return(NULL)
      if(length(strs) == 1) return(strs)
      if(all(strs == strs[1])) return(strs[1])
      
      chars_list <- strsplit(strs, "")
      min_len <- min(sapply(chars_list, length))
      prefix <- ""
      if(min_len > 0){
        for(i in 1:min_len){
          first_char <- chars_list[[1]][i]
          if(all(sapply(chars_list, function(x) x[i] == first_char))){
            prefix <- paste0(prefix, first_char)
          } else { break }
        }
      }
      
      rev_strs <- sapply(lapply(strsplit(strs, ""), rev), paste, collapse="")
      rev_chars_list <- strsplit(rev_strs, "")
      min_len_rev <- min(sapply(rev_chars_list, length))
      suffix_rev <- ""
      if(min_len_rev > 0){
        for(i in 1:min_len_rev){
          first_char <- rev_chars_list[[1]][i]
          if(all(sapply(rev_chars_list, function(x) x[i] == first_char))){
            suffix_rev <- paste0(suffix_rev, first_char)
          } else { break }
        }
      }
      suffix <- sapply(lapply(strsplit(suffix_rev, ""), rev), paste, collapse="")
      
      label <- paste0(prefix, suffix)
      if(label == "") return(NULL)
      return(label)
    }
    
    l1 <- find_common_part(odd_names)
    l2 <- find_common_part(even_names)
    
    label1 <- if(is.null(l1)) "変数1" else l1
    label2 <- if(is.null(l2)) "変数2" else l2
    
    labels_extracted <- c(label1, label2)
    
    # 画像生成-------------------
    temp_dir <- tempdir()
    img_paths <- character(n_vars)
    
    for(i in 1:n_vars){
      fname <- file.path(temp_dir, paste0("kanji_", session$token, "_", i, ".png"))
      img_paths[i] <- fname
      
      png(fname, width = 400, height = 400, bg = "transparent")
      showtext_begin() 
      par(mar=c(0, 0, 0, 0)) 
      plot.new()
      plot.window(xlim=c(0, 1), ylim=c(0, 1), xaxs="i", yaxs="i")
      text(x=0.5, y=0.5, labels=kanji_chars[i], cex=26, col="black") 
      showtext_end()
      dev.off()
    }
    
    list(
      raw = d,
      names = as.character(names_col),
      values = values_df,
      var_names = var_names,
      kanji_chars = kanji_chars,
      img_paths = img_paths,
      n_vars = n_vars,
      labels = labels_extracted
    )
  })
  
  observe({
    req(data_processed())
    res <- data_processed()
    dataset$name <- res$names
    dataset$Moji <- res$var_names
  })
  
  ### --- タイトル表示 ---------------------------------
  output$title_plot1 <- renderUI({
    req(data_processed())
    res <- data_processed()
    h3(paste0("漢字の横は「", res$labels[1], "」、縦は「", res$labels[2], "」"))
  })
  
  output$title_plot2 <- renderUI({
    req(data_processed())
    res <- data_processed()
    h3(paste0("漢字の大きさは「", res$labels[1], "」、レーダーチャートは「", res$labels[2], "」"))
  })
  
  ### 重複警告------------------------------
  output$duplicate_warning <- renderUI({
    req(data_processed())
    df <- data_processed()$raw
    if(any(duplicated(names(df)))){
      div(
        class = "alert alert-warning",
        style = "color: #856404; background-color: #fff3cd; border-color: #ffeeba; padding: 10px; margin-bottom: 15px; border-radius: 4px;",
        icon("exclamation-triangle"),
        strong("警告:"), "同じ列名が含まれていますが、問題ありませんか？"
      )
    }
  })
  
  output$choose_columns <- renderUI({
    req(dataset$name)
    selectInput("choose_columns", "Data name", choices = dataset$name, selected = dataset$name[1])
  })
  
  ### ---最大値設定の初期値をデータから自動取得 ---------------------------------
  output$max <- renderUI({
    req(input$header != TRUE)
    
    default_val <- 1000
    
    # データが存在すれば最大値を計算して初期値にする
    if (isTruthy(input$data1)) {
      res <- data_processed()
      vals <- res$values
      n <- res$n_vars
      size_cols <- vals[, c(1:n) * 2 - 1, drop = FALSE]
      
      mx <- max(size_cols, na.rm = TRUE)
      if(is.finite(mx) && mx > 0) {
        default_val <- mx
      }
    }
    
    numericInput("maxnum", "データの最大値を設定", value = default_val, min = 1)
  })
  
  output$clust <- renderUI({
    req(input$data2)
    if(input$group == 2){
      numericInput("clustnum", "クラスター数", value = 3, min = 1, max = 10, step = 1)
    }
  })
  
  
# 【描画関数1】従来型漢字グラフ------------------------------
  create_plot1 <- function(input, res_data) {
    req(res_data, input$choose_columns)
    
    idx <- match(as.character(input$choose_columns), as.character(res_data$names))
    req(!is.na(idx))
    
    p_tasu <- if(is.numeric(input$tasu) && input$tasu != 0) input$tasu else 3
    p_bai  <- if(is.numeric(input$bai)) input$bai else 1
    p_kakeru <- if(is.numeric(input$kakeru)) input$kakeru else 1
    
    raw_vals <- res_data$values
    scaled_matrix <- round(apply(as.matrix(raw_vals), 2, scale), 4)
    transformed <- scaled_matrix * p_kakeru + p_tasu
    
    final_vals <- transformed
    for (i in 1:ncol(transformed)) {
      final_vals[, i] <- transformed[, i] * p_bai / p_tasu
    }
    final_vals[final_vals < 0] <- 0
    
    n <- res_data$n_vars
    widths_all  <- final_vals[, c(1:n) * 2 - 1, drop = FALSE]
    heights_all <- final_vals[, c(1:n) * 2,     drop = FALSE]
    
    widths  <- as.numeric(widths_all[idx, ])
    heights <- as.numeric(heights_all[idx, ])
    
    xmax <- max(rowSums(widths_all, na.rm = TRUE), na.rm = TRUE)
    ymax <- max(heights_all, na.rm = TRUE)
    
    if (is.na(xmax) || xmax < 0.1) xmax <- 10
    if (is.na(ymax) || ymax < 0.1) ymax <- 5
    
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = c(0, xmax), ylim = c(0, ymax * 1.2), 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n",
         xaxs="i", yaxs="i")
    
    current_x <- 0
    for (i in 1:n) {
      img_path <- res_data$img_paths[i]
      w <- widths[i]
      h <- heights[i]
      
      if(w > 0.001 && h > 0.001){
        if(file.exists(img_path)){
          tryCatch({
            img <- png::readPNG(img_path, native = FALSE)
            rasterImage(img,
                        xleft = current_x, 
                        xright = current_x + w,
                        ybottom = 0, 
                        ytop = h,
                        interpolate = TRUE)
          }, error = function(e) {})
        }
      }
      current_x <- current_x + w
    }
  }
  
  
# 【描画関数2】拡張型漢字グラフ------------------------------
  create_plot2 <- function(input, res_data) {
    req(res_data, input$choose_columns)
    
    idx <- match(as.character(input$choose_columns), as.character(res_data$names))
    req(!is.na(idx))
    
    n <- res_data$n_vars
    full_data <- res_data$values
    
    if(input$header){
      scaled_vals <- scale(full_data[, c(1:n)*2-1])
      transformed <- scaled_vals * input$kakeru + input$tasu
      
      mojisize_all <- matrix(0, nrow=nrow(transformed), ncol=ncol(transformed))
      for(i in 1:ncol(transformed)){
        mojisize_all[,i] <- transformed[,i] * input$bai / as.numeric(input$tasu)
      }
      
      radar_vals <- full_data[idx, c(1:n)*2]
      maxdat <- rep(max(full_data[, c(1:n)*2]), n)
      mindat <- rep(0, n)
    } else {
      req(input$maxnum)
      radar_vals <- full_data[idx, c(1:n)*2]
      maxdat <- rep(max(full_data[, c(1:n)*2]), n)
      mindat <- rep(0, n)
      
      raw_vals_odd <- full_data[, c(1:n)*2-1]
      mojisize_all <- matrix(0, nrow=nrow(raw_vals_odd), ncol=ncol(raw_vals_odd))
      for(i in 1:ncol(raw_vals_odd)){
        mojisize_all[,i] <- raw_vals_odd[,i] * 15 / as.numeric(input$maxnum)
      }
    }
    
    current_sizes <- mojisize_all[idx, ]
    CONST_MAX_LIMIT <- 10 
    
    if(all(is.na(current_sizes))) {
      max_val_in_plot <- 0
    } else {
      max_val_in_plot <- max(current_sizes, na.rm = TRUE)
      if(is.infinite(max_val_in_plot)) max_val_in_plot <- 0
    }
    
    if(max_val_in_plot > CONST_MAX_LIMIT){
      scale_factor <- CONST_MAX_LIMIT / max_val_in_plot
      mojisize <- current_sizes * scale_factor
    } else {
      mojisize <- current_sizes
    }
    
    if(any(is.na(radar_vals))){
      radar_vals[is.na(radar_vals)] <- 0
    }
    
    plot_dat <- as.data.frame(rbind(maxdat, mindat, radar_vals))
    colnames(plot_dat) <- res_data$var_names
    
    req(nrow(plot_dat) >= 3)
    
    plot_dat <- plot_dat[, c(1, n:2)]
    
    par(mar = c(2, 2, 2, 2))
    plot(0, type="n", xlim=c(-3,3), ylim=c(-3,3), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
    
    par(new = TRUE)
    
    radarchart(plot_dat, axistype = 1, seg = 5, plty = 1, vlcex = 1, 
               axislabcol="black", centerzero = TRUE, 
               vlabels = rep(" ", n), 
               calcex = 0.8, pcol = 2, plwd = 3, 
               xlim=c(-2,2), ylim=c(-2,2))
    
    # vec <- c(0, c(1:n))
    # kaku <- pi/2 - 2*pi/n * vec
    # kaku <- kaku[-1]
    
    vec <- 0:(n - 1)
    kaku <- pi/2 - (2 * pi / n) * vec
    
    kanji <- res_data$kanji_chars
    base_radius <- 1.1 
    
    for(i in 1:length(kaku)){
      size <- if(!is.na(mojisize[i]) && mojisize[i] > 0) mojisize[i] else 0
      if(size > 0){
        offset <- 0.15 + (size * 0.02)
        pos_x <- (base_radius + offset) * cos(kaku[i])
        pos_y <- (base_radius + offset) * sin(kaku[i])
        text(x = pos_x, y = pos_y, labels = kanji[i], cex = size)
      }
    }
  }
  
  
# 出力・ダウンロード------------------------------
  output$plot2 <- renderPlot({
    res <- data_processed()
    create_plot1(input, res)
  })
  
  output$plot <- renderPlot({
    res <- data_processed()
    create_plot2(input, res)
  }, height = 700)
  
  output$out_down1 <- downloadHandler(
    filename = function() { paste("expanded_kanji", input$button1, sep = ".") },
    content = function(file) {
      res <- data_processed()
      if(input$button1 == "png") png(file, width=1000, height=1000)
      else if(input$button1 == "pdf") pdf(file, family="Japan1")
      else postscript(file, horizontal=FALSE, family="Japan1")
      create_plot2(input, res)
      dev.off()
    }
  )
  
  output$out_down2 <- downloadHandler(
    filename = function() { paste("kanji_graph", input$button2, sep = ".") },
    content = function(file) {
      res <- data_processed()
      if(input$button2 == "png") png(file, width=800, height=400)
      else if(input$button2 == "pdf") pdf(file, family="Japan1")
      else postscript(file, horizontal=FALSE, family="Japan1")
      create_plot1(input, res)
      dev.off()
    }
  )
  
  output$pca_result <- renderTable({
    res <- data_processed()
    head(res$raw, 20)
  })
  
  # output$face2 <- renderTable({
  #   req(input$data2)
  #   inFile3 <- input$data2
  #   validate(need(str_sub(inFile3$datapath, start = -3)=="csv", "CSVファイルを選択してください"))
  #   d2 <- tryCatch({
  #     read.csv(inFile3$datapath, header=TRUE, fileEncoding="CP932")
  #   }, error = function(e){
  #     read.csv(inFile3$datapath, header=TRUE, fileEncoding="UTF-8")
  #   })
  #   head(d2)
  # })
  # 
  # output$downloadData <- downloadHandler(
  #   filename = "newdata.csv",
  #   content = function(file) { write.csv(dataset$data33, file, row.names=FALSE) }
  # )
  
  
  #〇ローデータから解析用データを作成---------------------------
  output$face2 <- renderTable({
    req(input$data2)
    inFile3 <- input$data2
    req(str_sub(inFile3$datapath, start = -3)=="csv")
    
    # data222<-read.csv(inFile3$datapath, header=T, fileEncoding="CP932")
    data222 <- tryCatch({
      read.csv(inFile3$datapath, header=TRUE, fileEncoding="CP932")
    }, error = function(e){
      read.csv(inFile3$datapath, header=TRUE, fileEncoding="UTF-8")
    })
    
    data22<-data222[,-1]
    id<-data222[,1]
    
    clustinfo <- NULL
    
    if(input$group==1){
      clustinfo<-as.vector(data22[,ncol(data22)])
      data22<-data22[,-ncol(data22)]
      dataset$data44<-data222
      
    }else if(input$group==2 && !is.null(input$group)){
      req(input$clustnum)
      cluster.num<-as.numeric(input$clustnum) #クラスタ数
      validate(need(all(sapply(data22, is.numeric)), "クラスタリングには数値データが必要です"))
      # pmatrix<-scale(as.data.frame(data22)) #正規化
      k.result<-kmeans(data22,cluster.num)
      clustinfo<-k.result$cluster
      data22<-data22
      dataset$data44<-cbind(data222,cluster=clustinfo)
    }
    
    # temp5_all<-NULL
    # for(i in 1:length(unique(clustinfo))){
    #   temp5<-data22[which(clustinfo==i),]
    #   temp5_mean<-apply(temp5,2,mean)
    #   temp5_sd<-apply(temp5,2,sd)
    #   
    #   temp6<-NULL
    #   for(i in 1:ncol(data22)){
    #     temp6<-c(temp6,temp5_mean[i], temp5_sd[i])
    #   }
    #   temp5_all<-cbind(temp5_all,temp6)
    # }
    # 
    # id<-NULL
    # for(i in 1:ncol(temp5_all)){
    #   id<-c(id,paste("cluster",i))
    # }
    # 
    # dataset$data33<-cbind(id,t(temp5_all))
    # # dataset$data44<-cbind(data222,cluster=clustinfo)
    # as.data.frame(cbind(id,round(t(temp5_all),2)))
    temp5_all <- NULL
    if(!is.null(clustinfo)){
      uniq_clust <- sort(unique(clustinfo))
      for(k in uniq_clust){
        temp5 <- data22[which(clustinfo == k), , drop=FALSE]
        if(nrow(temp5) > 0){
          temp5_mean <- apply(temp5, 2, mean)
          temp5_sd <- apply(temp5, 2, sd)
          temp6 <- NULL
          for(j in 1:ncol(data22)){
            temp6 <- c(temp6, temp5_mean[j], temp5_sd[j])
          }
          temp5_all <- cbind(temp5_all, temp6)
        }
      }
    }
    
    res_id <- NULL
    if(!is.null(temp5_all)){
      for(i in 1:ncol(temp5_all)){
        res_id <- c(res_id, paste("cluster", i))
      }
      dataset$data33 <- cbind(id = res_id, t(temp5_all))
      as.data.frame(cbind(id = res_id, round(t(temp5_all), 2)))
    }
    
    
  })
  
  #ダウンロードボタン---------------------------
  output$downloadData = downloadHandler(
    filename = "newdata.csv",
    content = function(file) {
      write.csv(dataset$data33, file, row.names=FALSE, fileEncoding = "CP932")
    }
  )
  output$downloadData2 = downloadHandler(
    filename = "clusterresultdata.csv",
    content = function(file) {
      write.csv(dataset$data44, file, row.names=FALSE, fileEncoding = "CP932")
    }
  )
  
  
  #確認用-------------------------------
  output$face <- renderPrint({
    res <- data_processed()
    res$n_vars

  })
  
  
  
  
})