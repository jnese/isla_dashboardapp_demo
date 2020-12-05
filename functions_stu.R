
stu_timelost_plot_fx <- function(input){
  stu_dta <- dta %>% 
    filter(student_name == input$student_name,
           date >= input$stu_daterange[[1]],
           date <= input$stu_daterange[[2]]) 
  
  date_breaks_stu <- unique(stu_dta$date)
  
  stu_dta %>%
    group_by(date) %>% 
    summarize(time_lost = sum(time_lost, na.rm = TRUE),
              time_lost = ifelse(is.na(time_lost), 0, time_lost),
              n = n()) %>%    
    ggplot(aes(date, time_lost)) +
    geom_vline(xintercept = as_date(input$intervention1), color = "red", size = 1.5) +
    geom_col() +
    geom_text(aes(label = n), size = 8, position = position_dodge(width = 0.9), vjust = -0.25, color = "cornflowerblue") +
    theme_minimal(base_size = 24) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5),
          plot.title = element_text(color = "cornflowerblue")) +
    ggthemes::scale_fill_colorblind() +
    scale_x_date(breaks = date_breaks_stu,
                 date_labels = "%b %d") +
    labs(x = "",
         y = "Minutes",
         title = "Number of visits each date")
}

make_time <- function(x){
  format(strptime(strftime(x, format = "%H:%M", tz = "UTC"), format = "%H:%M"), "%I:%M %p")
}

stu_timelost_table_fx <- function(input){
  
  stu_dta <- dta %>%
    filter(student_name == input$student_name,
          date >= input$stu_daterange[[1]],
          date <= input$stu_daterange[[2]])
  
  if (as_date(input$intervention1) %in% stu_dta$date) {
    stu_dta <- stu_dta %>% 
      mutate(hiddenColumn = ifelse(date == input$intervention1, 1, 0))
  } else {
    stu_dta <- stu_dta %>% 
      mutate(hiddenColumn = ifelse(date <= input$intervention1, 0, 1)) %>% 
      group_by(hiddenColumn) %>% 
      mutate(hiddenColumn = ifelse(hiddenColumn == 0 & row_number() == n(), 1, 0))
  }
  
  stu_dta %>% 
    select(date, hiddenColumn, Teacher = teacher_name, `Time in` = time_in, `Time out` = time_out, `Mins Lost` = time_lost, `What Happened` = what_happened) %>% 
    mutate_at(c("Time in", "Time out"), ~make_time(.x)) %>% 
 #   mutate(hiddenColumn = ifelse(date <= input$intervention1, 0, 1)) %>% 
 #   group_by(hiddenColumn) %>% 
 #   mutate(hiddenColumn = ifelse(hiddenColumn == 0 & row_number() == n(), 1, 0)) %>% 
 #   ungroup() %>% 
    group_by(date) %>%
    mutate(`Mins Lost` = round(`Mins Lost`, 0),
           n = n(),
           occasion = row_number(),
           date = format(date,"%b %d"),
           date = ifelse(n == 1, date, paste0(date, " #", occasion))) %>% 
    select(-c(n, occasion)) %>% 
    
    datatable(.,
              caption = input$student_name,              ###################################
              extensions = "Buttons",                    ###################################
              options = 
                list( 
                  columnDefs = list(
                    list(visible = FALSE, targets = ncol(.))),
                  dom = "Blfrtip",             ###################################
                  buttons =                   ###################################
                    list(
                      # list(extend = 'collection',
                      #   buttons = c('csv', 'excel', 'pdf'),
                      #   filename = paste("Foofoofoofoo", input$student_name, sep = "-"),
                      #   text = 'Download')
                      list(extend = 'csv', 
                           filename = paste0("Minutes_in_Room_", input$student_name, "_", input$stu_daterange[[1]], "_to_", input$stu_daterange[[2]])),
                      list(extend = 'pdf', 
                           filename = paste0("Minutes_in_Room_", input$student_name, "_", input$stu_daterange[[1]], "_to_", input$stu_daterange[[2]]))
                    )
                #lengthMenu = list( c(10, 20, -1), # declare values
                #                   c(10, 20, "All") # declare titles
            )
    ) %>% 
    formatStyle(0:6, valueColumns = "hiddenColumn", 
              `border-bottom` = styleEqual(1, "solid red 3px"))
}


stu_fidelity_plot_fx <- function(input){
  
  stu_dta <- dta %>% 
    filter(student_name == input$student_name,
           date >= input$stu_daterange[[1]],
           date <= input$stu_daterange[[2]])  

  tx_rows <- 
    if (as_date(input$intervention1) %in% stu_dta$date) {
      which(stu_dta$date == input$intervention1)
    } else {
      stu_dta %>% 
        mutate(rown = row_number() + .5,
               tx = ifelse(date <= input$intervention1, 0, 1)) %>% 
        group_by(tx) %>% 
        mutate(tx = ifelse(tx == 0 & row_number() == n(), 1, 0)) %>% 
        filter(tx == 1) %>% 
        pull(rown)
    }

  plot_dta <- stu_dta %>%
    select(date, isla_debrief_completed:did_the_student_do_the_re_connection_conversation_with_their_teacher) %>%
    group_by(date) %>% 
    mutate(n = n(),
           occasion = row_number()) %>% 
    pivot_longer(
      cols = contains("_"),
      names_to = "fidelity",
      values_to = "Completed"
    ) %>% 
    mutate(Completed = ifelse(is.na(Completed), NA_character_, Completed),
           Completed = fct_relevel(Completed, "Yes", "No"),
           date_2 = format(date,"%b %d"),
           date_2 = ifelse(n == 1, date_2, paste0(date_2, " #", occasion)),
           date_2 = fct_reorder(date_2, date),
           fidelity = fct_relevel(fidelity, "did_the_student_do_the_re_connection_conversation_with_their_teacher",
                                  "did_the_student_complete_a_re_connection_card",
                                  "practiced_re_connection_conversation_with_you",
                                  "coached_student_on_appropriate_behavior_skill"),
           fidelity = recode(fidelity,
                             isla_debrief_completed = "Debrief \nCompleted",
                             coached_student_on_appropriate_behavior_skill = "Coached Student on \nAppropriate Behavior Skill",
                             practiced_re_connection_conversation_with_you = "Practiced \nRe-Connection Conversation",
                             did_the_student_complete_a_re_connection_card = "Completed \nRe-Connection Card",
                             did_the_student_do_the_re_connection_conversation_with_their_teacher = "Re-Connection Conversation \nwith Teacher"))
  
    percs <- plot_dta %>% 
      group_by(fidelity) %>% 
      summarize(Percent = round(sum(Completed == "Yes", na.rm = TRUE)/n()*100, 0),
                Percent = paste0(Percent, "%")) %>% 
      mutate(x = length(unique(plot_dta$date_2)) + 1)
  
    plot_dta %>% 
      ggplot(aes(date_2, factor(fidelity))) +
      geom_tile(aes(fill = Completed), color = "black") +
      geom_vline(xintercept = tx_rows, color = "red", size = 1.5) +
      geom_text(data = percs, aes(x, fidelity, label = Percent), size = 8) +
      scale_fill_manual(values = c("#009E73", "gray70", "white")) +
      scale_x_discrete(limits = c(levels(plot_dta$date_2), " ")) +
      annotate("text", x = " ", y = 5.5, label = "Percent Yes", size = 3) +
      theme_minimal(base_size = 24) +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom") +
      labs(x = "",
           y = "")
}


stu_fidelity_table_fx <- function(input){
  
  stu_dta <- dta %>%
    filter(student_name == input$student_name,
           date >= input$stu_daterange[[1]],
           date <= input$stu_daterange[[2]])
  
  if (as_date(input$intervention1) %in% stu_dta$date) {
    stu_dta <- stu_dta %>% 
      mutate(hiddenColumn = ifelse(date == input$intervention1, 1, 0))
  } else {
    stu_dta <- stu_dta %>% 
      mutate(hiddenColumn = ifelse(date <= input$intervention1, 0, 1)) %>% 
      group_by(hiddenColumn) %>% 
      mutate(hiddenColumn = ifelse(hiddenColumn == 0 & row_number() == n(), 1, 0))
  }
  
  stu_dta %>% 
    select(date, `Mins Lost` = time_lost, 
           `Safety Risk` = does_students_behavior_pose_a_safety_risk_to_them_or_others,
           `Sent to Administrator` = student_sent_to_administrator,
           `Re-Entered Classroom` = did_the_student_reenter_the_classroom,
           `Why No Re-Entry` = if_not_why,
           hiddenColumn) %>% 
    group_by(date) %>% 
    mutate(`Mins Lost` = round(`Mins Lost`, 0),
           n = n(),
           occasion = row_number(),
           date = format(date,"%b %d"),
           date = ifelse(n == 1, date, paste0(date, " #", occasion))) %>% 
    select(-c(n, occasion)) %>% 
    datatable(.,
              caption = input$student_name,              ###################################
              extensions = "Buttons",                    ###################################
              options = 
                list( 
                  columnDefs = list(
                    list(visible = FALSE, targets = ncol(.))),
                  dom = "Blfrtip",             ###################################
                  buttons =                   ###################################
                  list(
                    # list(extend = 'collection',
                    #   buttons = c('csv', 'excel', 'pdf'),
                    #   filename = paste("Foofoofoofoo", input$student_name, sep = "-"),
                    #   text = 'Download')
                    list(extend = 'csv', 
                         filename = paste0("Re-Enter_Classroom_", input$student_name, "_", input$stu_daterange[[1]], "_to_", input$stu_daterange[[2]])),
                    list(extend = 'pdf', 
                         filename = paste0("Re-Enter_Classroom_", input$student_name, "_", input$stu_daterange[[1]], "_to_", input$stu_daterange[[2]]))
                  )
                  #lengthMenu = list( c(10, 20, -1), # declare values
                  #                   c(10, 20, "All") # declare titles
                )
    ) %>% 
    formatStyle(0:6, valueColumns = "hiddenColumn", 
                `border-bottom` = styleEqual(1, "solid red 3px")) %>% 
    formatStyle("Safety Risk",
              backgroundColor = styleEqual(c("Yes"), c("firebrick")))
}

stu_totalincidents_box_fx <- function(input){
  
  dta %>%
    filter(student_name == input$student_name,
           date >= input$stu_daterange[[1]],
           date <= input$stu_daterange[[2]]) %>% 
    nrow()
}

stu_totalminslost_box_fx <- function(input){
  
  dta %>%
    filter(student_name == input$student_name,
           date >= input$stu_daterange[[1]],
           date <= input$stu_daterange[[2]]) %>% 
    summarize(minslost = round(sum(time_lost, na.rm = TRUE)), 0) %>% 
    pull(minslost)
}

downloadplot_stu_fx <- function(plotname, input, fx, inputstudentname, inputtime){
  downloadHandler(
    filename = function() {
      paste0(plotname, "_", inputstudentname, "_", inputtime[[1]], "_to_", inputtime[[2]], ".png")
    },
    content = function(file) {
      ggsave(file, fx(input), device = "png", width = 9, height = 6, dpi = 300)
    }
  )
}


