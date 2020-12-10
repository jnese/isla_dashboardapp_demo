
sch_totalincidents_box_fx <- function(input){
  
  if (input$grade_level == "All") {
    dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      pull(student_name) %>%
      length() %>% 
      formatC(., big.mark=",")
  } else {
    dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      pull(student_name) %>%
      length() %>% 
      formatC(., big.mark=",")
  }
}

sch_totalstudents_box_fx <- function(input){
  
  if (input$grade_level == "All") {
    dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      distinct(student_name) %>% 
      nrow()
  } else {
    dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      distinct(student_name) %>% 
      nrow()
  }
}

sch_totalhourslost_box_fx <- function(input){
  
  if (input$grade_level == "All") {
    dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      summarize(totalhours = round(sum(time_lost, na.rm = TRUE)/60, 0)) %>% 
      pull(totalhours) %>% 
      formatC(., big.mark=",")
  } else {
    dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      summarize(totalhours = round(sum(time_lost, na.rm = TRUE)/60, 0)) %>% 
      pull(totalhours) %>% 
      formatC(., big.mark=",")
  }
}

sch_aveminslost_box_fx <- function(input){
  
  if (input$grade_level == "All") {
    dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      summarize(avemins = round(mean(time_lost, na.rm = TRUE), 0)) %>% 
      pull(avemins)
  } else {
    dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>%
      summarize(avemins = round(mean(time_lost, na.rm = TRUE), 0)) %>% 
      pull(avemins)
  }
}

sch_incidstu_plot_fx <- function(input){
  
  if (input$grade_level == "All") {
    top_students <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>% 
      mutate(student_name = str_trim(student_name, side = c("left"))) %>% 
      group_by(student_name) %>% 
      summarize(Incidents = n()) %>% 
      arrange(-Incidents) %>% 
      slice(1:10)
  } else {
    top_students <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>% 
      mutate(student_name = str_trim(student_name, side = c("left"))) %>% 
      group_by(student_name) %>% 
      summarize(Incidents = n()) %>% 
      arrange(-Incidents) %>% 
      slice(1:10)
  }
  
top_students %>% 
  ggplot(aes(fct_reorder(factor(student_name), Incidents), Incidents)) +
  geom_bar(stat = "identity", fill = "#009E73") +
  coord_flip() +
  theme_minimal(base_size = 24) +
#  scale_y_continuous(breaks = seq(0, max(top_students$Incidents) + 2, 2)) +
  labs(x = "",
         y = "Number of Incidents")
}


sch_incidtch_plot_fx <- function(input){
  
  if (input$grade_level == "All") {
    top_teachers <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>% 
      group_by(teacher_name) %>% 
      summarize(Incidents = n()) %>% 
      arrange(-Incidents) %>% 
      mutate(fillr = ifelse(teacher_name == "Missing", "two", "one")) %>%
      slice(1:10)
  } else {
    top_teachers <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>% 
      group_by(teacher_name) %>% 
      summarize(Incidents = n()) %>% 
      arrange(-Incidents) %>% 
      mutate(fillr = ifelse(teacher_name == "Missing", "two", "one")) %>%
      slice(1:10)
  }
  
  top_teachers %>% 
    ggplot(aes(fct_reorder(factor(teacher_name), Incidents), as.double(Incidents), fill = fillr)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal(base_size = 24) +
    scale_fill_manual(values = c("#009E73", "gray")) +
    theme(legend.position = "none") +
#    scale_y_continuous(breaks = seq(0, max(top_teachers$Incidents) + 2, 2)) +
    labs(x = "",
         y = "Number of Incidents")
}


sch_relationships_table_fx <- function(input){
  
  if (input$grade_level == "All") {
    top_teachers <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>% 
      mutate(student_name = str_trim(student_name, side = "left")) %>% 
      group_by(student_name, teacher_name) %>% 
      summarize(Incidents = n()) %>% 
      arrange(-Incidents) %>% 
      rename(`Student` = student_name, `Teacher` = teacher_name) %>% 
      datatable(., filter = "top", options = list(dom = "t"))
  } else {
    top_teachers <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) %>% 
      mutate(student_name = str_trim(student_name, side = "left")) %>% 
      group_by(student_name, teacher_name) %>% 
      summarize(Incidents = n()) %>% 
      arrange(-Incidents) %>% 
      rename(`Student` = student_name, `Teacher` = teacher_name) %>% 
      datatable(., filter = "top", options = list(dom = "t"))
  }
  
}

sch_fidelity_plot_fx <- function(input){
  
  if (input$grade_level == "All") {
    isla_dta <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) 
  } else {
    isla_dta <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]])
  }
  
    isla_qs <- isla_dta %>% 
      select(isla_debrief_completed,
             coached_student_on_appropriate_behavior_skill,
             practiced_re_connection_conversation_with_you,
             did_the_student_complete_a_re_connection_card,
             did_the_student_do_the_re_connection_conversation_with_their_teacher
      ) %>% 
      gather(ISLA, value) %>% 
      mutate(ISLA = recode(ISLA,
                           isla_debrief_completed = "Debrief \nCompleted",
                           coached_student_on_appropriate_behavior_skill = "Coached Student on \nAppropriate Behavior Skill",
                           practiced_re_connection_conversation_with_you = "Practiced \nRe-Connection Conversation",
                           did_the_student_complete_a_re_connection_card = "Completed \nRe-Connection Card",
                           did_the_student_do_the_re_connection_conversation_with_their_teacher = "Re-Connection Conversation \nwith Teacher")) %>% 
      replace_na(list(value = "Missing"))
    
    if (length(unique(isla_qs$value)) == 3) {
      fill_color <- c("white", "gray70", "#009E73")
    } else {
      fill_color <- c("gray70", "#009E73")
    }
    
    isla_qs %>% 
      group_by(ISLA) %>% 
      summarize(Yes = sum(value == "Yes", na.rm = TRUE)/n(),
                No = sum(value == "No", na.rm = TRUE)/n(),
                Missing = sum(value == "Missing", na.rm = TRUE)/n()) %>% 
      pivot_longer(
        cols = -1,
        names_to = "cats",
        values_to = "percent"
      ) %>% 
      ggplot(aes(ISLA, y = percent, fill = fct_relevel(factor(cats), "Missing", "No"))) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(ISLA, percent, label = paste0(round(percent*100, 0), "%")), size = 8, position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 24) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = fill_color) +
      labs(y = paste0("Percent of ", nrow(isla_dta), " Incidents"),
           x = "",
           fill = "")
}

sch_reenter_plot_fx <- function(input){
  
  if (input$grade_level == "All") {
    reenter_dta <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) 
  } else {
    reenter_dta <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]])
  }
  
  enter_qs <- reenter_dta %>% 
    select(did_the_student_reenter_the_classroom) %>% 
    gather(qs, value) %>% 
    mutate(qs = recode(qs,
                       did_the_student_reenter_the_classroom = "Student Re-enter the Classroom?"),
           value = ifelse(is.na(value), "Missing", value)) 
  
  if (length(unique(enter_qs$value)) == 3) {
    fill_color <- c("white", "gray70", "#009E73")
  } else {
    fill_color <- c("gray70", "#009E73")
  }
  
  enter_qs %>% 
    group_by(qs) %>% 
    summarize(Yes = sum(value == "Yes", na.rm = TRUE)/n(),
              No = sum(value == "No", na.rm = TRUE)/n(),
              Missing = sum(value == "Missing", na.rm = TRUE)/n()) %>% 
    pivot_longer(
      cols = -1,
      names_to = "cats",
      values_to = "percent"
    ) %>% 
    filter(percent > 0) %>% 
    ggplot(aes(qs, y = percent, fill = cats)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(qs, percent, label = paste0(round(percent*100, 0), "%")), size = 8, position = position_stack(vjust = 0.5)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal(base_size = 24) +
    theme(legend.position = "bottom",
          axis.text.y = element_blank()) +
    scale_fill_manual(values = fill_color) +
    labs(y = paste0("Percent of ", nrow(enter_qs), " Incidents"),
         x = "",
         fill = "")
  
}


sch_whynoreentry_plot_fx <- function(input){
  
  if (input$grade_level == "All") {
    why_dta <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) 
  } else {
    why_dta <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]])
  }
  
  why <- why_dta %>% 
    filter(did_the_student_reenter_the_classroom == "No") %>% 
    select(if_not_why) %>% 
    gather(why, reason) %>% 
    mutate(reason = ifelse(is.na(reason), "Missing", reason),
           reason = fct_infreq(reason)) %>% 
    janitor::tabyl(reason) %>% 
    as_tibble()
  
  why %>% 
    mutate(fillr = ifelse(reason == "Missing", "two", "one")) %>% 
    ggplot(aes(percent, fct_rev(reason), fill = fillr)) +
    geom_bar(stat = "identity") +
    geom_text(aes(percent, fct_rev(reason), label = paste0(round(percent*100, 0), "%")), size = 8, position = position_stack(vjust = 0.5)) +
    theme_minimal(base_size = 24) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_fill_manual(values = c("#009E73", "gray70")) +
    theme(legend.position = "none") +
    labs(x = paste0("Percent of ", sum(why$n), " Not Returned to Class"),
         y = "")
  
}


sch_adminsafety_plot_fx <- function(input){
  
  if (input$grade_level == "All") {
    admin_dta <- dta %>%
      filter(date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]]) 
  } else {
    admin_dta <- dta %>%
      filter(grade == input$grade_level,
             date >= input$sch_daterange[[1]],
             date <= input$sch_daterange[[2]])
  }
  
  admin <- admin_dta %>% 
    select(does_students_behavior_pose_a_safety_risk_to_them_or_others,
           student_sent_to_administrator
    ) %>% 
    gather(qs, value) %>% 
    mutate(value = recode(value, "no" = "No"),
           qs = recode(qs,
                       does_students_behavior_pose_a_safety_risk_to_them_or_others = "Student's Behavior a Safety Risk?",
                       student_sent_to_administrator = "Student Sent to Administrator?")) %>%  
    mutate(value = ifelse(is.na(value), "Missing", value),
           value = fct_infreq(value)) %>% 
    group_by(qs) %>% 
    summarize(Yes = sum(value == "Yes", na.rm = TRUE)/n(),
              No = sum(value == "No", na.rm = TRUE)/n(),
              Missing = sum(value == "Missing", na.rm = TRUE)/n()) %>% 
    pivot_longer(
      cols = -1,
      names_to = "cats",
      values_to = "prop"
    )
  
    admin %>% 
      ggplot(aes(prop, qs, fill = cats)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(prop, qs, label = paste0(round(prop*100, 0), "%")), size = 8, position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_minimal(base_size = 24) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("white", "gray70", "firebrick")) +
      labs(x = paste0("Proportion of ", nrow(admin_dta), " Incidents"),
           y = "",
           fill = "")
  
}
################ Start here
# also, see if you can have different sidebar for school-Time

sch_incidents_time_plot_fx <- function(input){
  
  if (input$sch_incidents_plotselect == "Monthly") {
    
    if (input$grade_level == "All") {
      inc_mo_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_mo_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_mo_dta %>% 
      mutate(month = month(date, label = TRUE, abbr = TRUE)) %>% 
      group_by(month) %>% 
      count() %>%
      ungroup() %>% 
      mutate(month = ifelse(is.na(month), "Missing", as.character(month)),
             month = fct_relevel(month, "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Missing")) %>% 
      mutate(fillr = ifelse(month == "Missing", "gray70", "#009E73")) %>% 
      ggplot(aes(fct_drop(month), n, fill = fillr)) +
      geom_col() +
      theme_minimal(base_size = 24) +
      scale_fill_identity() +
      theme(legend.position = "none") +
      labs(x = "",
           y = "Number of Students in Room")
    
  } else if (input$sch_incidents_plotselect == "Daily") {
    
    if (input$grade_level == "All") {
      inc_day_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_day_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_day_dta %>% 
      drop_na(date) %>%
      group_by(date) %>% 
      count() %>% 
      mutate(fillr = ifelse(is.na(date), "gray70", "#009E73")) %>% 
      ggplot(aes(x = date, y = n, fill = fillr)) +
      geom_col() +
      theme_minimal(base_size = 24) +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
      scale_fill_identity() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 2, hjust = 1)) +
      labs(x = "",
           y = "Number of Students in Room") 
    
  } else if (input$sch_incidents_plotselect == "Time of day") {
    
    if (input$grade_level == "All") {
      inc_hour_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_hour_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_hour_dta %>% 
      mutate(entry_hour = paste0(as.character(hour(anytime::anytime(time_in))), "am"),
             entry_hour = recode(entry_hour,
                                 "12am" = "12pm",
                                 "13am" = "1pm",
                                 "14am" = "2pm",
                                 "15am" = "3pm",
                                 "16am" = "4pm",
                                 "NAam" = "Missing"),
             entry_hour = as_factor(entry_hour),
             entry_hour = fct_relevel(entry_hour, "7am", "8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm")) %>% 
      group_by(entry_hour) %>%
      count() %>%
      mutate(fillr = ifelse(entry_hour == "Missing", "gray70", "#009E73")) %>%
      ggplot(aes(entry_hour, n, fill = fillr)) +
      geom_col() +
      theme_minimal(base_size = 24) +
      scale_fill_identity() +
      theme(legend.position = "none") +
      labs(x = "",
           y = "Number of Students in Room")
    
  } else {
    
    if (input$grade_level == "All") {
      inc_dow_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_dow_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_dow_dta %>% 
      mutate(weekday = wday(date, label = TRUE, abbr = TRUE),
             weekday = ifelse(is.na(weekday), "Missing", as.character(weekday)),
             weekday = fct_relevel(weekday, "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Missing")) %>% 
      group_by(weekday) %>% 
      count() %>% 
      mutate(fillr = ifelse(weekday == "Missing", "gray70", "#009E73")) %>% 
      ggplot(aes(weekday, n, fill = fillr)) +
      geom_col() +
      theme_minimal(base_size = 24) +
      scale_fill_identity() +
      theme(legend.position = "none") +
      labs(x = "",
           y = "Number of Students in Room")
    
  }
}

sch_minslost_time_plot_fx <- function(input){
  
  if (input$sch_minslost_plotselect == "Monthly") {
    
    if (input$grade_level == "All") {
      inc_mo_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_mo_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_mo_dta %>% 
      mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
      mutate(month = ifelse(is.na(month), "Missing", as.character(month)),
             month = fct_relevel(month, "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Missing")) %>% 
      group_by(month) %>% 
      summarize(mean_minutes = mean(time_lost, na.rm = TRUE)) %>% 
      drop_na() %>% 
      mutate(fillr = ifelse(month == "Missing", "gray70", "#009E73")) %>%
      ggplot(aes(fct_drop(month), mean_minutes, fill = fillr)) +
      geom_col() +
      scale_fill_identity() +
      theme_minimal(base_size = 24) +
      labs(x = "",
           y = "Average Minutes in Room")
    
  } else if (input$sch_minslost_plotselect == "Daily") {
    
    if (input$grade_level == "All") {
      inc_day_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_day_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_day_dta %>% 
      group_by(date) %>% 
      summarize(mean_minutes = mean(time_lost, na.rm = TRUE)) %>% 
      ggplot(aes(date, mean_minutes)) +
      geom_col(fill = "#009E73") +
      theme_minimal(base_size = 24) +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 2, hjust = 1)) +
      labs(x = "",
           y = "Average Minutes in Room")
    
  } else if (input$sch_minslost_plotselect == "Time of day") {
    
    if (input$grade_level == "All") {
      inc_hour_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_hour_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_hour_dta %>% 
      mutate(entry_hour = paste0(as.character(hour(anytime::anytime(time_in))), "am"),
             entry_hour = recode(entry_hour,
                                 "12am" = "12pm",
                                 "13am" = "1pm",
                                 "14am" = "2pm",
                                 "15am" = "3pm",
                                 "16am" = "4pm",
                                 "NAam" = "Missing"),
             entry_hour = as_factor(entry_hour),
             entry_hour = fct_relevel(entry_hour, "7am", "8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm")) %>% 
      group_by(entry_hour) %>% 
      summarize(mean_minutes = mean(time_lost, na.rm = TRUE)) %>%
      drop_na() %>% 
      mutate(fillr = ifelse(entry_hour == "Missing", "gray70", "#009E73")) %>%
      ggplot(aes(entry_hour, mean_minutes, fill = fillr)) +
      geom_col(fill = "#009E73") +
      theme_minimal(base_size = 24) +
      scale_fill_identity() +
      theme(legend.position = "none") +
      labs(x = "",
           y = "Average Minutes in Room")
    
  } else {
    
    if (input$grade_level == "All") {
      inc_dow_dta <- dta %>%
        filter(date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]]) 
    } else {
      inc_dow_dta <- dta %>%
        filter(grade == input$grade_level,
               date >= input$sch_daterange[[1]],
               date <= input$sch_daterange[[2]])
    }
    
    inc_dow_dta %>% 
      mutate(weekday = wday(date, label = TRUE, abbr = TRUE),
             weekday = ifelse(is.na(weekday), "Missing", as.character(weekday)),
             weekday = fct_relevel(weekday, "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Missing")) %>% 
      group_by(weekday) %>% 
      summarize(mean_minutes = mean(time_lost, na.rm = TRUE)) %>%
      drop_na() %>% 
      mutate(fillr = ifelse(weekday == "Missing", "gray70", "#009E73")) %>% 
      ggplot(aes(weekday, mean_minutes, fill = fillr)) +
      geom_col(fill = "#009E73") +
      theme_minimal(base_size = 24) +
      scale_fill_identity() +
      theme(legend.position = "none") +
      labs(x = "",
           y = "Average Minutes in Room")
    
  }
}

downloadplot_sch_fx <- function(plotname, input, fx, inputgrade, inputtime){
  downloadHandler(
    filename = function() {
      paste0(plotname, "_grade", inputgrade, "_", inputtime[[1]], "_to_", inputtime[[2]], ".png")
    },
    content = function(file) {
      ggsave(file, fx(input), device = "png", width = 9, height = 6, dpi = 300)
    }
  )
}  
 

  