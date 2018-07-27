library(combinat)
library(gtools)
library(xlsx)

divisors <- function(x) {
  y <- seq_len(x)
  returnValue(y[x%%y == 0])
}

sum_combinations <- function(x) {
  sum_to_x <- seq_len(x)
  x_x_combn <- c(numeric(0))
  
  for (i in sum_to_x) {
    x_i_combn <- combinations(x, i, repeats.allowed = TRUE) 
    x_i_combn <- subset(x_i_combn, rowSums(x_i_combn) == x)
    rev_x_i_combn <- rev(x_i_combn)
    dim(rev_x_i_combn) <- dim(x_i_combn)
    x_i_combn <- rbind(x_i_combn, rev_x_i_combn)
    #x_i_combn <- x_i_combn[!duplicated.matrix(x_i_combn)]
    x_x_combn <- append(x_x_combn, list(x_i_combn))
  }
  
  returnValue(x_x_combn)
}

# apply_known_transfer <- function(which_r_naan, which_d_naan, known_amt){
#   returnValue( list( c(which_r_naan, whic_d_naan)  , known_amt))
# }


solve_alloc_matrix <- function(s_disbursed, s_receipt, any_known_info = data.frame()) {
  s_disbursed <- as.numeric(s_disbursed)
  s_receipt <- as.numeric(s_receipt)
  
  r_naan <- length(s_receipt)
  d_naan <- length(s_disbursed)
  
  alloc_matrix <- rep(0, r_naan * d_naan)
  dim(alloc_matrix) <- c(r_naan, d_naan)
  
  my_solver_x_series <- sort(rep( 1:r_naan, d_naan))
  my_solver_y_series <- rep(1:d_naan, r_naan)
  
  
  #apply known 
  # assert that known info is possible under the constraint
  if (nrow(any_known_info) > 0) {
    print('Applying known transactions between disbursed and receiving naans.')
    print('Caveat: Make sure your known transactions do not conflict with the total disbursment/receipt amount per naan group.')
    for (i in 1:nrow(any_known_info)) {
      alloc_matrix[any_known_info[i,]$which_r_naan, any_known_info[i,]$which_d_naan] <- any_known_info[i,]$known_amount
    }
    print(alloc_matrix)
  } 
  
  
  my_solver_x <- 1
  my_solver_y <- 1
  
  
  print("Traceback: Five conditions should meet simultaneously at all times.")
  cond_0 <- paste("#1 Subscript-Bound Constraint: ",my_solver_x, "less than / equal ",length(s_receipt), "and",
                  my_solver_y, "less than / equal ",length(s_disbursed))
  cond_1 <- paste("#2 Row Sum Constraint: ", alloc_matrix[my_solver_x, my_solver_y], "less than / equal", s_receipt[my_solver_x])
  cond_2 <- paste("#3 Column Sum Constraint: ", alloc_matrix[my_solver_x, my_solver_y], "less than / equal", s_disbursed[my_solver_y])
  cond_3 <- paste("#4 Current Row Sum: ", sum(alloc_matrix[my_solver_x,]), "less than / equal ", s_receipt[my_solver_x])
  cond_4 <- paste("#5 Current Column Sum: ",sum(alloc_matrix[,my_solver_y]), "less than / equal ", s_disbursed[my_solver_y])
  
  #try out the scenarios
  # while (   (my_solver_x <= length(s_receipt) && my_solver_y <= length(s_disbursed)
  #           && alloc_matrix[my_solver_x, my_solver_y] <= s_receipt[my_solver_x]
  #           && alloc_matrix[my_solver_x, my_solver_y] <= s_disbursed[my_solver_y]
  #           && sum(alloc_matrix[my_solver_x,]) <= s_receipt[my_solver_x]
  #           && sum(alloc_matrix[,my_solver_y]) <= s_disbursed[my_solver_y])
  #         ) {
  
  for ( i in 1:length(my_solver_x_series)  ) {
    
    my_solver_x <- my_solver_x_series[i]
    my_solver_y <- my_solver_y_series[i]
    
    # print(cond_0)
    # print(cond_1)
    # print(cond_2)
    # print(cond_3)
    # print(cond_4)
    # print("End of conditions")
    
      
    print(paste("Calculating payment at the following coordinates (x, y):", my_solver_x, my_solver_y, sep = " "))
      
    #print (paste("The math: Minimize ",(s_disbursed[my_solver_y] - sum(alloc_matrix[,my_solver_y])), ", ",
    #        (s_receipt[my_solver_x] - sum(alloc_matrix[my_solver_x,]) ) ))
      
      if ( any(any_known_info$which_r_naan == my_solver_x & any_known_info$which_d_naan == my_solver_y) ) {
        alloc_matrix[my_solver_x, my_solver_y] <- alloc_matrix[my_solver_x, my_solver_y]
      }
      else
      {
        alloc_matrix[my_solver_x, my_solver_y] <- 
          min( c(s_disbursed[my_solver_y] - sum(alloc_matrix[,my_solver_y]),
                 s_receipt[my_solver_x] - sum(alloc_matrix[my_solver_x,]) ))
      }
       
      # if (my_solver_x < length(s_receipt)) {
      #   my_solver_x <- my_solver_x + 1
      # }
      # 
      # if ( my_solver_y == length(s_disbursed)  && my_solver_x < length(s_receipt)) {
      #   my_solver_y <- 1
      # }
      # else {
      #   my_solver_y <- my_solver_y + 1
      # }
      # if ( my_solver_y == length(s_disbursed)  && my_solver_x == length(s_receipt)) {
      #   break
      # }
      
      
      
      #print(paste("Just Evaluated x,y at ", my_solver_x, my_solver_y, " "))
      
  }
  print("Disbursed from NAAN: ")
  print(c(s_disbursed))
  print("Receipts going to NAAN: ")
  print(c(s_receipt))
  returnValue(alloc_matrix)
}
#use a lot of memory for high value m2m and naan lists. 7860 MB.
#disbursed <- sum_combinations(m2m_total)[[d_naan]]
#receipts <- sum_combinations(m2m_total)[[r_naan]]
#scen_a_receipt <- receipts[5,]
#scen_a_disbursed <- disbursed[10,]

write_to_excel <- function() {
  #name_of_file <- orig_name_of_file
  
  # if (name_of_file %in% list.files()) {
  #   write_to_excel_counter <- write_to_excel_counter + 1
  #   name_of_file <- paste0( "0",as.character(write_to_excel_counter),"_", orig_name_of_file) 
  # }
  
    wb <- createWorkbook()  
    
    sheet_source<- createSheet(wb, sheetName = paste("Allocated for Source No.", R_naan_data$SOURCE[1]))
    sheet_disburse<- createSheet(wb, sheetName = "Disbursing_NAANs (TRAN_ALL_VW)")
    sheet_receive<- createSheet(wb, sheetName = "Receiving_NAANs (TRAN_ALL_VW)")
    
    
    addDataFrame(as.data.frame(paste('Source', R_naan_data$SOURCE[1])), sheet = sheet_source, 
                 col.names = FALSE, row.names = FALSE, startRow = 1, startColumn = 1)
    addDataFrame(as.data.frame('Disbursements'), sheet = sheet_source, 
                 col.names = FALSE, row.names = FALSE, startRow = 1, startColumn = 3)
    addDataFrame(as.data.frame('Receipts'), sheet = sheet_source, 
                 col.names = FALSE, row.names = FALSE, startRow = 3, startColumn = 1)
    
    addDataFrame(as.data.frame(t(c('NAAN', D_naan_data$NAAN))), sheet = sheet_source, 
                 col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 3)
    addDataFrame(as.data.frame(c('NAAN', R_naan_data$NAAN)), sheet = sheet_source, 
                 col.names = FALSE, row.names = FALSE, startRow = 3, startColumn = 2)
    
    
    addDataFrame(Allocated, sheet = sheet_source, 
               col.names = TRUE, row.names = TRUE, startRow = 3, startColumn = 3)
  
    addDataFrame(as.data.frame(as.character("REC_ID")), sheet = sheet_source, 
                 col.names = FALSE, row.names = FALSE, startRow = 3, startColumn = 3)
    
    addDataFrame(D_naan_data, sheet = sheet_disburse, row.names = TRUE)
    
    addDataFrame(R_naan_data, sheet= sheet_receive, row.names = TRUE)
    
  saveWorkbook(wb, name_of_file)
}


#example
#For a sourced transfer
R_naan_data <- read.csv("R_naans.csv", header=TRUE)
D_naan_data <- read.csv("D_naans.csv", header=TRUE)
test_receiving_naans <- abs(R_naan_data$AMOUNT_HIST)
test_disbursed_naans <- abs(D_naan_data$AMOUNT_HIST)


#known_info <- data.frame( which_r_rec_id = c(3), which_d_rec_id = c(1), known_amount = as.numeric( 148.79) )

Allocated_matrix <- solve_alloc_matrix(test_disbursed_naans, test_receiving_naans)
                                       #known_info)

Allocated <- as.data.frame(Allocated_matrix)
colnames(Allocated) <- D_naan_data$REC_ID
rownames(Allocated) <- R_naan_data$REC_ID
print("Conditions were upheld if the following three calculations equal.")
print(paste("Allocated all sum up to the total?", sum(Allocated)))
print(paste("Sum of the disbursed naans: ", sum(test_disbursed_naans)))
print(paste("Sum of the receiving naans: ",sum(test_receiving_naans)))




name_of_file <<- paste0("Test_NAAN_Transfers_Mapped_Source ", R_naan_data$SOURCE[1], " ",
                        Sys.Date(), ".xlsx")
write_to_excel()


