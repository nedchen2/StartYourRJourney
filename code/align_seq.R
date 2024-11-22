# Define a function to identify_the_shorter_seq

identify_the_seq <- function(seq1,seq2){
  # Input two sequence
  # seq1 and seq2
  # We will return
  # longer_seq : longer sequence in seq1 and seq2
  # shorter_seq : shorter sequence in seq1 and seq2
  # Also the length of those sequence
  
  seq1 = strsplit(seq1,split = "")[[1]]
  seq2 = strsplit(seq2,split = "")[[1]]  
  
  l1 <- length(seq1)
  l2 <- length(seq2)
  if (l1 >= l2){
    s1 <- seq1
    s2 <- seq2
  }else{
    s1 <- seq2 
    s2 <- seq1
    # Using a temporary variable here to swap the length
    . <- l1
    l1 <- l2
    l2 <- .
  }   
  return( list(longer_seq = s1, shorter_seq = s2, longer_length  = l1, shorter_length = l2) )
}

# Define a function to calculate the matching score, according to different start point

calculate_score <- function(s1, s2, l1, l2, startpoint){

  # Input 
  # s1: longer sequence
  # s2: shorter sequence
  # l1: length of s1
  # l2: length of s2
  # startpoint: the position in longer sequence, where shorter sequence start to match
  # startpoint = startpoint - 1
  startpoint_seq <- paste(rep(".",startpoint), collapse = "") # For better illustration
  matched <- paste(paste(s1, collapse = ""), "\n", startpoint_seq, sep = "") # formating the print
  score <- 0
  for (i in seq(length(s2))){
    if (i + startpoint  <= l1){
      if (s1[i + startpoint] == s2[i]){# if the bases match
        matched = paste(matched,s2[i],sep = "")
        score = score + 1
      }else{
        matched = paste(matched,"-",sep = "")
      }
    }
  }
  cat(paste0("Start point:", startpoint + 1), fill = T) # cat is another way to print out something 
  cat(matched,fill = T)
  return(score)
}

# calculate_score(s1, s2, l1, l2, 10)

higher_score_finder <- function(s1,s2,l1,l2){
  my_best_score = -1   # Set the default best score
  for (i in seq(length(s1))){ # for loop to use different start point for the same process.
    i = i - 1   # In this case start from 0, which is we start from the very beginning of the sequence
    z <- calculate_score(s1, s2, l1, l2, i) 
    if (z > my_best_score){ 
      my_best_score = z
      start_point = i
    }else if (z == my_best_score){ # You may find multiple result that have the same match score, you need to output all
      my_best_score = z            # still identical
      start_point = c(i, start_point) # store all possibble start point
    }
  }

  # Below is for better result visualization.
  print ("=====================Results of alignment=====================")
  print(paste("Best score:", my_best_score))
  print(paste("We have", length(start_point) , "best matches"))
  seq1 = paste(s1, collapse = "")
  seq2 = paste(s2, collapse = "")
  
  my_best_align = list()
  for (i in seq(length(start_point))){
    point = start_point[i]
    my_best_align[[paste("match", i)]] = paste(paste(rep(".",point), collapse = ""), seq2 , sep = "")
  } 
  print("==== Template sequence ====")
  print(seq1)
  print("==== Best aligned ====")
  print(my_best_align)
  return(list(my_best_score = my_best_score, 
              start_point = start_point, 
              best_align = my_best_align))
}


main1 <- function(){
  seq_list <- read.csv("../document/case3/align_seq_data.csv",header = F) # Read the sequence
  # seq1 and seq 2
  seq1 = seq_list[1,2]
  seq2 = seq_list[2,2]
  prepare <- identify_the_seq(seq1 = seq1, seq2 = seq2)
  res.align = higher_score_finder(s1 = prepare$longer_seq,
                      s2 = prepare$shorter_seq, 
                      l1 = prepare$longer_length,
                      l2 = prepare$shorter_length)  
  
}

main1()

main2 <- function(){
  seq_list <- read.csv("../document/case3/align_seq_data.csv",header = F) # Read the sequence
  # seq4 and seq 2
  seq1 = seq_list[2,2]
  seq2 = seq_list[4,2]
  prepare <- identify_the_seq(seq1 = seq1, seq2 = seq2)
  res.align = higher_score_finder(s1 = prepare$longer_seq,
                                  s2 = prepare$shorter_seq, 
                                  l1 = prepare$longer_length,
                                  l2 = prepare$shorter_length)  
  
}

main2()

main3 <- function(){
  seq1 = readLines("../document/case5/bacteria1.fa")[2]
  seq2 = readLines("../document/case5/bacteria_ref.fa")[2]
  # seq2 = paste( seq2, collapse = "")
  prepare <- identify_the_seq(seq1 = seq1, seq2 = seq2)
  res.align = higher_score_finder(s1 = prepare$longer_seq,
                                  s2 = prepare$shorter_seq, 
                                  l1 = prepare$longer_length,
                                  l2 = prepare$shorter_length)  
  
  res.align1 <- higher_score_finder(s1 = prepare$longer_seq,
                                    s2 = prepare$shorter_seq, 
                                    l1 = prepare$longer_length,
                                    l2 = prepare$shorter_length)  
  
  seq1 = readLines("../document/case5/bacteria2.fa")[2]
  
  prepare <- identify_the_seq(seq1 = seq1, seq2 = seq2)
  res.align2 = higher_score_finder(s1 = prepare$longer_seq,
                                  s2 = prepare$shorter_seq, 
                                  l1 = prepare$longer_length,
                                  l2 = prepare$shorter_length)  
  
  if (res.align1$my_best_score > res.align2$my_best_score){
    print("bacteria1.fa is more likely to be the one with e.coli")
  }else if(res.align1$my_best_score < res.align2$my_best_score) {
    print("bacteria2.fa is more likely to be the one with e.coli")
  }else{
    print("They share the same score")
  }
  
  return(list(align1 = res.align1,align2 = res.align2))
}
main3()
