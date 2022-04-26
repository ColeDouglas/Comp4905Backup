############ COMP 4905 ############
##### Ordinal View Graph Code #####

# @title: OrdinalViewGraph

#### Main Function ####

#' @Param
#'  Sample_data: a frame of input data
#'   probability:  The alpha value
#'   tails: Whether or not it's a two tailed test
#'   rankVector: How you choose to rank categorical data
#'
#' @return critical value
#'        creates a relevant graph as a byproduct
#'
#' @export

ordinal_view_graph <- function(sample_data, probability = 0.05, tails = 2, rankVector = NA){
  local({
    loopCounter1 = 1
    for(i in sample_data){
      loopCounter2 = 1
      for(ii in sample_data){
        if(loopCounter1 < loopCounter2){
          # Split the data
          set1 <- unlist(sample_data[loopCounter1], use.names = F)
          set2 <- unlist(sample_data[loopCounter2], use.names = F)

          #clean up NA's if treatments are different size
          set1 = set1[!is.na(set1)]
          set2 = set2[!is.na(set2)]

          #Sort Data based on rank vector
          if(!is.na(rankVector[1])){
            set1 = sort_by_rank_vector(set1, rankVector)
            set2 = sort_by_rank_vector(set2, rankVector)
          }else{
            set1 = sort(set1)
            set2 = sort(set2)
          }

          Set1Name = paste("Sample ", loopCounter1)
          Set2Name = paste("Sample ", loopCounter2)

          draw_ordinal_view_graph(set1, set2, probability, tails, rankVector, Set1Name, Set2Name)

          #Pause for pairwise testing
          if(length(sample_data)>2){
            readline(prompt="Press enter to continue pairwise testing")
          }
        }
        loopCounter2 = loopCounter2 +1
      }
      loopCounter1 = loopCounter1 + 1
    }
  })
}


####  Draw Functions ####

# Draw simple solution
draw_ts_graph <- function(set1, set2, tails){
  plot.new()

  ##get the test statistic
  test_Statistic <- get_test_statistic(set1, set2)

  smaller = test_Statistic[2]
  if(smaller == 1){
    smallercolor = "green"
    largercolor = "red"
  }else{
    smallercolor = "red"
    largercolor = "green"
  }

  test_Statistic = test_Statistic[1]


  ## get the length of all outcomes
  outcomes = length(set1)*length(set2)

  # convert to percentage
  test_Statistic = test_Statistic/outcomes


  #Title
  title(main = "Test Statistic")

  height = .2
  length = 1

  #draw the base
  polygon(c(0, 0, length, length), c(.65, height+.65, height+.65, .65), col=largercolor)

  #draw the test statistic
  polygon(c(0, 0, length*test_Statistic, length*test_Statistic),   c(.65, height+.65, height+.65, .65), col=smallercolor)

  #Add lines
  segments(0, 0.60, 0,height+.70, lwd = 2 )
  text( x = 0,y = .5, labels = "0")

  segments(length, 0.60, length, height+.70, lwd = 2 )
  text( x = length, y = .5, labels = outcomes)

  #Add critical values
  critical_value1 = get_critical_value(set1,set2,0.05,tails)
  ratio1 = critical_value1/outcomes


  critical_value2 = get_critical_value(set1,set2,0.01,tails)
  ratio2 = critical_value2/outcomes


  if(critical_value1>0){
    segments(ratio1, 0.60, ratio1, height+.70, lwd = 2 )
    text( x = length * ratio1, y = .5,labels = critical_value1)
    text(x = (ratio1+ratio2)/2, y = height+.61, label = expression("h"[0]),cex = .7)
    text(x = (ratio1+ratio2)/2, y = height+.5, label = "\u03b1 = 0.05", cex = .7)
  }


  if(critical_value2 > 0){
    segments(ratio2, 0.60, ratio2, height+.70, lwd = 2 )

    text( x = length * ratio2, y = .5,labels = critical_value2)
    text(x = ratio2/2,  y = height+.61, label = expression("h"[0]),cex = .5)
    text(x = ratio2/2,  y = height+.5, label = "\u03b1 = 0.01", cex = .5)
    text(x = 0.5, y = height+.55, label = expression("h"[1]))

  }
}

# Draw Professor Biddle's preferred solution
draw_ordinal_view_graph<-function(set1, set2, probability, tails , rankVector, set1Name, set2Name){

  #organize data just in case, because it's free when already sorted, and solves some very specific R problems
  set1 = sort(set1)
  set2 = sort(set2)

  ##get the test statistic
  test_Statistic <- get_test_statistic(set1, set2)

  ## Which side the critical value is on
  ## Some people were confused by the mirroring

  if(test_Statistic[2] == 2){
    Right = FALSE
  }else{
    Right = TRUE
  }

  test_Statistic = test_Statistic[1]

  ## get the length of all outcomes
  outcomes = length(set1)*length(set2)

  # convert to percentage
  test_Statistic_Percent = test_Statistic/outcomes

  #Add critical values
  # Check if probability is dominant
  if(probability == .05){
    critical_value1 = get_critical_value(set1,set2,0.05,tails)
    ratio1 = critical_value1/outcomes
    critical_value2 = get_critical_value(set1,set2,0.01,tails)
    ratio2 = critical_value2/outcomes
  }else{
    critical_value1 = get_critical_value(set1,set2,probability,tails)
    ratio1 = critical_value1/outcomes
    critical_value2 = 0
    ratio2 = 0
  }
  if(Right){
    critical_value1 = outcomes - critical_value1
    ratio1 = 1 - ratio1
    critical_value2 = outcomes - critical_value2
    ratio2 = 1 - ratio2
  }

  CommonLanguageEffectSize = round( test_Statistic_Percent, digits = 2)

  # find out the ranks of sample
  ranks = unique(sort(c(set1, set2)))
  set1Rank = set1

  counter = 1   # index of the for loop
  counter2 = 1  # index of the rank array
  for(i in ranks){
    for(x in set1){
      if (i == x){
        set1Rank[counter2] <- counter
        counter2 = counter2 +1
      }
    }
    counter = counter + 1
  }


  ### Draw the Graph ###

  #create new graph
  plot.new()

  subtitleEF = paste("Effect Size = ", CommonLanguageEffectSize)
  title(main = "Ordinal View Graph", sub = subtitleEF)

  sample1Color = "green"
  sample2Color = "red"

  ##Create a space for the graph##

  # Canvas for the Graph
  boffset2 = .1
  boffset = .2
  loffset = .1
  roffset = .9
  toffset = .7


  #Bottom axis 1
  segments(x0 = loffset,y0 = boffset,x1= roffset, y1 = boffset , cex =.5 )
  text( x = .5, y = 0,labels = paste(set1Name, " Values"))

  #Bottom axis 2
  segments(x0 = loffset,y0 = boffset2,x1= roffset, y1 = boffset2 , cex = .5)
  text( x = roffset, y = (boffset + boffset2)/2,labels = "Rank", pos = 4)

  #Middle axis
  middleOfGraph = (toffset+boffset)/2
  segments(x0 = loffset, y0 = middleOfGraph, x1= roffset, y1 = middleOfGraph )

  #Right lower Axis
  segments(x0 = loffset,y0 = boffset,x1= loffset, y1 = toffset )
  text( x = 0, y = (toffset+middleOfGraph)/2,labels = paste("< ", set2Name), srt =90, cex = 1)

  #Right Upper Axis
  segments(x0 = loffset,y0 = boffset,x1= loffset, y1 = toffset )
  text( x = 0, y = (boffset+middleOfGraph)/2,labels = paste("> ",set2Name), srt =90, cex = 1)


  #sort the two sets into difference lists

  set1Difference = set1
  set2Difference = rep(0,length(set1))

  # Get the number of each element in set 1 which is greater than element in set 2
  counter = 1
  for(i in set1){
    count = 0
    for(ii in set2){
      if(i<ii){
        count = count + 1
      }else if(ii < i){
        set2Difference[counter] = set2Difference[counter]+1;
      }
    }
    set1Difference[counter] = count;
    counter = counter + 1
  }


  # get the maximum and minimum of the differences of the ordinal ranks
  # With added space for aesthetics
  maxQuantity <- minQuantity <- max(c(set1Difference,set2Difference))

  # Draw the Right axis
  height = (1-((1-toffset)+boffset))/(maxQuantity+minQuantity)

  i = 0 - minQuantity
  while( i < maxQuantity +1){
    position = middleOfGraph + (i*height)
    segments(x0 = loffset,y0 = position, x1= loffset/2, y1 = position)
    text(x = (loffset/2) - .01, y = position, label= abs(i),cex = .9)
    i = i+1
  }

   # Fill in the Data
  maxlength = length(set1)

  width = (1-((1-roffset) + loffset))/(maxlength)
  i = 0
  counter = 0

  while(i<=maxlength){
    #Draw to the line
    position = (counter*width) + loffset
    segments(x0 = position,y0 = boffset,x1= position, y1 = boffset-.05)
    segments(x0 = position,y0 = boffset2,x1= position, y1 = boffset2-.05)

    if(i<maxlength){
      # Fill in Bottom Axis 1
      if(!(is.na(rankVector[1]))){
        text(x = (position + (position+width))/2, y = boffset2-.05, labels = rankVector[set1[counter+1]], cex = .5)
      }else{
        text(x = (position + (position+width))/2, y = boffset2-.05, labels = set1[counter+1], cex = .5)
      }

      # Fill in Bottom Axis 2
      text(x = (position + (position+width))/2, y = boffset-.05, labels = set1Rank[counter+1], cex = .5)

      #draw boxes for set 1 difference Here
      riser = 0
      while(riser < set1Difference[counter+1]){
          polygon( c(position,  position + width, position + width, position), c( middleOfGraph +(height*riser), middleOfGraph+(height*riser), middleOfGraph + (height*(riser+1)), middleOfGraph + (height*(riser+1))), col=sample2Color)
          riser = riser + 1
      }

      faller = 0
      while(faller < set2Difference[counter+1]){
        polygon( c(position,  position + width, position + width, position), c( middleOfGraph - (height*faller), middleOfGraph - (height*faller), middleOfGraph - (height*(faller+1)), middleOfGraph - (height*(faller+1))), col=sample1Color)
        faller = faller + 1
      }
    }
    i = i + 1;
    counter = counter + 1;
  }

  ## The Legend ##

  # Canvas for the Legend
  ltoffset = 1
  lboffset = toffset + .025
  lloffset = loffset
  lroffset = .4


  #Sample 1
  text(x = lloffset, y = lboffset + (2*(ltoffset-lboffset)/3), label= paste(set1Name," ="),cex = .9)
  polygon( c( lloffset-((lloffset-lroffset)*.75), lroffset+(2*(lloffset-lroffset)/3)+0.01, lroffset+(2*(lloffset-lroffset)/3)+0.01, lloffset-((lloffset-lroffset)*.75)), c( ltoffset,  ltoffset, ltoffset-((ltoffset-lboffset)/2),ltoffset-((ltoffset-lboffset)/2)), col=sample1Color)

  #sample 2
  text(x = lloffset, y = lboffset + ((ltoffset-lboffset)/3), label= paste(set2Name," ="), cex = .9)
  polygon( c( lloffset-((lloffset-lroffset)*.75), lroffset+(2*(lloffset-lroffset)/3)+0.01, lroffset+(2*(lloffset-lroffset)/3)+0.01, lloffset-((lloffset-lroffset)*.75)), c( lboffset+((ltoffset-lboffset)/2),lboffset+((ltoffset-lboffset)/2), lboffset,  lboffset), col=sample2Color)


  ## The Test Statistic ##

  # Make the test statistic
  ttoffset = 1
  tboffset = toffset + .025
  tloffset = lroffset + .025
  troffset = roffset

  # Draw the base
  if(Right){
    test_Statistic_Percent = 1 - test_Statistic_Percent
  }

  # Draw the test statistic
  polygon(c(tloffset, tloffset, troffset, troffset), c( tboffset+.2, ttoffset-.2, ttoffset - .2, tboffset +.2), col=sample2Color)
  polygon(c(tloffset, tloffset, tloffset+((troffset-tloffset)*test_Statistic_Percent), tloffset+ ((troffset-tloffset)*test_Statistic_Percent)), c( tboffset+.2, ttoffset-.2, ltoffset - .2, tboffset +.2), col=sample1Color)

  # Add lines at the ends
  segments(tloffset, ttoffset-.05, tloffset, tboffset+.05, lwd = 2 )
  text( x = tloffset, y = tboffset+.02, labels = "0", cex = .6)

  segments(troffset, ttoffset-.05, troffset, tboffset+.05, lwd = 2 )
  text( x = troffset, y = tboffset+.02, labels = outcomes, cex = .6)

  # Add the critical value lines
  if(Right){
    if(critical_value1 < outcomes){
      segments(tloffset+((troffset-tloffset)*ratio1), ttoffset-.06, tloffset+((troffset-tloffset)*ratio1), tboffset+.06, lwd = 1 )
      text( x = tloffset+((troffset-tloffset)*ratio1), y =tboffset+.05, labels = critical_value1, cex = .5)

      text(x = ( (tloffset+((troffset-tloffset)*ratio1))+(tloffset+((troffset-tloffset)*ratio2)))/2, y = ((tboffset+ttoffset)/2)+.01, label = expression("h"[0]),cex = .7)
      text(x = ( (tloffset+((troffset-tloffset)*ratio1))+(tloffset+((troffset-tloffset)*ratio2)))/2, y = ((tboffset+ttoffset)/2)-.01, label = paste("\u03b1 = ", probability), cex = .3)
    }
    if(critical_value2 < outcomes){
      segments(tloffset+((troffset-tloffset)*ratio2), ttoffset-.06, tloffset+((troffset-tloffset)*ratio2), tboffset+.06, lwd = 1 )
      text( x = tloffset+((troffset-tloffset)*ratio2), y =tboffset+.05, labels = critical_value2, cex = .5)

      text(x = (troffset + (tloffset+((troffset-tloffset)*ratio2)))/2, y = ((tboffset+ttoffset)/2)+.02, label = expression("h"[0]),cex = .6)
      text(x = (troffset + (tloffset+((troffset-tloffset)*ratio2)))/2, y = ((tboffset+ttoffset)/2)-.02, label = "\u03b1 = 0.01",cex = .5)
    }
    text(x = (tloffset + (tloffset+((troffset-tloffset)*ratio1)))/2,  y = ((tboffset+ttoffset)/2), label = expression("h"[1]))

  }else{
    if(critical_value1 > 0){
      segments(tloffset+((troffset-tloffset)*ratio1), ttoffset-.06, tloffset+((troffset-tloffset)*ratio1), tboffset+.06, lwd = 1 )
      text( x = tloffset+((troffset-tloffset)*ratio1), y =tboffset+.05, labels = critical_value1, cex = .5)

      text(x = ( (tloffset+((troffset-tloffset)*ratio1))+(tloffset+((troffset-tloffset)*ratio2))   )/2, y = ((tboffset+ttoffset)/2)+.01, label = expression("h"[0]),cex = .7)
      text(x = ( (tloffset+((troffset-tloffset)*ratio1))+(tloffset+((troffset-tloffset)*ratio2))   )/2, y = ((tboffset+ttoffset)/2)-.01, label = paste("\u03b1 = ", probability), cex = .3)
    }
    if(critical_value2 > 0){
      segments(tloffset+((troffset-tloffset)*ratio2), ttoffset-.06, tloffset+((troffset-tloffset)*ratio2), tboffset+.06, lwd = 1 )
      text( x = tloffset+((troffset-tloffset)*ratio2), y =tboffset+.05, labels = critical_value2, cex = .5)

      text(x = (tloffset + (tloffset+((troffset-tloffset)*ratio2)))/2,  y = ((tboffset+ttoffset)/2)+.02, label = expression("h"[0]),cex = .6)
      text(x = (tloffset + (tloffset+((troffset-tloffset)*ratio2)))/2,  y = ((tboffset+ttoffset)/2)-.02, label = "\u03b1 = 0.01",cex = .5)
    }
    text(x =(troffset + (tloffset+((troffset-tloffset)*ratio1)))/2, y = ((tboffset+ttoffset)/2), label = expression("h"[1]))
  }
}


#### Helper Functions ####

## Get the test statistics
# Returns c(test statistic, which of the two samples is the origin of the test statistic)
get_test_statistic <- function(set1, set2){

  # Compile the data
  all_data <- c(set1, set2)

  # Sort the data
  all_data <- sort(all_data)

  # Strip duplicates
  all_data2 <- c(NULL)
  for(i in all_data){
    if(!(is.element(i,all_data2))){
      all_data2 = append(all_data2,i)
    }
  }

  ### Get the Test Statistic ###

  r1 <- 0
  r2 <- 0
  rank <- 1

  # Calculate the test statistic
  for ( i in all_data2){
    count <- 0;

    for(ii in all_data){
      if(i == ii){
        count <- count + 1
      }else if(i<ii){
        break
      }
    }

    # The value for each element
    temp_value <- sum(seq(rank, rank+count-1))/count

    # update the tallies for each treatment
    for(ii in set1){
      if(ii == i){
        r1 <- r1 + temp_value
      }
    }

    for(ii in set2){
      if(ii == i){
        r2 <- r2 + temp_value
      }
    }

    rank <- rank + count
  }

  u1 <- length(set1)*length(set2) + (( length(set1) * (length(set1)+1))/2 ) - r1
  u2 <- length(set1)*length(set2) + (( length(set2) * (length(set2)+1))/2 ) - r2

  if(u2<=u1){
    output <- c(u2,2)
  }else{
    output <- c(u1,1)
  }
  return(output)
}

## Get the critical values
# Returns the critical value
get_critical_value <- function(set1, set2, prob, tails){
  lset1 <- length(set1)
  lset2 <- length(set2)
  output <- qwilcox(prob/tails,lset1,lset2) - 1
  return(output)
}

## Manually sorts categorical Data by rank vector
# Done explicitly at the request of professor Biddle, because of the huge implication for recommender systems
# Returns set manually sorted
sort_by_rank_vector<- function(set, rankVector){
  count = 1
  for(x in rankVector){
    count2 = 1
    for(i in set){
      if(tolower(x)==tolower(i)){
        set[count2] = count
      }
      count2 = count2 + 1
    }
    count = count + 1;
  }
  output = as.integer(set)

  return(output)
}

######## Testing ########

Testing <- function(){

  # The Examples from the report
  Examples = T
  test1 = Examples  # Appendix B
  test2 = Examples  # Example 1 - Complete Overlap
  test3 = Examples  # Example 2 - Complete Separation
  test4 = Examples  # Example 3 - Reject the Null hypothesis

  # Randomized Data
  RandomTest = T
  test5 = RandomTest  # Same Population
  test6 = RandomTest  # Different Population
  test7 = RandomTest  # 50/50 either way

  # Large data Sets
  LargeTest = T
  test8  = LargeTest  # Large - Same Population
  test9  = LargeTest  # Large - Different Population
  test10 = LargeTest # Large - 50/50 either way

  # Different P values
  pValues = T
  test11 = pValues  # Two tailed esoteric P-value
  test12 = pValues  # One tailed esoteric P-value
  test13 = pValues  # Two tailed random P-value
  test14 = pValues  # One tailed random P-value

  # Categorical Interpretations
  ranking = T
  test15 = ranking  # blue < green < red < yellow
  test16 = ranking  # green < yellow < blue < red


  # Multiple Data Sets
  multidata = T
  test17 = multidata # 3 data sources
  test18 = multidata # 4 data sources
  test19 = multidata # 5 data sources


  #### The Examples from the Report ####

  # Dataset fom Appendix B
  if(test1){
    local({

      cat("\n ## Working with data from Appendix B ## \n")
      cat("\n ##          Beginning data           ## \n")

      set1 = c(15,6,9,12,5)
      set2 = c(12,10,6,5,6)
      Samples1 = data.frame(v1 = set1, v2 = set2)

      ## Display the data
      print(Samples1)
      cat("\n")

      ordinal_view_graph(Samples1)
      readline(prompt="Press enter to continue")
    })
  }

  # Dataset fom Example 1
  if(test2){
    local({
      cat("\n ## Working with data from Example 1## \n")
      cat("\n ##     Two Identical Samples       ## \n")

      #create data
      set1 = c(1,2,3,4,5,6,7,8,9,10)
      set2 = c(1,2,3,4,5,6,7,8,9,10)
      Samples2 = data.frame(v1 = set1, v2 = set2)

      ## Display the data
      print(Samples2)
      cat("\n")

      ordinal_view_graph(Samples2)
      readline(prompt="Press enter to continue")
    })
  }

  # Dataset fom Example 2
  if(test3){
    local({
      cat("\n ## Working with data from Example 2## \n")
      cat("\n ##     Two Seperate Samples        ## \n")

      set1 = c(1,2,3,4,5,6,7,8,9,10)
      set2 = c(11,12,13,14,15,16,17,18,19,20)
      Samples3 = data.frame(v1 = set1, v2 = set2)

      ## Display the data
      print(Samples3)
      cat("\n")

      ordinal_view_graph(Samples3)
      readline(prompt="Press enter to continue")
    })
  }

  # Dataset fom Example 3
  if(test4){
    local({
      cat("\n ## Working with data from Example 3## \n")
      cat("\n ##    Reject the Null Hypothesis   ## \n")

      set1 = c(8,8,8,9,11,12,12,13,13,14)
      set2 = c(10,11,13,15,15,15,15,NA,NA,NA)
      Samples4 = data.frame(v1 = set1, v2 = set2)

      ## Display the data
      print(Samples4)
      cat("\n")

      ordinal_view_graph(Samples4)
      readline(prompt="Press enter to continue")
    })
  }



  #### Randomized Data ####

  # Same Population
  if(test5){
    local({
      cat("\n ## Random - Same Population ## \n")

      minimum_set_size = 5
      maximum_set_size = 15

      maximum_rank = 15

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      maxi= floor(runif(1,5,maximum_rank))
      mini= floor(runif(1,1,floor(maxi/2)))

      set1= floor(rnorm(length1,(mini+maxi)/2,2))
      set2= floor(rnorm(length2,(mini+maxi)/2,2))

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples)
      readline(prompt="Press enter to continue")
    })
  }

  # Different Population
  if(test6){
    local({
      cat("\n ## Random - Different Population ## \n")

      minimum_set_size = 5
      maximum_set_size = 15

      maximum_value = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      max1= floor(runif(1,8,maximum_value))
      min1= floor(runif(1,1,floor(max1/2)))

      set1= floor(rnorm(length1,(min1+max1)/2,2))
      set2= floor(rnorm(length2,((min1+max1)/2)-2,1))

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples)
      readline(prompt="Press enter to continue")
    })
  }

   # 50/50 either way
  if(test7){
    local({
      cat("\n ## Random - 50 percent chance of being the same, ## \n")

      minimum_set_size = 5
      maximum_set_size = 15

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      #50/50 if same set or different
      same = floor(runif(1,1,3))

      if(same == 1){
        maxi= floor(runif(1,5,maximum_rank))
        mini= floor(runif(1,1,floor(maxi/2)))

        set1= floor(rnorm(length1,(mini+maxi)/2,2))
        set2= floor(rnorm(length2,(mini+maxi)/2,2))

      }else{
        max1= floor(runif(1,8,maximum_rank))
        min1= floor(runif(1,1,floor(max1/2)))

        set1= floor(rnorm(length1,(min1+max1)/2,2))
        set2= floor(rnorm(length2,((min1+max1)/2)-2,1))
      }

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples)
    })
  }


  #### Large Datasets ####

  # Same Population
  if(test8){
    local({
      cat("\n ## Large DataSet Same Population ## \n")

      minimum_set_size = 50
      maximum_set_size = 100

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      maxi= floor(runif(1,5,maximum_rank))
      mini= floor(runif(1,1,floor(maxi/2)))

      set1= floor(rnorm(length1,(mini+maxi)/2, 3))
      set2= floor(rnorm(length2,(mini+maxi)/2, 3))


      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples)
      readline(prompt="Press enter to continue")
    })
  }

  # Different Population
  if(test9){
    local({
      cat("\n ## Large DataSets - Different Population ## \n")

      minimum_set_size = 50
      maximum_set_size = 100

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      max1= floor(runif(1,8,maximum_rank))
      min1= floor(runif(1,1,floor(max1/2)))

      set1= floor(rnorm(length1,(min1+max1)/2,2))
      set2= floor(rnorm(length2,((min1+max1)/2)-2,1))

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples)
      readline(prompt="Press enter to continue")
    })
  }

  # 50/50 either way
  if(test10){
    local({
      cat("\n ## LargeDataset - 50 percent chance of being the same, ## \n")

      minimum_set_size = 50
      maximum_set_size = 100

      maximum_rank = 15

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      #50/50 if same set or different
      same = floor(runif(1,1,3))

      if(same == 1){
        maxi= floor(runif(1,5,maximum_rank))
        mini= floor(runif(1,1,floor(maxi/2)))

        set1= floor(rnorm(length1,(mini+maxi)/2, 3))
        set2= floor(rnorm(length2,(mini+maxi)/2, 3))


      }else{
        max1= floor(runif(1,8,maximum_rank))
        min1= floor(runif(1,1,floor(max1/2)))

        set1= floor(rnorm(length1,(min1+max1)/2,2))
        set2= floor(rnorm(length2,((min1+max1)/2)-2,1))
      }

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples)
    })
  }


  #### Different P values ####

  # Two tailed esoteric P-value
  if(test11){
    local({
      cat("\n ## Two tailed esoteric P-value ## \n")

      minimum_set_size = 5
      maximum_set_size = 15

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      #50/50 if same set or different
      same = floor(runif(1,1,3))

      if(same == 1){
        maxi= floor(runif(1,5,maximum_rank))
        mini= floor(runif(1,1,floor(maxi/2)))

        set1= floor(rnorm(length1,(mini+maxi)/2, 3))
        set2= floor(rnorm(length2,(mini+maxi)/2, 3))

      }else{
        max1= floor(runif(1,8,maximum_rank))
        min1= floor(runif(1,1, floor(max1/2)))

        set1= floor(rnorm(length1,(min1+max1)/2,2))
        set2= floor(rnorm(length2,((min1+max1)/2)-2,1))
      }

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples, 0.073)
      readline(prompt="Press enter to continue")
    })
  }

  # One tailed esoteric P-value
  if(test12){
    local({
      cat("\n ## One tailed esoteric P-value ## \n")

      minimum_set_size = 5
      maximum_set_size = 15

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      #50/50 if same set or different
      same = floor(runif(1,1,3))

      if(same == 1){
        maxi= floor(runif(1,5,maximum_rank))
        mini= floor(runif(1,1,floor(maxi/2)))

        set1= floor(rnorm(length1,(mini+maxi)/2, 3))
        set2= floor(rnorm(length2,(mini+maxi)/2, 3))

      }else{
        max1= floor(runif(1,8,maximum_rank))
        min1= floor(runif(1,1,floor(max1/2)))

        set1= floor(rnorm(length1,(min1+max1)/2,2))
        set2= floor(rnorm(length2,((min1+max1)/2)-2,1))
      }

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      ordinal_view_graph(Samples, 0.073)
      readline(prompt="Press enter to continue")
    })
  }

  # Two tailed random P-value
  if(test13){
    local({
      cat("\n ## Two tailed random P-value ## \n")


      minimum_set_size = 5
      maximum_set_size = 15

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      #50/50 if same set or different
      same = floor(runif(1,1,3))

      if(same == 1){
        maxi= floor(runif(1,5,maximum_rank))
        mini= floor(runif(1,1,floor(maxi/2)))

        set1= floor(runif(length1,mini,maxi))
        set2= floor(runif(length2,mini,maxi))

      }else{
        max1= floor(runif(1,8,maximum_rank))
        min1= floor(runif(1,1,floor(max1/2)))

        set1= floor(rnorm(length1,(min1+max1)/2,2))
        set2= floor(rnorm(length2,((min1+max1)/2)-2,1))
      }

      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      randProb = floor((runif(1,1,25))*100)/10000
      ordinal_view_graph(Samples, randProb)
      readline(prompt="Press enter to continue")
    })
  }

  # One tailed random P-value
  if(test14){
    local({
      cat("\n ## One tailed random P-value ## \n")

      minimum_set_size = 5
      maximum_set_size = 15

      maximum_rank = 20

      #generate two random length sets
      length1 = floor(runif(1,minimum_set_size, maximum_set_size))
      length2 = floor(runif(1,minimum_set_size, maximum_set_size))

      #50/50 if same set or different
      same = floor(runif(1,1,3))

      if(same == 1){
        maxi= floor(runif(1,5,maximum_rank))
        mini= floor(runif(1,1,floor(maxi/2)))

        set1= floor(runif(length1,mini,maxi))
        set2= floor(runif(length2,mini,maxi))

      }else{
        max1= floor(runif(1,8,maximum_rank))
        min1= floor(runif(1,1,floor(max1/2)))

        set1= floor(rnorm(length1,(min1+max1)/2,2))
        set2= floor(rnorm(length2,((min1+max1)/2)-2,1))
      }


      # Pad out the vectors by hand
      if(length1 != length2){
        if(length1 > length2){
          length(set2) = length1
        }else{
          length(set1) = length2
        }
      }

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      randProb = floor((runif(1,1,25))*100)/10000
      ordinal_view_graph(Samples, randProb, 1)
      readline(prompt="Press enter to continue")
    })
  }


  #### Categorical Interpretations ####

  # blue < green < red < yellow
  if(test15){
    local({
      cat("\n ## blue < green < red < yellow ## \n")

      set1 = c("green", "blue",  "red",   "red","blue","red", "green",   "blue")
      set2 = c("yellow", "yellow", "red","yellow", "red","yellow", "yellow", "green")

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      rankV = c("blue","green","red","yellow")
      ordinal_view_graph(Samples, 0.05, 2, rankV )
      readline(prompt="Press enter to continue")
    })
  }

  # green < yellow < blue < red
  if(test16){
    local({
      cat("\n ## green < yellow < blue < red## \n")

      set1 = c("green", "blue",  "red",   "red","blue","red", "green","blue")
      set2 = c("yellow", "yellow", "red","yellow", "red","yellow", "yellow", "green")

      Samples = data.frame(v1 = set1, v2 = set2)

      print("Samples")
      print(Samples)
      cat("\n")

      rankV = c("green","yellow","blue","red")
      ordinal_view_graph(Samples, 0.05, 2, rankV )
      readline(prompt="Press enter to continue")
    })
  }


  #### Multiple Data Sets ####

  # 3 data sources
  if(test17){
    local({
      cat("\n ##     3 Data Sources        ## \n")

      set1 = c(1,2,3,4,5,6,7,8,9,10)
      set2 = c(11,12,13,14,15,16,17,18,19,20)
      set3 = c(1,2,3,4,5,6,7,8,9,10)
      Samples3 = data.frame(v1 = set1, v2 = set2, v3 = set3)

      ## Display the data
      print(Samples3)
      cat("\n")

      ordinal_view_graph(Samples3)
      readline(prompt="Press enter to continue")
    })
  }

  # 4 data sources
  if(test18){
    local({
      cat("\n ##     4 Data Sources        ## \n")

      set1 = c(1,2,3,4,5,6,7,8,9,10)
      set2 = c(11,12,13,14,15,16,17,18,19,20)
      set3 = c(1,2,3,4,5,6,7,8,9,10)
      set4 = c(21,22,23,24,25,26,27,28,29,30)
      Samples3 = data.frame(v1 = set1, v2 = set2, v3 = set3, v4 = set4)

      ## Display the data
      print(Samples3)
      cat("\n")

      ordinal_view_graph(Samples3)
      readline(prompt="Press enter to continue")
    })
  }

  # 5 data sources
  if(test19){
    local({
      cat("\n ##     5 Data Sources        ## \n")

      set1 = c(1,2,3,4,5,6,7,8,9,10)
      set2 = c(11,12,13,14,15,16,17,18,19,20)
      set3 = c(1,2,3,4,5,6,7,8,9,10)
      set4 = c(21,22,23,24,25,26,27,28,29,30)
      set5 = c(1,2,3,4,5,6,7,8,9,10)

      Samples3 = data.frame(v1 = set1, v2 = set2, v3 = set3, v4 = set4, v5 = set5)

      ## Display the data
      print(Samples3)
      cat("\n")

      ordinal_view_graph(Samples3)
      readline(prompt="Press enter to continue")
    })
  }
}
