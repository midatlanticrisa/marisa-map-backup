# Function to create a string of observations based on what is available and which time is oldest.
obs_string = function(TMP, DIS, GAG){
  OBS = TMP
  for(i in 1:length(TMP[ ,1])){
    if(TMP[i,2]=="NA" && DIS[i,2]=="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = ""
      OBS[i,3] = ""
      
    } else if (TMP[i,2]=="NA" && DIS[i,2]=="NA" && GAG[i,2]!="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Gage height: </strong><br/><br/>", GAG[i,2], " ft<br/><br/>", sep="")
      OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      
    } else if (TMP[i,2]=="NA" && DIS[i,2]!="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Discharge: </strong>",DIS[i,2], " ft&#179;/s<br/><br/>", sep="")
      OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")      
      
    } else if (TMP[i,2]!="NA" && DIS[i,2]=="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><br/>", sep="")
      OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      
    } else if (TMP[i,2]!="NA" && DIS[i,2]!="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><strong>Discharge: </strong>", 
                       DIS[i,2], " ft&#179;/s<br/><br/>", sep="")
      
      if(TMP[i,3] < DIS[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")
      }
      
    } else if (TMP[i,2]!="NA" && DIS[i,2]=="NA" && GAG[i,2]!="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><strong>Gage height: </strong>", 
                       GAG[i,2], " ft<br/><br/>", sep="")
      
      if(TMP[i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      }
      
    } else if (TMP[i,2]=="NA" && DIS[i,2]!="NA" && GAG[i,2]!="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Discharge: </strong>", DIS[i,2], " ft&#179;/s<br/><strong>Gage height: </strong>", GAG[i,2], " ft<br/><br/>", sep="")
      
      if(DIS[i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      }
      
    } else { 
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><strong>Discharge: </strong>", 
                       DIS[i,2], " ft&#179;/s<br/><strong>Gage height: </strong>", GAG[i,2], " ft<br/><br/>", sep="")
      
      if(TMP[i,3] < DIS[i,3] && TMP[i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      } else if(DIS[i,3] < TMP[i,3] && DIS [i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      }}
  }
  return(OBS)
}

# Function to create a string of observations based on what is available and which time is oldest.
obs_string_single = function(TMP, DIS, GAG){
  OBS = TMP
    if(TMP[2]=="NA" && DIS[2]=="NA" && GAG[2]=="NA"){
      OBS[2] = ""
      OBS[3] = ""
      
    } else if (TMP[2]=="NA" && DIS[2]=="NA" && GAG[2]!="NA"){
      OBS[2] = paste("<br/><br/><strong>Gage height: </strong>", GAG[2], " ft<br/><br/>", sep="")
      OBS[3] = paste("<br/><br/>Last Updated on ", GAG[3], sep="")
      
    } else if (TMP[2]=="NA" && DIS[2]!="NA" && GAG[2]=="NA"){
      OBS[2] = paste("<br/><br/><strong>Discharge: </strong>",DIS[2], " ft&#179;/s<br/><br/>", sep="")
      OBS[3] = paste("<br/><br/>Last Updated on ", DIS[3], sep="")      
      
    } else if (TMP[2]!="NA" && DIS[2]=="NA" && GAG[2]=="NA"){
      OBS[2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[2], " &#8457;<br/><br/>", sep="")
      OBS[3] = paste("<br/><br/>Last Updated on ", TMP[3], sep="")
      
    } else if (TMP[2]!="NA" && DIS[2]!="NA" && GAG[2]=="NA"){
      OBS[2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[2], " &#8457;<br/><strong>Discharge: </strong>", 
                       DIS[2], " ft&#179;/s<br/><br/>", sep="")
      
      if(TMP[3] < DIS[3]){
        OBS[3] = paste("<br/><br/>Last Updated on ", TMP[3], sep="")
      } else {
        OBS[3] = paste("<br/><br/>Last Updated on ", DIS[3], sep="")
      }
      
    } else if (TMP[2]!="NA" && DIS[2]=="NA" && GAG[2]!="NA"){
      OBS[2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[2], " &#8457;<br/><strong>Gage height: </strong>", 
                       GAG[2], " ft<br/><br/>", sep="")
      
      if(TMP[3] < GAG[3]){
        OBS[3] = paste("<br/><br/>Last Updated on ", TMP[3], sep="")
      } else {
        OBS[3] = paste("<br/><br/>Last Updated on ", GAG[3], sep="")
      }
      
    } else if (TMP[2]=="NA" && DIS[2]!="NA" && GAG[2]!="NA"){
      OBS[2] = paste("<br/><br/><strong>Discharge: </strong>", DIS[2], " ft&#179;/s<br/><strong>Gage height: </strong>", GAG[2], " ft<br/><br/>", sep="")
      
      if(DIS[3] < GAG[3]){
        OBS[3] = paste("<br/><br/>Last Updated on ", DIS[3], sep="")
      } else {
        OBS[3] = paste("<br/><br/>Last Updated on ", GAG[3], sep="")
      }
      
    } else { 
      OBS[2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[2], " &#8457;<br/><strong>Discharge: </strong>", 
                       DIS[2], " ft&#179;/s<br/><strong>Gage height: </strong>", GAG[2], " ft<br/><br/>", sep="")
      
      if(TMP[3] < DIS[3] && TMP[3] < GAG[3]){
        OBS[3] = paste("<br/><br/>Last Updated on ", TMP[3], sep="")
      } else if(DIS[3] < TMP[3] && DIS [3] < GAG[3]){
        OBS[3] = paste("<br/><br/>Last Updated on ", DIS[3], sep="")
      } else {
        OBS[3] = paste("<br/><br/>Last Updated on ", GAG[3], sep="")
      }}
  return(OBS)
}