# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Jan 25, 2019    - expanded to entire US
# prior edit: June 18, 2018
# prior edit: June 16, 2017
#
# This script parses XML data of current tide station observations from the
# National Ocean and Atmospheric Administration and outputs the results as
# a figure of preliminery 6-minute water level heights.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# --------------------------------------------------------------------------------------------------------------------

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# output directory
if(comp$nodename=="rsc64dot1x-60.ems.psu.edu"){
  outDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){
  outDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
}else{
  outDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
}


# Create vector including each station ID.
# <!-- TIDE STATIONS -->
##Alabama
#tideIDs <- c("8735523", "8737048", "8738043", "8737138", "8739803", "8732828", "8735180", "8735391", "8736897") 
##Alaska
#tideIDs <- c(tideIDs, "9454240", "9457292", "9462450", "9468756", "9491094", "9468333", "9462620", "9461380", "9457804",
#                      "9455920", "9454050", "9453220", "9452634", "9452210", "9451600", "9451054", "9450460", "9452400",
#                      "9455090", "9455500", "9455760", "9459450", "9459881", "9461710", "9464212", "9497645") # not available: "9452635", "9456173", "9458705"
##California
#tideIDs <- c(tideIDs, "9410170", "9410230", "9411406", "9414290", "9415144", "9419750", "9416841", "9415102", "9415020",
#                      "9414958", "9414575", "9413450", "9412110", "9411340", "9410660", "9410840", "9414523", "9414750", "9414863",
#                      "9418767")
##Connecticut
#tideIDs <- c(tideIDs, "8461490", "8467150", "8465705")
tideIDs <- c("8461490", "8467150", "8465705")
##Delaware
tideIDs <- c(tideIDs, "8555889", "8551762", "8557380", "8551910")
##DC
tideIDs <- c(tideIDs, "8594900")
##Florida
#tideIDs <- c(tideIDs, "8720219", "8726384", "8726520", "8726667", "8726724", "8729210", "8729840", "8725520", "8725110",
#                      "8723970", "8723214", "8722956", "8722670", "8720625", "8720030", "8720215", "8720218", "8720226", "8721604",
#                      "8724580", "8726607", "8727520", "8728690", "8729108")
##Georgia
#tideIDs <- c(tideIDs, "8670870")
##Hawaii
#tideIDs <- c(tideIDs, "1612480", "1617760", "1611400", "1615680", "1612340", "1617433")
##Louisiana
#tideIDs <- c(tideIDs, "8760721", "8760922", "8761305", "8767961", "8768094", "8766072", "8762075",
#                      "8761724", "8761927", "8762483", "8764044", "8764227", "8764314", "8767816")
##Louisiana MSL
#tideIDsMSL <- c("8761955", "8762482") # MSL datum
##Maine
#tideIDs <- c(tideIDs, "8410140", "8419317", "8418150", "8413320", "8411060") # c("8419751", "8414821") # error no longer supported
##Maryland
tideIDs <- c(tideIDs, "8575512", "8574680", "8571421", "8571892", "8573927", "8570283", "8577330", "8573364")
##Massachusetts
tideIDs <- c(tideIDs, "8443970", "8447386", "8449130", "8447930", "8447435") # error "8447506" # no longer supported
##Mississippi
#tideIDs <- c(tideIDs, "8740166", "8741533", "8747437")
##New Hampshire
tideIDs <- c(tideIDs, "8423898")
##New Jersey
tideIDs <- c(tideIDs, "8534720", "8539094", "8536110", "8531680", "8537121")
##New York
tideIDs <- c(tideIDs, "8519483", "8516945", "8518750", "8518962", "8510560")
##North Carolina
#tideIDs <- c(tideIDs, "8651370", "8652587", "8654467", "8656483", "8658120", "8658163")
##Oregon
#tideIDs <- c(tideIDs, "9439201", "9439099", "9431647", "9432780", "9435380", "9437540", "9439040")
##Pennsylvania
tideIDs <- c(tideIDs, "8546252", "8548989", "8545240", "8540433")
##Rhode Island
tideIDs <- c(tideIDs, "8452660", "8454000", "8452944", "8454049")
##South Carolina
#tideIDs <- c(tideIDs, "8665530", "8661070", "8662245")
##Texas
#tideIDs <- c(tideIDs, "8770475", "8770570", "8770613", "8770808", "8770971", "8771972", "8772447", "8772985", "8773767", "8775237",
#                      "8775244", "8775296", "8775792", "8779280", "8779770", "8779748", "8775870", "8774770",
#                      "8773259", "8773037", "8772471", "8771450", "8771013", "8770822", "8770520", "8770777", "8771341", "8771486",
#                      "8773146", "8773701", "8774230", "8775241", "8779749") # "8771801" not supported
#tideIDsMSL <- c(tideIDsMSL, "8776604", "8777812", "8778490", "8776139")
##Virginia
tideIDs <- c(tideIDs, "8638863", "8635027", "8632200", "8635750", "8639348", "8638610", "8631044", "8636580", "8637689", "8638901")
##Washington
#tideIDs <- c(tideIDs, "9440422", "9440581", "9442396", "9443090", "9444900", "9449880",
#                      "9444090", "9440910", "9440569", "9440083", "9441102", "9446484", "9447130", "9449424") # c("9447112", "9446500", "9447113", "9447114", "9447115", "9449896", 9449712") # wa error no longer supported
##Lake Erie
tideIDsGrtLakes <- c("9063020", "9063063", "9063038", "9063053", "9063028", "9063085", "9063079")
##Detroit River
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9044020", "9044036", "9044030", "9044049")
##Lake Huron
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9075002", "9075065", "9075014", "9075035", "9075099", "9075080")
##Lake Michigan
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9087031", "9087044", "9087072", "9087079", "9087096", "9087023", "9087088", "9087057", "9087068")
##Lake Onterio
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9052000", "9052030", "9052090", "9052058", "9052025", "9052076")
##Lake St. Clair
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9034052", "9034057")
##Lake Superior
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9099004", "9099018", "9099064", "9099044", "9099090")
##Niagara River
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9063007", "9063012", "9063009")
##St. Clair River
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9014070", "9014080", "9014087", "9014096", "9014090", "9014098")
##St. Lawrence River
tideIDsGrtLakes <- c(tideIDsGrtLakes, "8311062", "8311030")
##St. Marys River
tideIDsGrtLakes <- c(tideIDsGrtLakes, "9076024", "9076060", "9076027", "9076033", "9076070")



#save(file=paste0(outDir, "tideStationIDs.RData"), "tideIDs", "tideIDsMSL", "tideIDsGrtLakes")
save(file=paste0(outDir, "tideStationIDs_regional.RData"), "tideIDs", "tideIDsGrtLakes")




