#* @apiTitle Admin API
#* @apiDescription Endpoints to get index information.

#* Get TPE cost table
#* @get /tpeTable
#* @serializer json
#* 
function() {
  portalQuery("SELECT * FROM tpetable")
}

#* Get all attribute information
#* @get /attributes
#* @serializer json
#* 
function() {
  portalQuery("SELECT * FROM attributes")
}

#* Get current season
#* @get /getCurrentSeason
#* @serializer json
#* 
function() {
  indexQuery("SELECT * FROM seasoninfo ORDER BY startDate DESC LIMIT 1")
}

#* Get all statistics information
#* @get /statistics
#* @serializer json
#* 
function() {
  indexQuery("SELECT * FROM statlegend")
}
