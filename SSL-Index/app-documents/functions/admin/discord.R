## Loads config information for discord information
config <- config::get(config = "discord")

sendTest <- function(){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$player, "');

      request.setRequestHeader('Content-type', 'application/json');

      const params = {
        username: 'Captain Hook',
        avatar_url: '',
        content: 'The message to send'
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

sendNewCreate <- function(data, username){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$player, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A new player has been created'
        },
        title: '", paste0(data$first, " ", data$last) %>% str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'TPE Banked',
                      value: ", data$tpebank,",
                   },
                   {
                      name: 'Position',
                      value: ", data$position,",
                   },
                   { 
                      name: 'Username',
                      value: '", username, "',
                   },
        ]
      }

      var params = {
        username: 'Captain Hook',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

