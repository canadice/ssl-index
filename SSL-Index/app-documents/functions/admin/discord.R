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

sendGradedTPE <- function(source, tpe){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$tpe, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A new PT has been graded!'
        },
        title: '", source, "',
        fields: [",
                   paste0("{ name: '", tpe$username, "', value:", tpe$tpe, ", inline: TRUE}") %>% paste(collapse = ","),
        "],
        footer: {
          text: 'The TPE has already been added to your player page, this is just a report.'
        }
      }

      var params = {
        username: 'PT Watcher',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

sendAcademyIndexUpdate <- function(season){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$indexUpdate, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'The Academy Index has been updated!'
        },
        title: 'Season ", season, "'
      }

      var params = {
        username: 'Index Update',
        embeds: [ myEmbed ]
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

sendApprovedCreate <- function(data){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$approved, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A new player has been approved'
        },
        title: '", paste0(data$first, " ", data$last) %>% str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'TPE Banked',
                      value: ", data$tpebank,",
                   },
                   {
                      name: 'Position',
                      value: '", data$position,"',
                   },
                   { 
                      name: 'Username',
                      value: '", data$username, "',
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

sendRetiredPlayer <- function(data){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$player, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A player has retired'
        },
        title: '", paste0(data$first, " ", data$last) %>% str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'Team',
                      value: '", data$team,"',
                   },
                   { 
                      name: 'Username',
                      value: '", data$username, "',
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

