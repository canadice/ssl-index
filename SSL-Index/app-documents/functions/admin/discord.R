## Loads config information for discord information
config <- config::get(config = "discord")

sendTest <- function(){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$player, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A new PT has been graded!'
        },
        title: 'TPE List',
        fields: [
                   {name: '', value:'```Test\\\\nTEST```'}
        ]
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

sendGradedTPE <- function(source, tpe){
  
  gradedString <- apply(tpe %>% select(username, tpe), 1, function(row) paste(row, collapse = ": ")) %>% 
    paste(collapse = "\\n")
  
  if(nchar(gradedString) > 1024){
    gradedString1 <- apply(tpe %>% select(username, tpe), 1, function(row) paste(row, collapse = ": ")) %>% 
      .[1:floor(length(.)/2)] %>% 
      paste(collapse = "\\n")
    
    gradedString2 <- apply(tpe %>% select(username, tpe), 1, function(row) paste(row, collapse = ": ")) %>% 
      .[(floor(length(.)/2) + 1):length(.)] %>% 
      paste(collapse = "\\n")
    
    jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$tpe, "');
      request.setRequestHeader('Content-type', 'application/json');
      var myEmbed = {
        author: {name: 'A new PT has been graded!'},
        title: '", source, "',
        fields: [ 
          {name: '', 
           value: '", sprintf("```%s```", gradedString1), "'},
          {name: '', 
           value: '", sprintf("```%s```", gradedString2), "'}
        ],
        footer: {
          text: 'If you have received 0 or reduced TPE, please check a summary post in the PT thread. \\n\\nThe TPE has already been added to your player page, this is just a report.'
        } 
      };
      
      var params = {username: 'PT Watcher',embeds: [ myEmbed ]};
      
      request.send(JSON.stringify(params));
    }
    
    sendMessage();"
    )
    
    # cat(jscode)
    
    runjs(jscode)
    
  } else {
    jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$tpe, "');
      request.setRequestHeader('Content-type', 'application/json');
      var myEmbed = {
        author: {name: 'A new PT has been graded!'},
        title: '", source, "',
        fields: [ 
          {name: '', 
           value: '", sprintf("```%s```", gradedString), "'}
        ],
        footer: {
          text: 'If you have received 0 or reduced TPE, please check a summary post in the PT thread. \\n\\nThe TPE has already been added to your player page, this is just a report.'
        } 
      };
      
      var params = {username: 'PT Watcher',embeds: [ myEmbed ]};
      
      request.send(JSON.stringify(params));
    }
    
    sendMessage();"
    )
    
    # cat(jscode)
    
    runjs(jscode)
  }
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

sendIndexUpdate <- function(season){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$indexUpdate, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'The Index has been updated!'
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
                   { 
                      name: 'Discord',
                      value: '", data$discord, "',
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
  
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", config$discord$approved, "');

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

