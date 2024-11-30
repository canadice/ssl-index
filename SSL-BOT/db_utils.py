import pandas as pd
import sqlite3

#### DATABASE FUNCTIONS ####
## Creates a connection to the user/player name database
def create_connection():
    """ create a database connection to the SQLite database
        specified by db_file
    :param db_file: database file
    :return: Connection object or None
    """
    conn = None
    try:
        conn = sqlite3.connect("database/discordBotUser.db")
    except Exception as e:
        print(e)

    return conn
  
## Adds a new user row to the database
def add_row(conn, data):
    """
    Create a new row into the table
    :param conn:
    :param data:
    :return: row id
    """
    sql = ''' INSERT INTO discordUser(discordID, username, player)
              VALUES(?,?,?) '''
    cur = conn.cursor()
    cur.execute(sql, data)
    conn.commit()
    return cur.lastrowid

## Updates a current user row to the database
def update_row(conn, data):
    """
    update priority, begin_date, and end date of a task
    :param conn:
    :param task:
    :return: row id
    """
    sql = ''' UPDATE discordUser
              SET discordID = ? ,
                  username = ? ,
                  player = ?
              WHERE discordID = ?'''
    cur = conn.cursor()
    cur.execute(sql, data)
    conn.commit()
    return(cur.lastrowid)

## Gets the current player name tied to a Discord User
def get_name(discord_id):
    session = create_connection()
    with session:
      table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
      if(table.shape[0] > 0):
        name = table["player"][0]
      else:
        name = None
    session.close()
    
    return(name)


## Gets the current player name tied to a Discord User
def get_username(discord_id):
    session = create_connection()
    with session:
      table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
      if(table.shape[0] > 0):
        name = table["username"][0]
      else:
        name = None
    session.close()
    
    return(name)

def get_discord_id(username):
    session = create_connection()
    with session:
        table = pd.read_sql_query("SELECT * from discordUser WHERE username = " + str(username), session)
        if(table.shape[0] > 0):
            id = table["discordID"][0]
        else:
            id = None
    session.close()
    return(id)


