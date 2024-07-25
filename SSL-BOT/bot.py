import discord
import os # default module
from dotenv import load_dotenv
import asyncio  # missing import
import pandas as pd
import sqlite3

load_dotenv('.secrets/.env') # load all the variables from the env file
intents = discord.Intents.all()
TOKEN = os.getenv('DISCORD_v2_TOKEN')
SERVER = [os.getenv('DISCORD_SERVER')]

bot = discord.Bot(debug_guilds=SERVER)


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
    except Error as e:
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

## Gets the current username tied to a Discord User
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


#### SLASH FUNCTIONS ####
@bot.event
async def on_ready():
    print(f"{bot.user} is ready and online!")

@bot.slash_command(name="store", description="Store your forum username in the bot.")
async def store(ctx: discord.ApplicationContext, *, username: typing.Optional[str] = None):
    if username is None:
        await ctx.respond("Please provide a username.")
        return
      
    discord_id = ctx.author.id
    discord_user = ctx.author.name
    
    session = create_connection()
    with session:
      table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
      
      if(table.shape[0] > 0):
        old_name = table["username"][0]
        
        update_row(session, [discord_id, discord_user, player_name, discord_id])
        
        await ctx.respond(f"Changed association from: {old_name} -> {username}")
        
      else:
        add_row(session, [discord_id, discord_user, player_name])
        
        await ctx.respond(f"Associated Discord account with username: {username}")
        
    session.close()

@bot.slash_command(name="reload", description="Reloading some cogs")
async def reload(ctx: discord.ApplicationContext, extension: str):
    bot.unload_extension(f'cogs.{extension}')
    bot.load_extension(f'cogs.{extension}')
    await ctx.respond("Extension is being reloaded.")

@bot.slash_command(name="hello", description="Say hello to the bot")
async def hello(ctx: discord.ApplicationContext):
    await ctx.respond("Why are you the only command that shows up???")

cogs_list = [
    'trivia',
    'player',
    'leaders'
]

for cog in cogs_list:
    bot.load_extension(f'cogs.{cog}')

async def main():
    async with bot:
        await bot.start(TOKEN)  # Run the bot with the token

try:
    # Check if there is already a running event loop
    loop = asyncio.get_running_loop()
    # Create a task to run the bot in the existing event loop
    loop.create_task(main())
    loop.run_forever()
except RuntimeError:
    # If no running loop is found, create a new event loop
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    try:
        # Run the bot using the new event loop
        loop.run_until_complete(main())
    except KeyboardInterrupt:
        # Gracefully handle the shutdown
        pass
    finally:
        # Close the loop
        loop.close()


