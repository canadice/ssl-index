import discord
import os # default module
from dotenv import load_dotenv
import asyncio  # missing import
import typing
import requests
from db_utils import *

load_dotenv('.secrets/.env') # load all the variables from the env file
intents = discord.Intents.all()
TOKEN = os.getenv('DISCORD_v2_TOKEN')
SERVER = [os.getenv('DISCORD_SERVER')]

bot = discord.Bot(debug_guilds=SERVER)

#### SLASH FUNCTIONS ####
@bot.event
async def on_ready():
    print(f"{bot.user} is ready and online!")

@bot.slash_command(name="store", description="Store your forum username in the bot.")
async def store(ctx: discord.ApplicationContext, *, username: str):
    discord_id = ctx.author.id
    discord_user = ctx.author.name
    
    player = requests.get('https://api.simulationsoccer.com/player/getPlayer?username=' + username.replace(" ", "%20"))
    
    playerData = pd.DataFrame(eval(player.content))
    
    if(playerData.shape[1] < 2):
      await ctx.respond("Please check the spelling of the username as none was found with that name.")
      return
    else:
      playerName = playerData.iloc[0]['name']
      
      session = create_connection()
      with session:
        table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
        
        if(table.shape[0] > 0):
          old_name = table["username"][0]
          
          update_row(session, [discord_id, username, playerName, discord_id])
          
          await ctx.respond(f"Changed association from: {old_name} -> {username}")
          
        else:
          add_row(session, [discord_id, username, playerName])
          
          await ctx.respond(f"Associated Discord account with username: {username}")
          
      session.close()

@bot.slash_command(name="whoami", description="Shows who you are to the bot.")
async def store(ctx: discord.ApplicationContext):
  discord_id = ctx.author.id
  discord_name = ctx.author.name
  session = create_connection()
  with session:
    table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
    
    if(table.shape[0] > 0):
      embed = discord.Embed(color = discord.Color(0xBD9523))
        
      embed.title = discord_name
      
      embed.add_field(name = "Forum User", value = table.iloc[0]['username'], inline = True)
      
      embed.add_field(name = "Player", value = table.iloc[0]['player'], inline = False)
      
      await ctx.respond(embed = embed)
      
    else:
      await ctx.respond("You have no user stored. Use /store to store your forum username.")
      
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


