import discord
from discord.ext import commands
from dotenv import load_dotenv
import os # default module
import asyncio
import requests
from db_utils import *

load_dotenv('.secrets/.env') # load all the variables from the env file
TOKEN = os.getenv('DISCORD_V3_TOKEN')
OWNER_ID = int(os.getenv('DISCORD_OWNER'))

bot = commands.Bot(command_prefix="/", intents=discord.Intents.all())

@bot.event
async def on_ready():
    print("The Bot is now ready!")
    try: 
        synced_commands = await bot.tree.sync()
        print(f"Synced {len(synced_commands)} commands.")
    except Exception as e:
        print("An error with syncing application commands has occurred. ", e)

@bot.command()
async def hello(ctx):
    await ctx.send(f"Hello there, {ctx.author.mention}")


## SLASH COMMANDS
@bot.tree.command(name = "store", description="Store your forum username and connect it to your current player.")
async def store(interaction: discord.Interaction, *, username: str):
    discord_id = interaction.user.id

    # Get player information from username via the SSL API
    player = requests.get('https://api.simulationsoccer.com/player/getPlayer?username=' + username.replace(" ", "%20"))
    playerData = pd.DataFrame(eval(player.content))
    if(playerData.shape[1] < 2):
        await interaction.response.send_message("Please check the spelling of the username as none was found with that name.")
        return
    else:
        playerName = playerData.iloc[0]['name']
        session = create_connection()
        with session:
            table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
            if(table.shape[0] > 0):
                old_name = table["username"][0]
                update_row(session, [discord_id, username, playerName, discord_id])
                await interaction.response.send_message(f"Changed association from: {old_name} -> {username}")
            else:
                add_row(session, [discord_id, username, playerName])
                await interaction.response.send_message(f"Associated Discord account with username: {username}")
            
        session.close()

@bot.tree.command(name="whoami", description="Shows who the Bot thinks you are.")
async def whoami(interaction: discord.Interaction):
    discord_id = interaction.user.id
    discord_user = interaction.user.name

    session = create_connection()
    with session:
        table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
        if(table.shape[0] > 0):
            embed = discord.Embed(color = discord.Color(0xBD9523))
            embed.title = discord_user
            embed.add_field(name = "Forum User", value = table.iloc[0]['username'], inline = True)
            embed.add_field(name = "Player", value = table.iloc[0]['player'], inline = False)
            await interaction.response.send_message(embed = embed)
        else:
            await interaction.response.send_message("You have no user stored. Use /store to store your forum username.")
    session.close()

@bot.tree.command(name="reload", description="Reloading named cogs")
async def reload(interaction: discord.Interaction, extension: str):
    if interaction.user.id != OWNER_ID:
        await interaction.response.send_message("You do not have permission to use this command.")
        return
    else:
        await bot.unload_extension(f'cogs.{extension}')
        await bot.load_extension(f'cogs.{extension}')
        await interaction.response.send_message("Extension is being reloaded.")

## COGS AND STARTUP OF THE BOT
async def load():
    for filename in os.listdir("./cogs"):
        if filename.endswith(".py"):
            await bot.load_extension(f"cogs.{filename[:-3]}")

async def main():
    async with bot:
        await load()
        await bot.start(TOKEN)

asyncio.run(main())
