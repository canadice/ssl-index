import discord
import os # default module
from dotenv import load_dotenv
import asyncio  # missing import
import pandas as pd

load_dotenv('.secrets/.env') # load all the variables from the env file
intents = discord.Intents.all()
TOKEN = os.getenv('DISCORD_v2_TOKEN')
SERVER = [os.getenv('DISCORD_SERVER')]

bot = discord.Bot(debug_guilds=SERVER)

@bot.event
async def on_ready():
    print(f"{bot.user} is ready and online!")


@bot.slash_command(name="reload", description="Reloading some cogs")
async def reload(ctx: discord.ApplicationContext, extension: str):
    bot.unload_extension(f'cogs.{extension}')
    bot.load_extension(f'cogs.{extension}')

@bot.slash_command(name="hello", description="Say hello to the bot")
async def hello(ctx: discord.ApplicationContext):
    await ctx.respond("Why are you the only command that shows up???")

cogs_list = [
    'trivia',
    'player'
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


