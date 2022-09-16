# bot.py
import os
import random
import requests
import io
import PIL.Image

import discord
from dotenv import load_dotenv

from discord.ext import commands

load_dotenv(".secrets/.env")
TOKEN = os.getenv('DISCORD_TOKEN')
SERVER = os.getenv('SERVER_NAME')

intents = discord.Intents.all()

bot = commands.Bot(command_prefix = '!', intents = intents)

@bot.event
async def on_ready():
    guild = discord.utils.get(bot.guilds, name=SERVER)
    print(
      f'{bot.user.name} is connected to the following guild:\n'
      f'{guild.name}(id: {guild.id})'
    )

@bot.command(name='99', help='Responds with a random quote from Brooklyn 99')
async def nine_nine(ctx):
    brooklyn_99_quotes = [
        'I\'m the human form of the 💯 emoji.',
        'Bingpot!',
        (
            'Cool. Cool cool cool cool cool cool cool, '
            'no doubt no doubt no doubt no doubt.'
        ),
    ]

    response = random.choice(brooklyn_99_quotes)
    await ctx.send(response)
    
@bot.command(name='player', help='Gets the latest game log data for your player.')
async def player(ctx, *, name: str):
    response = requests.get('http://143.198.159.1/ssl/playerLog?player=' + name.replace(" ", "%20"))
    
    reply = response.text.replace("[1] ", "").replace('"', '').replace('\\t', '\t').replace('\\n', '\n')
    
    reply = name + '\n\n' + reply
    
    reply = '```' + reply + '```'
    
    await ctx.send(reply)
    
@bot.command(name='graph', help='Gets the attribute visualization of a chosen player.')
async def player(ctx, *, name: str):
    response = requests.get('http://143.198.159.1/ssl/playerGraphSimple?player=' + name.replace(" ", "%20"))
    
    open('my.png', 'wb').write(response.content)
    
    file = discord.File("my.png", filename="image.png")
    
    embed = discord.Embed(color = discord.Color.red())
    
    embed.set_image(url = "attachment://image.png")
    
    await ctx.send(file = file, embed = embed)

bot.run(TOKEN)

