import discord
from discord.ext import commands
from discord import app_commands
import pandas as pd
import typing
import requests
import json

class Leaders(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener()
    async def on_ready(self):
        print(f"{__name__} is online!")
        
        
    @app_commands.command(name='classleaders', description='Shows the draft class leaders for a specific class (number)')
    async def classleaders(self, interaction: discord.Interaction, season: typing.Optional[int] = None):
        if season is None:
            info = requests.get('https://api.simulationsoccer.com/player/getDraftClass')
            leader = "Academy"
        else: 
            info = requests.get('https://api.simulationsoccer.com/player/getDraftClass?class=' + str(season))
            leader = 'S' + str(season)
        # Data formatting
        data = pd.DataFrame(json.loads(info.content))
        embed = discord.Embed(color = discord.Color(0xBD9523))
        embed.title = leader + ' Class Leaders'
        # TPE Leaders
        stat = data[["tpe", "name", "username"]].sort_values("tpe", ascending = False).head()
        stat.columns = stat.columns.str.upper()
        # Convert the DataFrame to a formatted string
        stat_string = stat.to_string(index=False)
        # Format the string to fit nicely in the embed
        formatted_stat_string = f"```\n{stat_string}\n```"
        embed.add_field(name = "", value = formatted_stat_string, inline = True)
        await interaction.response.send_message(embed = embed)
        
async def setup(bot):
    await bot.add_cog(Leaders(bot))
