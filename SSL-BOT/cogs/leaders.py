import discord
from discord.ext import commands
import pandas as pd
import typing
import requests

class Leaders(commands.Cog): # create a class for our cog that inherits from commands.Cog
    # this class is used to create a cog, which is a module that can be added to the bot

    def __init__(self, bot): # this is a special method that is called when the cog is loaded
        self.bot = bot
        
        
    @discord.slash_command(name='classleaders', description='Shows the draft class leaders for a specific class (number)')
    async def classleaders(self, ctx: discord.ApplicationContext, season: typing.Optional[int] = None):
        if season is None:
          info = requests.get('https://api.simulationsoccer.com/player/getDraftClass')
          season = 17
          
        else: 
          info = requests.get('https://api.simulationsoccer.com/player/getDraftClass?class=' + str(season))
        
        # Data formatting
        data = pd.DataFrame(eval(info.content))
    
        embed = discord.Embed(color = discord.Color(0xBD9523))
        
        embed.title = 'S' + str(season) + ' Class Leaders'
        
        # TPE Leaders
        stat = data[["tpe", "name", "username"]].sort_values("tpe", ascending = False).head()
        
        stat.columns = stat.columns.str.upper()
        
        # Convert the DataFrame to a formatted string
        stat_string = stat.to_string(index=False)
        
        # Format the string to fit nicely in the embed
        formatted_stat_string = f"```\n{stat_string}\n```"
        
        embed.add_field(name = "", value = formatted_stat_string, inline = True)
          
        await ctx.respond(embed = embed)
        
def setup(bot): # this is called by Pycord to setup the cog
    bot.add_cog(Leaders(bot)) # add the cog to the bot
