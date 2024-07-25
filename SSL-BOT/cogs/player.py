import discord
from discord.ext import commands
import pandas as pd
import typing
import requests

class Player(commands.Cog): # create a class for our cog that inherits from commands.Cog
    # this class is used to create a cog, which is a module that can be added to the bot

    def __init__(self, bot): # this is a special method that is called when the cog is loaded
        self.bot = bot
        
    @staticmethod
    def playerStatsEmbed(data: pd.DataFrame) -> discord.Embed:
  
        embed = discord.Embed(color = discord.Color(0xBD9523))
        
        embed.title = data.iloc[0]['name']
        
        embed.set_thumbnail(url = "https://cdn.discordapp.com/attachments/1001211146159792239/1020739550588456960/league-logo.png")
        
        embed.add_field(name = "TPE", value = data.iloc[0]['tpe'], inline = True)
        
        return(embed)
        
    @discord.slash_command(name='player', description='Gets player information')
    async def player(self, ctx: discord.ApplicationContext, season: typing.Optional[int] = None, *, name: typing.Optional[str] = None):
        if name is None:
          await ctx.respond("You need to provide a name.")
        
        info = requests.get('https://api.simulationsoccer.com/player/getPlayer?name=' + name.replace(" ", "%20"))
        
        # Data formatting
        data = pd.DataFrame(eval(info.content))
    
        embed = self.playerStatsEmbed(data)
          
        await ctx.respond(embed = embed)
        
    @discord.slash_command(name='bank', description='Gets player bank information')
    async def bank(self, ctx: discord.ApplicationContext, name: typing.Optional[str] = None):
        if name is None:
          await ctx.respond("You need to provide a name.")
        
        balance = requests.get('https://api.simulationsoccer.com/bank/getBankBalance?name=' + name.replace(" ", "%20"))
        transactions = requests.get('https://api.simulationsoccer.com/bank/getBankHistory?name=' + name.replace(" ", "%20"))
        
        # Data formatting
        balancedata = pd.DataFrame(eval(balance.content))
        transactiondata = pd.DataFrame(eval(transactions.content))
        
        if transactiondata.empty:
          await ctx.respond("This player does not have any bank information. Check the spelling.")
        else: 
          embed = discord.Embed(color = discord.Color(0xBD9523))
          
          embed.title = name
          
          embed.add_field(name = "Bank Balance", value = balancedata["balance"].iloc[0])
          
          # Latest transactions
          stat = transactiondata.sort_values("Time", ascending = False).head()
          
          stat.columns = stat.columns.str.upper()
          
          # Convert the DataFrame to a formatted string
          stat_string = stat.to_string(index=False)
          
          # Format the string to fit nicely in the embed
          formatted_stat_string = f"```\n{stat_string}\n```"
          
          embed.add_field(name = "Latest Transactions", value = formatted_stat_string, inline = False)
      
          await ctx.respond(embed = embed)
        
def setup(bot): # this is called by Pycord to setup the cog
    bot.add_cog(Player(bot)) # add the cog to the bot
