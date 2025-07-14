import discord
from discord.ext import commands
from discord import app_commands
import pandas as pd
import typing
import requests
import json 
from db_utils import *

class Player(commands.Cog): # create a class for our cog that inherits from commands.Cog
    # this class is used to create a cog, which is a module that can be added to the bot

    def __init__(self, bot): # this is a special method that is called when the cog is loaded
        self.bot = bot

    @commands.Cog.listener()
    async def on_ready(self):
        print(f"{__name__} is online!")
        
    @staticmethod
    def playerStatsEmbed(data: pd.DataFrame) -> discord.Embed:
        embed = discord.Embed(color = discord.Color(0xBD9523))
        embed.title = data.iloc[0]['name']
        embed.set_thumbnail(url = "https://cdn.discordapp.com/attachments/1001211146159792239/1020739550588456960/league-logo.png")
        embed.add_field(name = "TPE", value = data.iloc[0]['tpe'], inline = True)
        return(embed)
        
    @app_commands.command(name='player', description='Gets player information')
    async def player(self, interaction: discord.Interaction, *, name: typing.Optional[str] = None):
        if name is None:
          name = get_name(interaction.user.id)
        if name is None:  
          await interaction.response.send_message("You have no user stored. Use /store to store your forum username.")  
        else:
          info = requests.get('https://api.simulationsoccer.com/player/getPlayer?name=' + name.replace(" ", "%20"))
          # Data formatting
          data = pd.DataFrame(json.loads(info.content))
          embed = self.playerStatsEmbed(data)
          await interaction.response.send_message(embed = embed)
        
    @app_commands.command(name='bank', description='Gets player bank information')
    async def bank(self, interaction: discord.Interaction, name: typing.Optional[str] = None):
        if name is None:
          name = get_name(interaction.user.id)
        if name is None:  
          await interaction.response.send_message("You have no user stored. Use /store to store your forum username.")  
        else:
          balance = requests.get('https://api.simulationsoccer.com/bank/getBankBalance?name=' + name.replace(" ", "%20"))
          transactions = requests.get('https://api.simulationsoccer.com/bank/getBankHistory?name=' + name.replace(" ", "%20"))
          # Data formatting
          balancedata = pd.DataFrame(json.loads(balance.content))
          transactiondata = pd.DataFrame(json.loads(transactions.content))
          if transactiondata.empty:
            await interaction.response.send_message("This player does not have any bank information. Check the spelling.")
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
            await interaction.response.send_message(embed = embed)
            
    @app_commands.command(name='checklist', description='Returns the weekly TPE checklist')
    async def checklist(self, interaction: discord.Interaction, username: typing.Optional[str] = None):
        if username is None:
          username = get_username(interaction.user.id)
        if username is None:  
          await interaction.response.send_message("You have no user stored. Use /store to store your forum username.")  
        else:
          checklist = requests.get('https://api.simulationsoccer.com/player/tpeChecklist?username=' + username.replace(" ", "%20"))
          # Data formatting
          checklistdata = pd.DataFrame(json.loads(checklist.content))
          if checklistdata.empty:
            await interaction.response.send_message("This player does not have any checklist information. Check the spelling.")
          else: 
            # Group the data based on the 'posted' status
            posted_true = checklistdata[checklistdata['posted'] == True]
            posted_false = checklistdata[checklistdata['posted'] == False]
            embed = discord.Embed(color = discord.Color(0xBD9523))
            embed.title = username + ' Weekly TPE Checklist'
            # Add fields for each group
            if not posted_true.empty:
                posted_true_str = "\n".join([f"[{row['subject']}]({row['link']})" for index, row in posted_true.iterrows()])
                embed.add_field(name="Completed", value=posted_true_str, inline=False)
            if not posted_false.empty:
                posted_false_str = "\n".join([f"[{row['subject']}]({row['link']})" for index, row in posted_false.iterrows()])
                embed.add_field(name="Remaining", value=posted_false_str, inline=False)
            await interaction.response.send_message(embed = embed)
            
    @app_commands.command(name='teamchecklist', description='Returns the weekly TPE checklist for all users on the team')
    async def teamchecklist(self, interaction: discord.Interaction, username: typing.Optional[str] = None):
        await interaction.response.defer()  # Defer immediately to avoid timeout
        try:
            if username is None:
                username = get_username(interaction.user.id)
            if username is None:
                await interaction.followup.send("You have no user stored. Use /store to store your forum username.")
                return
            checklist = requests.get('https://api.simulationsoccer.com/player/teamTPEChecklist?username=' + username.replace(" ", "%20"))
            checklist.raise_for_status()  # Raise error for bad HTTP status
            checklistdata = pd.DataFrame(json.loads(checklist.content))
            if checklistdata.empty:
                await interaction.followup.send("This user is not part of a team. Check the spelling.")
                return
            grouped = checklistdata.groupby('subject')
            embed = discord.Embed(color = discord.Color(0xBD9523))
            embed.title = 'Uncompleted Weekly Tasks'
            for subject, rows in grouped:
                posted_false_users = rows[rows['posted'] == False]
                posted_false_str = "\n".join([f"{row['user']}" for index, row in posted_false_users.iterrows()])
                embed.add_field(name=f"**{subject}**", value = posted_false_str or "All complete!", inline=False)
                embed.add_field(name="", value = f"[Link to task]({rows['link'].iloc[0]})")
            await interaction.followup.send(embed = embed)
        except Exception as e:
            await interaction.followup.send(f"An error occurred: {e}")
        
async def setup(bot): # this is called by Pycord to setup the cog
    await bot.add_cog(Player(bot)) # add the cog to the bot
