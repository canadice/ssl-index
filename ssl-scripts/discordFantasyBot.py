
# cd ssl-bot-env
# source my_env/bin/activate
# nohup python discordStatBot.py &
# 8570

import os
import random
import requests
import io
import PIL.Image
import typing
import sqlite3
import pandas as pd
import numpy as np
from googleapiclient.discovery import build
import asyncio
from datetime import datetime
import difflib

import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import discord
from dotenv import load_dotenv

from discord.ext import commands

# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/spreadsheets.readonly']

load_dotenv(".secrets/.env")
TOKEN = os.getenv('DISCORD_FANTASY_TOKEN')
SERVER = os.getenv('SERVER_NAME')
BANK_SHEET = os.getenv('BANK_SHEET')
PLAYER_RANGE = os.getenv('PLAYER_SHEET')

intents = discord.Intents.all()

### Bot Functions
# def get_data(search_string, has_claim):
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
  
def create_fantasy_connection():
    """ create a database connection to the SQLite database
        specified by db_file
    :param db_file: database file
    :return: Connection object or None
    """
    conn = None
    try:
        conn = sqlite3.connect("database/discordBotFantasy.db")
    except Error as e:
        print(e)

    return conn
  
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

def get_group(discord_id):
  session = create_fantasy_connection()
  with session:
    table = pd.read_sql_query("SELECT * from groups WHERE discordID = " + str(discord_id), session)
    if(table.shape[0] > 0):
      group = table["fantasyGroup"][0]
    else:
      group = None
  session.close()
  
  return(group)

def get_pick(discord_id, group):
  session = create_fantasy_connection()
  with session:
    table = pd.read_sql_query("SELECT * from picks WHERE fantasyGroup = " + str(group), session)
    
    table = table[table["pickPlayer"].isnull()]
    if(table.shape[0] > 0):
      pick = table[["discordID", "pickNr"]].iloc[0]
    else:
      pick = None
  session.close()
  
  return(pick)

def get_picks(discord_id, group):
  session = create_fantasy_connection()
  with session:
    table = pd.read_sql_query("SELECT * from picks WHERE fantasyGroup = " + str(group), session)
    
    if(table.shape[0] > 0):
      picks = table[["user", "pickNr", "pickPlayer"]]
    else:
      picks = None
  session.close()
  
  return(picks)

def get_user_pick(discord_id, group):
  session = create_fantasy_connection()
  with session:
    table = pd.read_sql_query("SELECT * from picks WHERE fantasyGroup = " + str(group), session)
    
    table = table[table["pickPlayer"].isnull()]
    if(table.shape[0] > 0):
      pick = table["userPick"].iloc[0]
    else:
      pick = None
  session.close()
  
  return(pick)

def add_signup(conn, data):
    """
    Create a new row into the table
    :param conn:
    :param data:
    :return: row id
    """
    sql = ''' INSERT INTO signup(user, discordID, date)
              VALUES(?,?,?) '''
    cur = conn.cursor()
    cur.execute(sql, data)
    conn.commit()
    return cur.lastrowid
  
def add_pick(conn, data):
    """
    Create a new row into the table
    :param conn:
    :param data:
    :return: row id
    """
    sql = ''' UPDATE picks
              SET pickPlayer = ?
              WHERE (discordID = ? AND pickNr = ?)'''
    cur = conn.cursor()
    cur.execute(sql, data)
    conn.commit()
    return cur.lastrowid
  
def check_player(name):
  values = pd.read_csv("https://docs.google.com/spreadsheets/export?id={}&exportFormat=csv&gid={}".format(BANK_SHEET, PLAYER_RANGE))
  values = values[(values["Username"].notna()) & (values["Team Name"] != "Retired")]
  
  if(name.lower() in values["Player Name"].str.lower()):
    check = False
  else:
    suggestion = difflib.get_close_matches(name, values["Player Name"], n = 1)
    
    if len(suggestion) == 0:
        name = None
    else:
        name = suggestion[0]
    
    check = True
  
  return(check, name)


### Actual Bot Commands

bot = commands.Bot(command_prefix = '.', intents = intents)

@bot.event
async def on_ready():
    guild = discord.utils.get(bot.guilds)
    print(
      f'{bot.user.name} is connected to the following guild:\n'
      f'{guild.name}(id: {guild.id})'
    )
    
@bot.command(name='pick', help='Makes a pick for your fantasy team')
async def pick(ctx):
  discord_id = ctx.author.id

  # name = get_name(discord_id)
  # 
  # if name is None:
  #     await ctx.send("You have no user associated with this account, use !claim to associate one!")

  group = get_group(discord_id)

  if group is None:
      await ctx.send("<@{ctx.author.id}>, you have not been placed in a group. Please contact the Fantasy Manager.")

  current_pick, pick = get_pick(discord_id, group)

  if pick is None:
      await ctx.send("<@{ctx.author.id}>, you have no more picks available.")
  elif not current_pick == discord_id:
      await ctx.send("<@{ctx.author.id}>, it is not your pick to make.")
  else:
      await ctx.send(f'<@{ctx.author.id}>, which player do you want to select for pick number {pick} in group {group}.')
    
      def check(m):
          return m.author == ctx.author and m.channel == ctx.channel
    
      try:
          player = await bot.wait_for('message', check=check, timeout = 30.0)
      except asyncio.TimeoutError:
          await ctx.send(f'<@{ctx.author.id}> your pick timed out. Please try again.')
      else:
          ## Checks if the player exists in the league (spelling)
          ## returns true if player does not exist, with the closest match
          check, choice = check_player(player.content)
          
          if choice is None:
              await ctx.send(f'<@{ctx.author.id}>, there is no player resembling that name. Please start over from !pick and check the spelling.')
          else:
              await ctx.send(f'<@{ctx.author.id}>, do you want to select: "{choice}"? (yes/no)')
              
              def check2(m):
                  return m.author == ctx.author and m.channel == ctx.channel
              
              try:
                  prompt = await bot.wait_for('message', check=check2, timeout = 30.0)
              except asyncio.TimeoutError:
                  await ctx.send(f'<@{ctx.author.id}> your prompt timed out. Please start again with !pick.')
              else:
                  made_picks = get_picks(discord_id, group)
                  made_picks = made_picks[made_picks["pickPlayer"].notna()]["pickPlayer"]
                  
                  if((prompt.content.lower() in ["yes", "y"]) & (not made_picks.str.contains(choice).any())):
                      session = create_fantasy_connection()
          
                      with session:
                          add_pick(session, [choice, discord_id, pick])
                        
                      session.close()
                      
                      await ctx.send(f'You have picked {choice} as your selection.')
                      
                      next_pick, pick = get_pick(discord_id, group)
                      await ctx.send(f'<@{next_pick}>, you are up with pick number {pick}.')
                  elif (made_picks.str.contains(choice).any()):
                      await ctx.send(f'<@{ctx.author.id}>, the player has already been selected. Please start again with !pick.')
                  else: 
                      await ctx.send(f'<@{ctx.author.id}>, please start again with !pick and check the spelling of the player.')


@bot.command(name='signup', help='Signs you up for the current Fantasy Season')
async def signup(ctx):
  
  discord_id = ctx.author.id
  
  date = datetime.today().strftime('%Y-%m-%d')
  
  session = create_fantasy_connection()
  
  with session:
      table = pd.read_sql_query("SELECT * from signup WHERE discordID = " + str(discord_id), session)
      
      if(table.shape[0] > 0):
        user = table["user"][0]
        
        await ctx.send(f"<@{ctx.author.id}>, you have already signed up with the username ''{user}''. Please contact your Fantasy Manager if this is incorrect.")
        
      else:
        await ctx.send('<@{ctx.author.id}>, what is your username on the forum?')

        def check(m):
          return m.author == ctx.author and m.channel == ctx.channel
    
        try:
          username = await bot.wait_for('message', check=check, timeout = 10.0)
          
        except asyncio.TimeoutError:
          await ctx.send(f'<@{ctx.author.id}>, your pick timed out.')
          
        else:
          await ctx.send(f'<@{ctx.author.id}>, you have now signed up for Fantasy with the username: {username.content}!')
            
          add_signup(session, [username.content, discord_id, date])
        
  session.close()
  
@bot.command(name='group', help='Shows the picks made for your Fantasy Group')
async def group(ctx):
  discord_id = ctx.author.id

  # name = get_name(discord_id)
  # 
  # if name is None:
  #     await ctx.send("You have no user associated with this account, use !claim to associate one!")

  group = get_group(discord_id)

  if group is None:
      await ctx.send("<@{ctx.author.id}>, you have not been placed in a group. Please contact the Fantasy Manager.")

  picks = get_picks(discord_id, group)

  if picks is None:
      await ctx.send("This is a weird error.")
  else:
    
      embed = discord.Embed(color = discord.Color.from_str("#BD9523"))
    
      embed.title = "Group " + str(group)
      
      embed.set_thumbnail(url = "https://cdn.discordapp.com/attachments/1001211146159792239/1020739550588456960/league-logo.png")
      
      picked = picks[picks["pickPlayer"].notnull()][["pickNr", "user", "pickPlayer"]]
      
      if picked.empty: 
          picked = "No players have been picked."
      else: 
          picked["pickNr"] = '**' + picked["pickNr"].astype(str) + '**:'
          picked["user"] = picked["user"].astype(str) + ' - '
          picked = picked.to_csv(index = False, header = False, sep = "\t")
      
      embed.add_field(name = "Picked Players", value = picked, inline = False)
      
      remaining = picks[picks["pickPlayer"].isnull()][["pickNr", "user", "pickPlayer"]]
      if remaining.empty:
          remaining = "No players remain  to be picked."
      else: 
          remaining["pickNr"] = '**' + remaining["pickNr"].astype(str) + '**:'
          remaining = remaining.to_csv(index = False, header = False, sep = "\t")
      
      embed.add_field(name = "Remaining Picks", value = remaining, inline = False)
      
      await ctx.send(embed = embed)
   
bot.run(TOKEN)

