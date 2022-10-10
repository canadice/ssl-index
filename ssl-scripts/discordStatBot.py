
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

import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import discord
from dotenv import load_dotenv

from discord.ext import commands

# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/spreadsheets.readonly']

load_dotenv(".secrets/.env")
TOKEN = os.getenv('DISCORD_TOKEN')
SERVER = os.getenv('SERVER_NAME')
BANK_SHEET = os.getenv('BANK_SHEET')
PLAYER_RANGE = os.getenv('PLAYER_SHEET')

intents = discord.Intents.all()

### Bot Functions
# def get_data(search_string, has_claim):
def get_data(search_string):
    values = pd.read_csv("https://docs.google.com/spreadsheets/export?id={}&exportFormat=csv&gid={}".format(BANK_SHEET, PLAYER_RANGE))
    values = values[values["Username"].notna()]
    return_value = []
    if search_string.lower() not in values["Player Name"].to_string().lower():
        print('No data found.')
    else:
        row = values[values["Player Name"].str.lower() == search_string.lower()]
        
        return_value = row[["Username", "Player Name", "Balance"]]

    return (return_value, values)
  
def get_team_data(search_string):
    values = pd.read_csv("https://docs.google.com/spreadsheets/export?id={}&exportFormat=csv&gid={}".format(BANK_SHEET, PLAYER_RANGE))
    values = values[values["Username"].notna()]
    return_value = []
    if search_string.lower() not in values["Team Name"].to_string().lower():
        print('No data found.')
    else:
        row = values[values["Team Name"].str.lower() == search_string.lower()]
        
        return_value = row[["Username", "Player Name", "Balance"]]
        
    return return_value
  
def calculate_balance_rank(search_name, all_rows):
    search_name = search_name.str.lower().to_string(index = False)
  
    users = []
    usernames = []
    for index, row in all_rows.iterrows():
      balance = int(row[4].replace("$","").replace(",",""))
      username = row[1]
      if username not in usernames:
        usernames.append(username)
        user_data = (balance, username)
        users.append(user_data)
    
    users.sort(reverse=True)
    
    users = pd.DataFrame(users, columns = ["Balance", "Username"])
    
    total_users = users.shape[0]
    
    rank = np.where(search_name == users["Username"].str.lower())[0][0]
    
    percentile = round(round((total_users - rank) / total_users, 4)*100,2) 
    
    return(rank, percentile)
  
def playerStatsEmbed(data, desc):
  
    embed = discord.Embed(color = discord.Color.from_str("#BD9523"))
    
    embed.title = data.iloc[0]['Name']
    
    embed.description = desc
    
    embed.set_thumbnail(url = "https://cdn.discordapp.com/attachments/1001211146159792239/1020739550588456960/league-logo.png")
    
    if data.iloc[0]['Position'] == "GK":
      basic = data.filter(items = ["Apps", "Won","Lost", "Clean Sheets", "Player of the Match"]).melt()
      basic['variable'] = '**' + basic['variable'] + '**:'
      
      key = data.filter(items = ["Conceded", "Saves Parried", "Saves Held", "Saves Tipped"]).melt()
      key['variable'] = '**' + key['variable'] + '**:'
      
      advanced = data.filter(items = ["Save%", "xSave%", "Average Rating"]).melt()
      advanced['variable'] = '**' + advanced['variable'] + '**:'
      
      embed.add_field(name = "Basic", value = basic.to_csv(index = False, header = False, sep = "\t"), inline = True)
      embed.add_field(name = "Key", value = key.to_csv(index = False, header = False, sep = "\t"), inline = True)
      embed.add_field(name = '\u200b', value = '\u200b', inline = False)
      embed.add_field(name = "Advanced", value = advanced.to_csv(index = False, header = False, sep = "\t"), inline = True)
    else:
      basic = data.filter(items = ["Apps", "Goals","Assists","Player of the Match"]).melt()
      basic['variable'] = '**' + basic['variable'] + '**:'
      
      key = data.filter(regex = "Key|Chances").melt()
      key['variable'] = '**' + key['variable'] + '**:'
      
      defensive = data.filter(items = ["Interceptions", "Clearances", "Fouls", "Yellow Cards", "Red Cards"]).melt()
      defensive['variable'] = '**' + defensive['variable'] + '**:'
      
      advanced = data.filter(items = ["xG", "Average Rating"]).melt()
      advanced['variable'] = '**' + advanced['variable'] + '**:'
      
      embed.add_field(name = "Basic", value = basic.to_csv(index = False, header = False, sep = "\t"), inline = True)
      embed.add_field(name = "Key", value = key.to_csv(index = False, header = False, sep = "\t"), inline = True)
      embed.add_field(name = '\u200b', value = '\u200b', inline = False)
      embed.add_field(name = "Defensive", value = defensive.to_csv(index = False, header = False, sep = "\t"), inline = True)
      embed.add_field(name = "Advanced", value = advanced.to_csv(index = False, header = False, sep = "\t"), inline = True)
  
    return(embed)
  
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


### Actual Bot Commands

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
    
@bot.command(name='trivia', help='Responds with a random trivia from the SSL')
async def nine_nine(ctx):
    ssl_trivia = [
        'Seoul Mythic FC used to be located in Rio de Janeiro as FC Rio.',
        'Pingu Nootazuki (@Pingu) became the first user to retire their player.',
        'The shape and symbols of the Shield trophy is an homage to the original 6 teams in the league.',
        'The league was originally founded by @Playoff Lonzo, @siddhus and @FriendlyHermit in the fall of 2021.',
        'The first player ever created was Radek Soboda (@Will3).',
        'The inaugural manager of Cairo City was @Will3.',
        'The inaugural manager of FC Rio was @lukechezzwoo.',
        'The inaugural manager of Athênai F.C. was @Canadice.',
        'The inaugural manager of Hollywood FC was @CJTheM16.',
        'The inaugural manager of Inter London was @frazzle14.',
        'The inaugural manager of Tokyo S.C. was @Pingu.',
        'CF Catalunya and Montréal United were the first expansion teams in season 2.',
        "The first SSL Goal ever scored was by Athênai F.C.'s A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ® in the 43rd minute of MD1 in Season 1.",
        'The first team to win all major trophies (League, Cup, and Shield) was Hollywood FC after winning the S4 Founders Shield.'
      ]

    response = random.choice(ssl_trivia)
    await ctx.send(response)
    
@bot.command(name='player', help='Gets player information', aliases = ['p'])
async def player(ctx, season: typing.Optional[int] = None, *, name: typing.Optional[str] = None):
    if name is None:
      discord_id = ctx.author.id
      
      name = get_name(discord_id)
    
    if name is None:  
      await ctx.send("You have no name associated with this account, use !claim to associate one!")
    
    log = requests.get('http://143.198.159.1/ssl/playerLog?player=' + name.replace(" ", "%20"))
    
    if season is None:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20"))
      
      description = "Accumulated statistics during the season"
    else:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&season=' + str(season))
      
      description = "Accumulated statistics during season " + str(season)
    
    # Player log formatting
    log = log.text.replace("[1] ", "").replace('"', '').replace('\\t', '\t').replace('\\n', '\n')
    
    log = '```' + log + '```'
    
    # Data formatting
    data = pd.DataFrame(eval(stats.content))

    embed = playerStatsEmbed(data, description)
      
    embed.add_field(name = "Five latest games", value = log, inline = False)
    
    embed.set_footer(text = "For specific statistics from a competition, try !player1, !player2, or !playerCup.")
    
    await ctx.send(embed = embed)
    
@bot.command(name='player1', help='Gets player information from Division 1', aliases = ['p1'])
async def player(ctx, season: typing.Optional[int] = None, *, name: typing.Optional[str] = None):
    if name is None:
      discord_id = ctx.author.id
      
      name = get_name(discord_id)
    
    if name is None:  
      await ctx.send("You have no name associated with this account, use !claim to associate one!")
    
    log = requests.get('http://143.198.159.1/ssl/playerLog?player=' + name.replace(" ", "%20"))
    
    if season is None:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&league=1')
            
      description = "Accumulated statistics during the season in Division 1."
    else:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&season=' + str(season) + '&league=1')
            
      description = "Accumulated statistics during season " + str(season) + " in Division 1."
    
    # Player log formatting
    log = log.text.replace("[1] ", "").replace('"', '').replace('\\t', '\t').replace('\\n', '\n')
    
    log = '```' + log + '```'
    
    # Data formatting
    data = pd.DataFrame(eval(stats.content))
    
    embed = playerStatsEmbed(data, description)
      
    embed.add_field(name = "Five latest games", value = log, inline = False)
    
    await ctx.send(embed = embed)
    
@bot.command(name='player2', help='Gets player information from Division 2', aliases = ['p2'])
async def player(ctx, season: typing.Optional[int] = None, *, name: typing.Optional[str] = None):
    if name is None:
      discord_id = ctx.author.id
      
      name = get_name(discord_id)
    
    if name is None:  
      await ctx.send("You have no name associated with this account, use !claim to associate one!")
      
    log = requests.get('http://143.198.159.1/ssl/playerLog?player=' + name.replace(" ", "%20"))
    
    if season is None:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&league=2')
      
      description = "Accumulated statistics during the season in Division 2."
    else:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&season=' + str(season) + '&league=2')
      
      description = "Accumulated statistics during season " + str(season) + " in Division 2."
    
    # Player log formatting
    log = log.text.replace("[1] ", "").replace('"', '').replace('\\t', '\t').replace('\\n', '\n')
    
    log = '```' + log + '```'
    
    # Data formatting
    data = pd.DataFrame(eval(stats.content))
    
    embed = playerStatsEmbed(data, description)
      
    embed.add_field(name = "Five latest games", value = log, inline = False)
    
    await ctx.send(embed = embed)
    
@bot.command(name='playerCup', help='Gets player information from the Cup', aliases = ['pc'])
async def player(ctx, season: typing.Optional[int] = None, *, name: typing.Optional[str] = None):
    if name is None:
      discord_id = ctx.author.id
      
      name = get_name(discord_id)
    
    if name is None:  
      await ctx.send("You have no name associated with this account, use !claim to associate one!")
    
    log = requests.get('http://143.198.159.1/ssl/playerLog?player=' + name.replace(" ", "%20"))
    
    if season is None:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&league=Cup')
      
      description = "Accumulated statistics during the season in the SSL Cup."
    else:
      stats = requests.get('http://143.198.159.1/ssl/playerStats?player=' + name.replace(" ", "%20") + '&season=' + str(season) + '&league=Cup')
      
      description = "Accumulated statistics during season " + str(season) + " in the SSL Cup."
    
    # Player log formatting
    log = log.text.replace("[1] ", "").replace('"', '').replace('\\t', '\t').replace('\\n', '\n')
    
    log = '```' + log + '```'
    
    # Data formatting
    data = pd.DataFrame(eval(stats.content))
    
    embed = playerStatsEmbed(data, description)
      
    embed.add_field(name = "Five latest games", value = log, inline = False)
    
    await ctx.send(embed = embed)
    
@bot.command(name='graph', help='Gets the attribute visualization of a chosen player.')
async def player(ctx, season: typing.Optional[int] = None, *, name: typing.Optional[str] = None):
    if name is None:
      discord_id = ctx.author.id
      
      name = get_name(discord_id)
    
    if name is None:  
      await ctx.send("You have no name associated with this account, use !claim to associate one!")
      
    if season is None:
      response = requests.get('http://143.198.159.1/ssl/playerGraphSimple?player=' + name.replace(" ", "%20"))
    else:
      response = requests.get('http://143.198.159.1/ssl/playerGraphSimple?player=' + name.replace(" ", "%20") + '&season=' + str(season))
    
    open('my.png', 'wb').write(response.content)
    
    file = discord.File("my.png", filename="image.png")
    
    embed = discord.Embed(color = discord.Color.from_str("#BD9523"))
    
    if not season is None:
      embed.description = "The attribute distribution during season " + str(season)
    
    embed.set_author(name = ctx.author.display_name)
    
    embed.set_image(url = "attachment://image.png")
    
    await ctx.send(file = file, embed = embed)

@bot.command(name='leaders', help='Shows league leaders in different statistics.', aliases = ["ldr", "l"])
async def player(ctx, league: typing.Optional[str] = None, season: typing.Optional[int] = None):
    if season is None and league is None:
      response = requests.get('http://143.198.159.1/ssl/leaders')
      
      description = "The league leaders in different categories."
    elif season is None:
      response = requests.get('http://143.198.159.1/ssl/leaders?league=' + league)
      
      description = "The league leaders in different categories in Division " + league + "."
    elif league is None:
      response = requests.get('http://143.198.159.1/ssl/leaders?season=' + str(season))
      
      description = "The league leaders in different categories during season " + str(season) + "."
    else:
      response = requests.get('http://143.198.159.1/ssl/leaders?league=' + league + '&season=' + str(season))
      
      description = "The league leaders in different categories during season " + str(season) + " in Division " + league + "."
  
    data = pd.DataFrame(eval(response.content))

    embed = discord.Embed(color = discord.Color.from_str("#BD9523"))
    
    embed.title = "League Leaders"
    
    embed.description = description
    
    embed.set_thumbnail(url = "https://cdn.discordapp.com/attachments/1001211146159792239/1020739550588456960/league-logo.png")
    
    # Goals
    stat = data[["Goals", "Name"]].sort_values("Goals", ascending = False).head()
    
    embed.add_field(name = "Goal ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # Assists 
    stat = data[["Assists", "Name"]].sort_values("Assists", ascending = False).head()
    
    embed.add_field(name = "Assist ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # xG
    stat = data[["xG", "Name"]].sort_values("xG", ascending = False).head()
    
    embed.add_field(name = "xG ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # Chances Created 
    stat = data[["Chances Created", "Name"]].sort_values("Chances Created", ascending = False).head()
    
    embed.add_field(name = "Chances ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # Player of the Match
    stat = data[["Player of the Match", "Name"]].sort_values("Player of the Match", ascending = False).head()
    
    embed.add_field(name = "PotM ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # Average Rating
    stat = data[["Average Rating", "Name"]].sort_values("Average Rating", ascending = False).head()
    
    embed.add_field(name = "Average Rating ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    ### Separator
    embed.add_field(name = "---------------------------", value = '\u200b', inline = False)
    
    # Wins
    stat = data[["Won", "Name"]].sort_values("Won", ascending = False).head()
    
    embed.add_field(name = "Wins ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # Clean Sheets
    stat = data[["Clean Sheets", "Name"]].sort_values("Clean Sheets", ascending = False).head()
    
    embed.add_field(name = "Clean Sheets ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    # Save%
    stat = data[["Save%", "Name"]].sort_values("Save%", ascending = False).head()
    
    embed.add_field(name = "Save% ", value = stat.to_csv(index = False, header = False, sep = "\t"), inline = True)
    
    await ctx.send(embed = embed)
    
@bot.command(name='bank', help='Shows bank balance.', aliases = ["b"])   
async def get_balance(ctx, *, name: typing.Optional[str] = None):
    if name is None:
      discord_id = ctx.author.id
      
      name = get_name(discord_id)
    
    if name is None:  
      await ctx.send("You have no name associated with this account, use !claim to associate one!")
    
    data, all_rows = get_data(name)
    
    player = data['Player Name'].to_string(index = False)
    
    balance = data["Balance"].to_string(index = False)
    
    new_embed = discord.Embed(title=f"Balance for {player}",color = discord.Color.from_str("#BD9523"))
    rank, percent = calculate_balance_rank(data["Username"], all_rows)
    new_embed.add_field(name=f"Total Balance", value=balance, inline=False)
    new_embed.add_field(name=f"Rank", value=rank, inline=False)
    new_embed.add_field(name=f"Percentile", value=f"{percent}", inline=False)

    # await message.channel.send(f"{print_string}")
    await ctx.send(embed=new_embed)
    
@bot.command(name='bankTeam', help='Shows bank balance for the entire team.', aliases = ["bt"])   
async def get_balance(ctx, *, name: typing.Optional[str] = None):
    if name is None:  
      await ctx.send("You need to select a team to get the balances of. Please write a team name after !bankTeam.")
      return
    
    data = get_team_data(name)
    
    data["BalanceInt"] = data["Balance"].str.replace("$","").str.replace(",","")
    data["BalanceInt"] = data["BalanceInt"].astype(int)
    
    data = data.sort_values("BalanceInt", ascending = False)
    
    users = data["Username"].to_string(index = False)
    
    players = data["Player Name"].to_string(index = False)
    
    balance = data["Balance"].to_string(index = False)
    
    new_embed = discord.Embed(title=f"Balance for {name}",color = discord.Color.from_str("#BD9523"))
    new_embed.add_field(name=f"User", value=users, inline=True)
    new_embed.add_field(name=f"Player", value=players, inline=True)
    new_embed.add_field(name=f"Total Balance", value=balance, inline=True)
    
    # await message.channel.send(f"{print_string}")
    await ctx.send(embed=new_embed)

@bot.command(name='claim', help='Claims a player name for the Discord user.', aliases = ["c"])   
async def claim(ctx, *, input_data: str):
    discord_id = ctx.author.id
    discord_user = ctx.author.name
    player_name = input_data
    if input_data == "":
        await ctx.send("Please provide a player name to claim")
        return
      
    session = create_connection()
    with session:
      table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
      
      if(table.shape[0] > 0):
        old_name = table["player"][0]
        
        update_row(session, [discord_id, discord_user, player_name, discord_id])
        
        await ctx.send(f"Changed association from: {old_name} -> {player_name}")
        
      else:
        add_row(session, [discord_id, discord_user, player_name])
        
        await ctx.send(f"Associated Discord account with name: {player_name}")
        
    session.close()

@bot.command(name='whoami', help='Shows the player the Discord user has claimed.', aliases = ["whois", "who"])   
async def who_am_i(ctx):
    discord_id = ctx.author.id
    session = create_connection()
    with session:
      table = pd.read_sql_query("SELECT * from discordUser WHERE discordID = " + str(discord_id), session)
      
      if(table.shape[0] > 0):
        old_name = table["player"][0]
        
        await ctx.send(f"This account is associated with the name: {old_name}")
        
      else:
        await ctx.send("You have no name associated with this account, use !claim to associate one!")
        
    session.close()
        
bot.run(TOKEN)

