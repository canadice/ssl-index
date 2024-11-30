import discord
from discord.ext import commands
import random

class Trivia(commands.Cog): # create a class for our cog that inherits from commands.Cog
    # this class is used to create a cog, which is a module that can be added to the bot

    def __init__(self, bot): # this is a special method that is called when the cog is loaded
        self.bot = bot
        
    @discord.slash_command(name='trivia', description='Responds with a random trivia from the SSL')
    async def trivia(self, ctx: discord.ApplicationContext):
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
            "The first SSL goal ever scored was by Athênai F.C.'s A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ® in the 43rd minute of MD1 in Season 1.",
            'The first team to win all major trophies (League, Cup, and Shield) was Hollywood FC after winning the season 4 Founders Shield.',
            'The fastest goal scored in an SSL League match was Antonio Governo (CABA) after 9 seconds in a 6-1 victory over Romana in season 7 of Division 2.',
            'The fastest sending off in an SSL League match was Siilver Druid (MTL) after 04:54 in a 1-3 loss to Cairo in season 3.'
          ]
    
        response = random.choice(ssl_trivia)
        await ctx.respond(response)
      
    @discord.slash_command(name='nine_nine', description='Responds with a random quote')
    async def nine_nine(self, ctx: discord.ApplicationContext):
        brooklyn_99_quotes = [
            "No doubt, no doubt, no doubt.",
            "Cool, cool, cool, cool, cool, cool, cool, cool, cool, cool, cool, cool.",
            "I'm not superstitious, but I am a little stitious.",
            "To the left! To the left!",
            "Nine-Nine!",
            "I am the human form of the 100 emoji.",
            "I’m the captain now.",
            "Boyle, do you know what this is? It's a nuzzle hug!",
            "If you could just stop being you for a second, that would be great.",
            "The only thing I care about is the fact that my partner is a human disaster.",
            "You’re the best partner in the world.",
            "I’m a little bit of a psycho, but I’m a fun psycho.",
            "I’ve got a million ideas. The problem is, most of them are terrible.",
            "We’re going to make it through this, one ridiculous joke at a time.",
            "Terry loves yogurt!",
            "The only thing that matters is that we are together.",
            "Jake Peralta: I have an idea. Why don’t you go to a restaurant, and while you’re there, you can think about what you did.",
            "Amy Santiago: I’m going to need a moment to process this. And then I’m going to need another moment to cry.",
            "Captain Holt: I’m not a robot. I’m a human who feels emotions. Although, they’re not as strong as yours.",
            "Rosa Diaz: I am not going to say anything, because I don’t want to reveal that I am an emotional wreck inside.",
            "Gina Linetti: I’m just going to keep doing what I do best: making things weird."
          ]
    
        response = random.choice(brooklyn_99_quotes)
        await ctx.respond(response)
    
def setup(bot): # this is called by Pycord to setup the cog
    bot.add_cog(Trivia(bot)) # add the cog to the bot
