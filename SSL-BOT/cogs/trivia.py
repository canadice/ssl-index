import discord
from discord.ext import commands
from discord import app_commands
import random

class Trivia(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener()
    async def on_ready(self):
        print(f"{__name__} is online!")
    
    # @commands.command()
    # async def ping(self, ctx):
    #     ping_embed = discord.Embed(title = "Ping", description="Latency in ms", color=discord.Color.blue())
    #     ping_embed.add_field(name=f"{self.bot.user.name}'s latency (ms):", value=f"{round(self.bot.latency * 1000)} ms.", inline=False)
    #     ping_embed.set_footer(text=f"Requested by {ctx.author.name}.", icon_url=ctx.author.avatar)
    #     await ctx.send(embed=ping_embed)

    @app_commands.command(name = "trivia", description="Responds with a random trivia from the SSL.")
    async def trivia(self, interaction: discord.Interaction):
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
        await interaction.response.send_message(response)

async def setup(bot):
    await bot.add_cog(Trivia(bot))