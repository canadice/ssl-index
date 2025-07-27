require(sslrtools)
require(googlesheets4)

playerData <- readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true"))

names <- c(
  "Nicolás Muñoz", "Sterling Scott", "Bengt Rubin", "Leo Fiachra", "Daryl McManus",
  "Louie Duck", "Valentin Morgenstern", "Bartholomew Lorequavius", "Misagh Khabiri",
  "Riamel Kloulechad", "Kuba Kunicki", "Hugh Mann", "Joga Bonito", "Aart de Trella",
  "Carolien Miedema", "Bartholomew Twinkletoes", "Ben Nelson", "Tre Quartista",
  "Ilya Prusikin", "Bernardo Fry", "Ljubica Kamenova", "Jordan Bamford",
  "Ali Dia", "Goku Atliketwopercent", "Gerd Klose",
  "Pete Martell", "Nathan Cormier", "Alexandros Mograine",
  "Duncan Walrusson", "Alex Calderon", "Hercule Hefeweizen",
  "Jalen Brooks", "Nikola Lovrić", "Alessandro Del Pirlo",
  "Bimitar Derbatov", "Brick Wall Jr.", "Florian Gaisberg", "Furious Chicken",
  "George Shaheen", "Marco Tentacles", "Mikko Rashford II", "Orlando Mastache Maldonado",
  "Santos Neymarinho", "Steven Urkel", "David Luiz Jr.", "Coin Flip", "Hippity Hoppity",
  "Emmanuel Blackman", "Andres Pedrillo", "Rigby Emerson", "Rodiano Santori",
  "Bob Kronkowski", "Dizzy Martin", "Daedalus Kronus", "Jürgen Müller",
  "Wang Zhihao", "Fara Dian", "Kimi Häkkinen", "Duncan Maxwell", "Clara Schmidt",
  "Maggie Sinclair", "William Williams", "Dina Skovgaard", "Cameron Millwall",
  "Zoe Clarke", "Erik Beermann", "Ryan Kirkpatrick", "Beaklie Eilish",
  "Zyqwarndalethron Velstrazyn-Smith", "Roquefort Cotswold", "Julian Rubio",
  "Benecio Aguilera II", "Slab Head", "Yoma Hashimoto", "João Peixoto", "Chef Gagne",
  "Zach Mulder", "Jude Greer", "Freja Ekholm-Gunnarsson", "Puma Superhoops",
  "Fernand Rivest", "Momo Adamu", "Zlatan Ibruhimovic", "Jia Yun", "Charlie Chambers",
  "Dalton Canders II", "Amore Delo", "Malachi Shturm"
)



exposedPlayers <- 
  playerData %>% 
  filter(
    !(name %in% names),
    !(team %in% c("Free Agent", "Academy", "Retired")),
    !status_p == 2
  ) %>% 
  select(
    username, userStatus, name, class, organization, tpe, tpebank, bankBalance, `left foot`:redistused
  ) %>% 
  arrange(
    organization,
    userStatus,
    tpe %>% desc()
  )

gs4_auth()

write_sheet(
  data = exposedPlayers,
  ss = "https://docs.google.com/spreadsheets/d/1ShsHsb-Nyf10-NMrEeMlvGjyw1DlTU7ScB-rSzLlTYw/edit#gid=0",
  sheet = "List"
)

