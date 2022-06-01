require(googlesheets4)
require(dplyr)

exposedPlayers <- 
  playerData %>% 
  filter(
    !(Name %in% 
      c(
        #ATH 7
        "Henrik Lind",
        "Jannik Andersen",
        "Deedee Yoker",
        "Zinedine Gintonic",
        "Eirlys Snooks",
        "Cal Labovitch",
        "Nico Fischer",
        
        #CBA 7
        "Laurent Gourcuff",
        "Antonio Governo",
        "Siegward OfCatarina",
        "Patrik Björkås",
        "Aimo Tälli",
        "Jaume Vila",
        "Koschei Oakdown",
        
        #CAI 7
        "Connor Azpilicueta",
        "Franco Torres",
        "Powdered ToastMan",
        "Ask Jeeves",
        "Alessio Calvatore",
        "Pierre Houde",
        "Pascal Anvierre",
        
        #CAT 7
        "Budget Busquets",
        "Tim Sinclair",
        "Shion Okamoto",
        "Juha Jarvinen",
        "Jökull Júlíusson",
        "Alexandra Gunnarsson",
        "Vins Vins",
        
        #HOL 9
        "Tiki Taka",
        "Hunter Jones",
        "Ricky Bobby",
        "Mikko Rashford",
        "Spack Jarrow",
        "Scott Sterling",
        "Hun Possible",
        "Gerald Gerrard",
        "Alfredo Puttanesca",
        
        #LON 7
        "Tony Yeboah",
        "Mazrim Taim",
        "Linnea Nesse",
        "Donna Rumma",
        'Bud-Lite "Large Box" McGuirk',
        "Gavin Millar",
        "K Clamence",
        "Kuai Liang",
        
        #MTL
        "Dr. Doofenshmirtz",
        "Giannis Kroustis",
        "Ioannis Papastathopoulos",
        "Siilver Druid",
        "Dorian Lexington",
        "Coffee Biscuit",
        'Makrus "The Tater" Jager',
        
        #SDY 8
        "Xherdan Xhaka",
        "Tim Possible",
        "Owen Forty-Four",
        "Sator Freddy",
        "Skoomina Hulk a Votto",
        "Certified Problem",
        "Hunter Havok",
        "Malakai Black",
        
        #SEO 8
        "George Costanza",
        "King Kong",
        "Ketchup Noodle",
        "Cole Mertz",
        "Jay Jay Okocha",
        "Marianne Maltais",
        "Michael Müller",
        "Hugh Mann",
        
        #TKY
        
        # Retiring
        "Icarus Isosceles",
        "Blue Cod",
        "Arcueid Brunestud"
      )),
    Team != "FA"
  ) %>% 
  relocate(
    Active,
    .after = Username
  ) %>% 
  arrange(
    Team,
    Active
  )

write_sheet(
  data = exposedPlayers,
  ss = "https://docs.google.com/spreadsheets/d/1ShsHsb-Nyf10-NMrEeMlvGjyw1DlTU7ScB-rSzLlTYw/edit#gid=0",
  sheet = "List"
)

