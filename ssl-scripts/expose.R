source("SSL-Index/app-documents/dataLoader.R")
require(googlesheets4)

exposedPlayers <- 
  playerData %>% 
  filter(
    !(Name %in% 
      c(
        #ATH 9
        "Henrik Lind",
        "Jannik Andersen",
        "Deedee Yoker",
        "Zinedine Gintonic",
        "Sydney Ramirez",
        "Cal Labovitch",
        "Nico Fischer",
        'Makrus "The Tater" Jager',
        "Jeffrey LaVert",
        
        #CBA 7
        "Laurent Gourcuff",
        # "Antonio Governo",
        # "Siegward OfCatarina",
        # "Patrik Björkås",
        # "Aimo Tälli",
        # "Jaume Vila",
        # "Koschei Oakdown",
        
        #CAI 8
        "Connor Azpilicueta",
        "Franco Torres",
        "Powdered ToastMan",
        "Ask Jeeves",
        "Alessio Calvatore",
        "Pierre Houde",
        "Wesley Stains",
        "Arnošt Hlemýžď",
        
        #CAT 7
        "Budget Busquets",
        # "Tim Sinclair",
        "Shion Okamoto",
        # "Juha Jarvinen",
        # "Jökull Júlíusson",
        # "Alexandra Gunnarsson",
        # "Vins Vins",
        
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
        # "Tony Yeboah",
        # "Mazrim Taim",
        # "Linnea Nesse",
        "Donna Rumma",
        # 'Bud-Lite "Large Box" McGuirk',
        # "Gavin Millar",
        # "K Clamence",
        # "Kuai Liang",
        
        #MTL 8
        "Alexander Eyesak",
        "Manny Calavera",
        "Ioannis Papastathopoulos",
        "Siilver Druid",
        "Dorian Lexington",
        "Coffee Biscuit",
        "Yves Mathieu",
        "Seth Dolan",
        
        #SDY 8
        "Xherdan Xhaka",
        "Tim Possible",
        # "Owen Forty-Four",
        # "Sator Freddy",
        # "Skoomina Hulk a Votto",
        # "Certified Problem",
        # "Hunter Havok",
        # "Malakai Black",
        
        #SEO 8
        "King Kong",
        "Ketchup Noodle",
        "Cole Mertz",
        "Jay Jay Okocha",
        "Hugh Mann",
        "Kjell Holmberg",
        "Berocka Aloisi",
        "Darren Lockyer",
        
        #TKY
        "Sky Ryze",
        "Mike Rup",
        
        #Paris
        "Leonidas Papadopoulos",
        
        #Accra
        "Kofi Anshah",
        
        # Retiring
        "Haruka Mayoi"
      )),
    !(Team %in% c("FA", "Prospect", "Retired"))
  ) %>% 
  relocate(
    Active,
    .after = Username
  ) %>% 
  arrange(
    Team,
    Active
  )

gs4_auth()

write_sheet(
  data = exposedPlayers,
  ss = "https://docs.google.com/spreadsheets/d/1ShsHsb-Nyf10-NMrEeMlvGjyw1DlTU7ScB-rSzLlTYw/edit#gid=0",
  sheet = "List"
)

