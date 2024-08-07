use portaldb;

UPDATE portaldb.playerdata SET status_p = 1 where pid = 40;

set global local_infile = 1;

CREATE TABLE IF NOT EXISTS organizations (
	id INTEGER PRIMARY KEY,
    name TEXT
);

INSERT INTO organizations (id, name) VALUES 
(0, "Buenos Aires"),
(1, "Tokyo"),
(2, "Hollywood"),
(3, "Catalunya"),
(4, "Romana"),
(5, "São Paulo"),
(6, "Reykjavik"),
(7, "Schwarzwälder");

CREATE TABLE IF NOT EXISTS teams (
	orgID INTEGER,
    teamID INTEGER PRIMARY KEY,
    name TEXT,
    established INTEGER,
    affiliate INTEGER,
    abbreviation CHAR(4),
    primaryColor VARCHAR(7),
    secondaryColor VARCHAR(7),
    tertiaryColor VARCHAR(7),
    alternateColor VARCHAR(7),
    city TEXT,
    stadium TEXT,
    FOREIGN KEY (orgID) REFERENCES organizations(id)
);

INSERT INTO teams (teamID, name, orgID, established, affiliate, abbreviation, primaryColor, secondaryColor, tertiaryColor, alternateColor, city, stadium) VALUES
(1, 'Athênai F.C.', 0, 1, 2,'ATH', '#283D70', '#C6934B', '#E0DDD5', NULL, 'Athens, Greece', 'Exarcheia Stadion'),
(2, 'Cairo City', 1, 1, 2,'CAI', '#F7B200', '#F5FCF2', '#462B14', NULL, 'Cairo, Egypt', 'Tomb of Doom'),
(3, 'FC Rio', NULL, 1, NULL,'RIO', '#15335b', '#b8d1ec', '#DF9034', NULL, 'Rio de Janeiro, Brazil', NULL),
(4, 'Hollywood FC', 2, 1, 1, 'HOL', '#0a1b33', '#EBA51F', '#3B73B9', NULL, 'Los Angeles, USA', 'The Pitch'),
(5, 'Inter London', 4, 1, 2, 'LON', '#196DBF', '#ffffff', '#000000', NULL, 'London, England', 'Boris Johnson''s Partydome'),
(6, 'Tokyo S.C.', 1, 1, 1, 'TOK', '#18A68E', '#FDC47D', '#DA4839', NULL, 'Tokyo, Japan', 'Nintendome'),
(7, 'Montréal United', 7, 2, 2, 'MTL', '#0B191E', '#499878', '#D8B65C', NULL, 'Montréal, Canada', 'Stade Fleur-de-lis'),
(8, 'CF Catalunya', 3, 2, 1, 'CAT', '#DD2025', '#FFFFFF', '#F8DE0D', NULL, 'Barcelona, Spain', 'Camp de la Indústria'),
(9, 'Seoul MFC', 3, 3, 2, 'SEO', '#1B4522', '#74B4BE', '#AE1E2A', NULL, 'Seoul, South Korea', 'Seoul Civic Stadium'),
(10, 'CA Buenos Aires', 0, 3, 1, 'CABA', '#E73F02', '#DCBC2A', '#FFFFFF', NULL, 'Buenos Aires, Argentina', 'Estadio de Solens'),
(11, 'Sydney City', NULL, 3, NULL, 'SYD', '#340936', '#FFE204', '#FFFFFF', NULL, 'Sydney, Australia', NULL),
(12, 'AS Paris', 5, 4, 2, 'PAR', '#004170', '#DA291C', '#FFFFFF', NULL, 'Paris, France', 'Stade de Rêves'),
(13, 'Adowa Accra FC', NULL, 4, NULL, 'ACC', '#eb9f15', '#0f6f70', NULL, NULL, 'Accra, Ghana', 'Accra Arena'),
(14, 'União São Paulo', 5, 5, 1, 'USP', '#e52525', '#FFFFFF', '#000000', NULL, 'São Paulo, Brazil', 'Estádio de São Paulo'),
(15, 'F.C. Kaapstad', 2, 5, 2, 'FCK', '#02492a', '#ffb612', NULL, NULL, 'Cape Town, South Africa', 'Nelson Mandela Stadium'),
(16, 'A.C. Romana', 4, 7, 1, 'ACR', '#5C1466', '#D4AF37', '#000000', NULL, 'Rome, Italy', 'Colosseo'),
(17, 'Red Star Laos', NULL, 7, NULL, 'LAO', '#CC0000', '#FFD633', '#002868', NULL, 'Vientiane, Laos', 'Vientiane Complex'),
(18, 'Reykjavik United', 6, 7, 1, 'RKV', '#000000', '#53E048', '#2D7F24', NULL, 'Reykjavík, Iceland', 'Leikvöllurinn'),
(19, 'Schwarzwälder FV', 7, 10, 1, 'SFV', '#1F1F1F', '#FFFAFA', '#05472A', '#F2003C', 'Pforzheim, Germany', 'Drei-Täler-Kampfbahn'),
(20, 'North Shore United', 6, 12, 2, 'NSU', '#000000', '#FFFFFF', '#CBD5FF', '#FFE17D', 'Auckland, New Zealand', 'The Beach');

CREATE TABLE IF NOT EXISTS managers (
	orgID INTEGER,
    orgManager INTEGER,
    assManager1 INTEGER,
    assManager2 INTEGER,
    FOREIGN KEY (orgID) REFERENCES organizations(id)
);

INSERT INTO managers (orgID, orgManager, assManager1, assManager2) VALUES
(0, 6, 497, NULL),
(1, 94, 33, 465),
(2, 238, 433, NULL),
(3, 159, 77, NULL),
(4, 272, NULL, NULL),
(5, 548, NULL, NULL),
(6, 67, 325, 41),
(7, 313, 318, NULL);

CREATE TABLE `attributes` (
  `attribute` text,
  `group` text,
  `keeper` text,
  `abbr` text,
  `explanation` text
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


CREATE TABLE `regression` (
  `pid` double NOT NULL,
  `tpe` double DEFAULT NULL,
  PRIMARY KEY (`pid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


CREATE TABLE `tpehistory` (
  `time` double NOT NULL,
  `uid` double DEFAULT NULL,
  `pid` double NOT NULL,
  `source` varchar(32) NOT NULL,
  `tpe` double DEFAULT NULL,
  PRIMARY KEY (`time`,`pid`,`source`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `tpetable` (
  `value` bigint NOT NULL,
  `cumCost` double DEFAULT NULL,
  `sinCost` double DEFAULT NULL,
  PRIMARY KEY (`value`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `updatehistory` (
  `time` double NOT NULL,
  `uid` double DEFAULT NULL,
  `pid` double NOT NULL,
  `attribute` varchar(32) NOT NULL,
  `old` double DEFAULT NULL,
  `new` double DEFAULT NULL,
  PRIMARY KEY (`time`,`pid`,`attribute`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE portaldb.`playerdata` (
  `uid` bigint DEFAULT NULL,
  `pid` int NOT NULL AUTO_INCREMENT,
  `status_p` int DEFAULT 0,
  `first` text,
  `last` text,
  `name` text,
  `class` text,
  `created` double DEFAULT NULL,
  `tpe` bigint DEFAULT NULL,
  `tpeused` double DEFAULT NULL,
  `tpebank` double DEFAULT NULL,
  `team` text,
  `affiliate` text,
  `birthplace` text,
  `nationality` text,
  `height` bigint DEFAULT NULL,
  `weight` bigint DEFAULT NULL,
  `hair_color` text,
  `hair_length` text,
  `skintone` bigint DEFAULT NULL,
  `render` text,
  `footedness` text,
  `position` text,
  `pos_st` bigint DEFAULT NULL,
  `pos_lam` bigint DEFAULT NULL,
  `pos_cam` bigint DEFAULT NULL,
  `pos_ram` bigint DEFAULT NULL,
  `pos_lm` bigint DEFAULT NULL,
  `pos_cm` bigint DEFAULT NULL,
  `pos_rm` bigint DEFAULT NULL,
  `pos_lwb` bigint DEFAULT NULL,
  `pos_cdm` bigint DEFAULT NULL,
  `pos_rwb` bigint DEFAULT NULL,
  `pos_ld` bigint DEFAULT NULL,
  `pos_cd` bigint DEFAULT NULL,
  `pos_rd` bigint DEFAULT NULL,
  `pos_gk` bigint DEFAULT NULL,
  `acceleration` bigint DEFAULT NULL,
  `agility` bigint DEFAULT NULL,
  `balance` bigint DEFAULT NULL,
  `jumping reach` bigint DEFAULT NULL,
  `natural fitness` bigint DEFAULT NULL,
  `pace` bigint DEFAULT NULL,
  `stamina` bigint DEFAULT NULL,
  `strength` bigint DEFAULT NULL,
  `corners` bigint DEFAULT NULL,
  `crossing` bigint DEFAULT NULL,
  `dribbling` bigint DEFAULT NULL,
  `finishing` bigint DEFAULT NULL,
  `first touch` bigint DEFAULT NULL,
  `free kick` bigint DEFAULT NULL,
  `heading` bigint DEFAULT NULL,
  `long shots` bigint DEFAULT NULL,
  `long throws` bigint DEFAULT NULL,
  `marking` bigint DEFAULT NULL,
  `passing` bigint DEFAULT NULL,
  `penalty taking` bigint DEFAULT NULL,
  `tackling` bigint DEFAULT NULL,
  `technique` bigint DEFAULT NULL,
  `aggression` bigint DEFAULT NULL,
  `anticipation` bigint DEFAULT NULL,
  `bravery` bigint DEFAULT NULL,
  `composure` bigint DEFAULT NULL,
  `concentration` bigint DEFAULT NULL,
  `decisions` bigint DEFAULT NULL,
  `determination` bigint DEFAULT NULL,
  `flair` bigint DEFAULT NULL,
  `leadership` bigint DEFAULT NULL,
  `off the ball` bigint DEFAULT NULL,
  `positioning` bigint DEFAULT NULL,
  `teamwork` bigint DEFAULT NULL,
  `vision` bigint DEFAULT NULL,
  `work rate` bigint DEFAULT NULL,
  `aerial reach` bigint DEFAULT NULL,
  `command of area` bigint DEFAULT NULL,
  `communication` bigint DEFAULT NULL,
  `eccentricity` bigint DEFAULT NULL,
  `handling` bigint DEFAULT NULL,
  `kicking` bigint DEFAULT NULL,
  `one on ones` bigint DEFAULT NULL,
  `reflexes` bigint DEFAULT NULL,
  `tendency to rush` bigint DEFAULT NULL,
  `tendency to punch` bigint DEFAULT NULL,
  `throwing` bigint DEFAULT NULL,
  `traits` text,
  `redistused` int default 0,
  `rerollused` int default 0,
  PRIMARY KEY (`pid`)
) ENGINE=InnoDB AUTO_INCREMENT=5114 DEFAULT CHARSET=utf8mb4;

UPDATE playerdata
SET 
  acceleration = NULLIF(acceleration, 0),
  agility = NULLIF(agility, 0),
  balance = NULLIF(balance, 0),
  `jumping reach` = NULLIF(`jumping reach`, 0),
  `natural fitness` = NULLIF(`natural fitness`, 0),
  pace = NULLIF(pace, 0),
  stamina = NULLIF(stamina, 0),
  strength = NULLIF(strength, 0),
  corners = NULLIF(corners, 0),
  crossing = NULLIF(crossing, 0),
  dribbling = NULLIF(dribbling, 0),
  finishing = NULLIF(finishing, 0),
  `first touch` = NULLIF(`first touch`, 0),
  `free kick` = NULLIF(`free kick`, 0),
  heading = NULLIF(heading, 0),
  `long shots` = NULLIF(`long shots`, 0),
  `long throws` = NULLIF(`long throws`, 0),
  marking = NULLIF(marking, 0),
  passing = NULLIF(passing, 0),
  `penalty taking` = NULLIF(`penalty taking`, 0),
  tackling = NULLIF(tackling, 0),
  technique = NULLIF(technique, 0),
  aggression = NULLIF(aggression, 0),
  anticipation = NULLIF(anticipation, 0),
  bravery = NULLIF(bravery, 0),
  composure = NULLIF(composure, 0),
  concentration = NULLIF(concentration, 0),
  decisions = NULLIF(decisions, 0),
  determination = NULLIF(determination, 0),
  flair = NULLIF(flair, 0),
  leadership = NULLIF(leadership, 0),
  `off the ball` = NULLIF(`off the ball`, 0),
  positioning = NULLIF(positioning, 0),
  teamwork = NULLIF(teamwork, 0),
  vision = NULLIF(vision, 0),
  `work rate` = NULLIF(`work rate`, 0),
  `aerial reach` = NULLIF(`aerial reach`, 0),
  `command of area` = NULLIF(`command of area`, 0),
  communication = NULLIF(communication, 0),
  eccentricity = NULLIF(eccentricity, 0),
  handling = NULLIF(handling, 0),
  kicking = NULLIF(kicking, 0),
  `one on ones` = NULLIF(`one on ones`, 0),
  reflexes = NULLIF(reflexes, 0),
  `tendency to rush` = NULLIF(`tendency to rush`, 0),
  `tendency to punch` = NULLIF(`tendency to punch`, 0),
  throwing = NULLIF(throwing, 0);

ALTER TABLE `portaldb`.`updatehistory` 
CHANGE COLUMN `old` `old` VARCHAR(256) NULL DEFAULT NULL ,
CHANGE COLUMN `new` `new` VARCHAR(256) NULL DEFAULT NULL ;