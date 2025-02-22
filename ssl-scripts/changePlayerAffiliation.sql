





-- Log the update for Patrick Carrigan
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Cairo City' 
WHERE pd.name = 'Patrick Carrigan';

-- Update the playerdata for Patrick Carrigan
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Cairo City' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Patrick Carrigan';


-- Log the update for Princess Changshan
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Cairo City' 
WHERE pd.name = 'Princess Changshan';

-- Update the playerdata for Princess Changshan
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Cairo City' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Princess Changshan';


-- Log the update for Rippley Dieter
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Tokyo S.C.' 
WHERE pd.name = 'Rippley Dieter';

-- Update the playerdata for Rippley Dieter
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Tokyo S.C.' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Rippley Dieter';











-- Log the update for Angry Lizard
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'AS Paris' 
WHERE pd.name = 'Angry Lizard';

-- Update the playerdata for Angry Lizard
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'AS Paris' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Angry Lizard';





















-- Log the update for Zach Mulder
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'União São Paulo' 
WHERE pd.name = 'Zach Mulder';

-- Update the playerdata for Zach Mulder
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'União São Paulo' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Zach Mulder';






-- Log the update for Glenn Smart
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Inter London' 
WHERE pd.name = 'Glenn Smart';

-- Update the playerdata for Glenn Smart
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Inter London' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Glenn Smart';






-- Log the update for Andres Pedrillo
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'A.C. Romana' 
WHERE pd.name = 'Andres Pedrillo';

-- Update the playerdata for Andres Pedrillo
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'A.C. Romana' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Andres Pedrillo';






-- Log the update for Manuel Neuer
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'A.C. Romana' 
WHERE pd.name = 'Manuel Neuer';

-- Update the playerdata for Manuel Neuer
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'A.C. Romana' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Manuel Neuer';















-- Log the update for Henry Andrews
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'North Shore United' 
WHERE pd.name = 'Henry Andrews';

-- Update the playerdata for Henry Andrews
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'North Shore United' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Henry Andrews';
-- Log the update for Hanibal Armaros
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
WHERE pd.name = 'Hanibal Armaros';

-- Update the playerdata for Hanibal Armaros
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Hanibal Armaros';
-- Log the update for Santos Neymarinho
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
WHERE pd.name = 'Santos Neymarinho';

-- Update the playerdata for Santos Neymarinho
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Santos Neymarinho';
-- Log the update for Viktoria Snooks
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
WHERE pd.name = 'Viktoria Snooks';

-- Update the playerdata for Viktoria Snooks
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Viktoria Snooks';


-- Log the update for Charles-Andrew-Simon Utley-Abara-Lunga
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
WHERE pd.name = 'Charles-Andrew-Simon Utley-Abara-Lunga';

-- Update the playerdata for Charles-Andrew-Simon Utley-Abara-Lunga
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Reykjavik United' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Charles-Andrew-Simon Utley-Abara-Lunga';


















-- Log the update for Hercule Hefeweizen
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'F.C. Kaapstad' 
WHERE pd.name = 'Hercule Hefeweizen';

-- Update the playerdata for Hercule Hefeweizen
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'F.C. Kaapstad' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Hercule Hefeweizen';
-- Log the update for Marian Gorgoń
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Hollywood FC' 
WHERE pd.name = 'Marian Gorgoń';

-- Update the playerdata for Marian Gorgoń
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Hollywood FC' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Marian Gorgoń';






















-- Log the update for Hugh Mann
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Seoul MFC' 
WHERE pd.name = 'Hugh Mann';

-- Update the playerdata for Hugh Mann
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Seoul MFC' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Hugh Mann';


-- Log the update for Apollo Zervas
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'CF Catalunya' 
WHERE pd.name = 'Apollo Zervas';

-- Update the playerdata for Apollo Zervas
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'CF Catalunya' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Apollo Zervas';
-- Log the update for Ben Nelson
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'CF Catalunya' 
WHERE pd.name = 'Ben Nelson';

-- Update the playerdata for Ben Nelson
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'CF Catalunya' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Ben Nelson';
-- Log the update for Bernardo Fry
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'CF Catalunya' 
WHERE pd.name = 'Bernardo Fry';

-- Update the playerdata for Bernardo Fry
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'CF Catalunya' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Bernardo Fry';















































-- Log the update for Dombrofski Maximiliano
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Montréal United' 
WHERE pd.name = 'Dombrofski Maximiliano';

-- Update the playerdata for Dombrofski Maximiliano
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Montréal United' 
SET pd.team = org.orgID, pd.affiliate = 2 
WHERE pd.name = 'Dombrofski Maximiliano';
-- Log the update for Erik Beermann
INSERT INTO portaldb.updatehistory (time, uid, pid, attribute, old, new) 
SELECT 
    UNIX_TIMESTAMP(NOW()), 1, pd.pid, 'TEAM', pd.team, org.orgID 
FROM portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Schwarzwälder FV' 
WHERE pd.name = 'Erik Beermann';

-- Update the playerdata for Erik Beermann
UPDATE portaldb.playerdata pd 
JOIN portaldb.teams org ON org.name = 'Schwarzwälder FV' 
SET pd.team = org.orgID, pd.affiliate = 1 
WHERE pd.name = 'Erik Beermann';




















































