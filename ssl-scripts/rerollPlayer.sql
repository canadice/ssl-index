SET @uid = 1; -- Variable for user ID
SET @pid = 5520; -- Variable for player ID

-- New values
SET @new_first = 'Joga'; 
SET @new_last = 'Bonito'; 
SET @new_name = 'Joga Bonito'; 
SET @new_skin = '16'; 
SET @new_height = 71; 
SET @new_weight = 170; 

-- Get the current time in the USA/Pacific timezone as a UNIX timestamp
SET @current_time = UNIX_TIMESTAMP(NOW());

-- Log and update first
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time, @uid, @pid, 'FIRST', pd.first, @new_first
FROM playerdata pd
WHERE pd.pid = @pid;

-- Log and update last
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time, @uid, @pid, 'LAST', pd.last, @new_last
FROM playerdata pd
WHERE pd.pid = @pid;

-- Log and update name
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time, @uid, @pid, 'NAME', pd.name, @new_name
FROM playerdata pd
WHERE pd.pid = @pid;

-- Log and update skin
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time, @uid, @pid, 'SKIN', pd.skintone, @new_skin
FROM playerdata pd
WHERE pd.pid = @pid;

-- Log and update height
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time, @uid, @pid, 'HEIGHT', pd.height, @new_height
FROM playerdata pd
WHERE pd.pid = @pid;

-- Log and update weight
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time, @uid, @pid, 'WEIGHT', pd.weight, @new_weight
FROM playerdata pd
WHERE pd.pid = @pid;

-- Update the playerdata table
UPDATE playerdata
SET 
    first = @new_first,
    last = @new_last,
    name = @new_name,
    skintone = @new_skin,
    height = @new_height,
    weight = @new_weight,
    rerollused = 1
WHERE pid = @pid;