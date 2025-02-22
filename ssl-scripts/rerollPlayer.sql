SET @uid = 1; -- Variable for user ID
SET @pid = 5563; -- Variable for player ID

-- New values
SET @new_first = 'Kim'; 
SET @new_last = 'Minjeong'; 
SET @new_name = 'Kim Minjeong'; 
SET @new_skin = '1'; 
SET @new_height = 76; 
SET @new_weight = 220; 

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
    weight = @new_weight
WHERE pid = @pid;