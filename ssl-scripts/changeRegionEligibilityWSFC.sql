SET @uid = 1; -- Variable for user ID
SET @pid = 5484; -- Variable for player ID
SET @new_value = 'Switzerland'; -- New value for nationality

-- Get the current time in the USA/Pacific timezone as a UNIX timestamp
SET @current_time = UNIX_TIMESTAMP(NOW());

-- Insert into the updatehistory table
INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
SELECT 
    @current_time AS time,
    @uid AS uid,
    @pid AS pid,
    'NATIONALITY' AS attribute,
    pd.nationality AS old,
    @new_value AS new
FROM 
    playerdata pd
WHERE 
    pd.pid = @pid;
    
-- Update the playerdata table's nationality to the new value
UPDATE playerdata
SET nationality = @new_value
WHERE pid = @pid;