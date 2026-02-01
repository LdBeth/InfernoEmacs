if application "Music" is running then
	tell application "Music"
		if player state is stopped then
			set display to "No Track Playing"
		else
			set track_artist to artist of current track
			set track_name to name of current track
			set display to track_artist & " - " & track_name
		end if
	end tell
else
	set display to "Music.app is not running"
end if
