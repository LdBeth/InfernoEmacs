if application "Music" is running then
	tell application "Music"
		if player state is stopped then
			set display to "0"
		else
			set now to player position
			set total to duration of current track
			set display to total - now
		end if
	end tell
else
	set display to "0"
end if
