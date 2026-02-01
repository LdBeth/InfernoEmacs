tell application "Music"
	set {a, b, c} to get {name, artist, album} of current track
	do shell script "curl -u openmic:nRNSRvDPkIE -X POST -H 'Content-Type: text/plain; charset=utf-8' --data-urlencode '" & a & " - " & b & ", " & c & "' https://anonradio.net/playlist/index.cgi"
end tell
