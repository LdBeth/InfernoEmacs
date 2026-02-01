on run {theFile}
	tell application "Acrobat Distiller"
		Distill sourcePath theFile
	end tell
end run

