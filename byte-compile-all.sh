for E in ./*.el; do
	Eesc="$(echo "$E" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g')"
	echo -n "$E" ""
	emacsclient -e "(byte-compile-file \"$Eesc\")"
done
