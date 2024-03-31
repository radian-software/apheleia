BEGIN {
	print "Users and their corresponding home"
	print " UserName \t HomePath"
	print "___________ \t __________"
	FS = ":"
}

{
	if ($2 == "foo") {
		print $2
	}
	print $1 "  \t  " $6
}

END {
	print "The end"
}

