all:
	fsharpc -I FParsec.0.9.2.0/lib/net40 -r FParsec.dll -o scheve.exe --standalone parser.fs
