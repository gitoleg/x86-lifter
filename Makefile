all:
	bapbuild main.native

plugin:
	bapbuild -pp ppx-jane -ppflag -dump-ast newX86.plugin

install:
	bapbundle install newX86.plugin

uninstall:
	bapbundle remove newX86.plugin
