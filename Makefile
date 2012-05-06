# Configuration
OCFLAGS=-w,Ae
MAINS=toplc

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
NATIVES=$(addsuffix .native,$(MAINS))
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags $(OCFLAGS)

default: native

byte:
	@$(OCAMLBUILD) $(BYTES)
	ln -sf toplc.byte toplc

native:
	@$(OCAMLBUILD) $(NATIVES)
	ln -sf toplc.native toplc

test: default
	@cd tests; ./test -c

checker:
	@mkdir -p classes
	@javac -sourcepath src src/topl/*.java -Xlint:all -d classes

clean:
	@$(OCAMLBUILD) -clean
	@rm -rf classes
	@rm -f toplc src/config.ml

.PHONY: byte checker clean default native test
