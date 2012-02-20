# Configuration
OCFLAGS=-w,Ae
MAINS=instrumenter

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
NATIVES=$(addsuffix .native,$(MAINS))
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags $(OCFLAGS)

default: native

byte:
	@$(OCAMLBUILD) $(BYTES)

native:
	@$(OCAMLBUILD) $(NATIVES)

checker:
	@mkdir -p classes
	@javac -sourcepath src src/topl/*.java -Xlint:all -d classes

clean:
	@$(OCAMLBUILD) -clean
	@rm -f .out
	@rm -rf classes

.PHONY: byte checker clean default native
