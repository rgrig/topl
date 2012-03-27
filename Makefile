# Configuration
OCFLAGS=-w,Ae
MAINS=toplc

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
NATIVES=$(addsuffix .native,$(MAINS))
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags $(OCFLAGS)

default: native

byte: src/config.ml
	@$(OCAMLBUILD) $(BYTES)
	ln -sf toplc.byte toplc

native: src/config.ml
	@$(OCAMLBUILD) $(NATIVES)
	ln -sf toplc.native toplc

test: default
	@cd tests; ./test -c

src/config.ml:
	echo "let src_dir = \"$(CURDIR)/src\"" > src/config.ml

checker:
	@mkdir -p classes
	@javac -sourcepath src src/topl/*.java -Xlint:all -d classes

clean:
	@$(OCAMLBUILD) -clean
	@rm -rf classes
	@rm -f toplc src/config.ml

.PHONY: byte checker clean default native test
