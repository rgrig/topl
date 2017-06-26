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
	cp toplc.byte toplc

native:
	@$(OCAMLBUILD) $(NATIVES)
	cp toplc.native toplc

lib:
	@$(OCAMLBUILD) topl.cma topl.cmxa

install-lib: lib
	@cp src/META META
	@echo "version=\"$$(date +%Y%m)\"" >> META
	@ocamlfind remove topl
	@ocamlfind install topl META $(addprefix _build/topl.,cmi cma cmxa a)
	@rm -f META

test: default
	@cd tests; ./test -c

checker:
	@mkdir -p classes/checker
	@javac -sourcepath src src/topl/*.java -Xlint:all -d classes/checker

infer-compiler:
	@mkdir -p classes/infer-compiler
	@javac -sourcepath src src/infer/*.java -Xlint:all -d classes/infer-compiler
	@cd classes/infer-compiler; jar cfe ../toplc.jar infer.Compile $$(find . -type f)

clean:
	@$(OCAMLBUILD) -clean
	@rm -rf classes
	@rm -f toplc
	@rm -f META

.PHONY: byte checker clean default install-lib lib native test
