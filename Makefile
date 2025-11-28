bold=$(shell tput bold)
normal=$(shell tput sgr0)

FLAGS=-use-ocamlfind -Is heptagon/compiler/,lib/ \
      -pkgs str,unix,menhirLib,ocamlgraph,js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-tyxml,js_of_ocaml-lwt.graphics,ezjs_ace,chartjs,yojson \
	  -no-hygiene

SRC := \
	notebook.ml \
	notebooks.ml \
	hept_scoping2.ml \
	compil.ml \
	chronogram.ml \
	saveNotebook.ml \
	page.ml \
	js_obc_conversion.ml \
	simul.ml interp.ml \
	kind2Json.ml verify.ml autocorrect.ml user.ml \
	tryhept.ml \
	lib/pervasives.ml lib/mathlib.ml

all: tryhept.js login.js

# examples.ml:
# 	ocaml preproc_examples.ml

NOTEBOOKS = demo cours1 cours2 cours3 cours4 scratchpad fft
NOTEBOOK_DIRS = $(foreach dir,$(NOTEBOOKS),notebooks/$(dir))
NOTEBOOK_FILES = $(foreach dir, $(NOTEBOOK_DIRS), $(dir)/*)

mknotebooks.byte: notebook.ml mknotebooks.ml
	ocamlfind ocamlc -o $@ -package yojson yojson.cma $^

notebooks.ml: mknotebooks.byte $(NOTEBOOK_FILES)
	ocamlrun $< $(NOTEBOOK_DIRS)

tryhept.byte: $(SRC)
	@echo "${bold}Building tryhept...${normal}"
	ocamlbuild ${FLAGS} tryhept.byte
	@echo "${bold}Done.${normal}"

login.byte: login.ml user.ml
	ocamlbuild ${FLAGS} login.byte

%.js: %.byte
	js_of_ocaml $^

lib/%.epci: lib/%.epi
	cd lib && ../heptagon/heptc $*.epi

%.epci: heptagon/lib/%.epi
	cd heptagon/lib && make
	cp heptagon/lib/$@ .

lib/%.ml: lib/%.epci embed_epci.ml
	cd lib && ocaml ../embed_epci.ml $*.epci > $*.ml

clean:
	rm -rf _build/ *.byte tryhept.js pervasives.epci notebooks.ml lib/*.epci lib/*.ml

.PHONY:
	all clean extraction
