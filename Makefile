bold=$(shell tput bold)
normal=$(shell tput sgr0)

SRC_DIR := heptc/src
EXTRACTED := heptc/extraction/extracted

FLAGS=-use-ocamlfind -Is heptagon/compiler/ \
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
	pervasives.ml mathlib.ml

all: tryhept.js login.js

# examples.ml:
# 	ocaml preproc_examples.ml

NOTEBOOKS = cours1 cours2 cours4
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

%.epci: heptagon/lib/%.epi
	cd heptagon/lib && make
	cp heptagon/lib/$@ .

%.ml: %.epci embed_epci.ml
	ocaml embed_epci.ml $^ > $@

clean:
	rm -rf _build/ *.byte tryhept.js pervasives.epci notebooks.ml

.PHONY:
	all clean extraction
