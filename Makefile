SOURCES=multi_set.ml diophantienne.ml term.ml
OBJECTS=$(SOURCES:.ml=.cmo)
INTERFACES=$(SOURCES:.ml=.mli)
FLAGS=-g
EXECUTABLE=exec



compil:
	ocamlc -c $(INTERFACES)		
	ocamlc -c $(SOURCES)

clean:
	rm -f *~
	rm -f *cmi
	rm -f *cmo
	rm -f $(EXECUTABLE)


