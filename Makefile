SOURCES=multi_set.ml diophantienne.ml term.ml unify.ml test.ml
OBJECTS=$(SOURCES:.ml=.cmo)
INTERFACES=$(SOURCES:.ml=.mli)
FLAGS=-g
EXECUTABLE=exec


$(EXECUTABLE): compil
	ocamlopt -o $(EXECUTABLE) $(SOURCES)

compil:
	ocamlopt -c $(INTERFACES)		
	ocamlopt -c $(SOURCES)

clean:
	rm -f *~
	rm -f *.cmo
	rm -f *.cmx
	rm -f *.cmi
	rm -f *.o
	rm -f $(EXECUTABLE)


