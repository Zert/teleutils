ERLC		= erlc
RM			= rm -rf

SOURCES = $(wildcard src/*.erl)

BEAMDIR = ebin
SRCDIR  = src
DOCDIR	= doc

BEAMS = $(subst ${SRCDIR},${BEAMDIR},$(subst .erl,.beam,${SOURCES}))
HTMLS = $(subst ${SRCDIR},${DOCDIR},$(subst .erl,.html,${SOURCES}))

INCLUDES	=

ERLCFLAGS	+= +debug_info
ERLCFLAGS	+= -W3
ERLCFLAGS	+= ${INCLUDES}
ERLCFLAGS	+= -o ${BEAMDIR}

all: compile-beam make-doc TAGS

compile-beam: ${BEAMS}

make-doc: ${HTMLS}

${BEAMDIR}/%.beam: ${SRCDIR}/%.erl
	${ERLC} ${ERLCFLAGS} $<

${DOCDIR}/%.html: ${SRCDIR}/%.erl
	erl -noshell -eval "edoc:file(\"$<\", [{dir, \"./$(DOCDIR)\"}])" -s init stop

clean:
	${RM} ${BEAMDIR}/*.beam erl_crash.dump ${SRCDIR}/*~ *~ TAGS doc/*

dialyzer:
	dialyzer -Wno_match -c ${BEAMDIR}/*.beam

dialyzer-src:
	dialyzer --src ${INCLUDES} -Wno_match -c ${SRCDIR}/*.erl

TAGS:
	etags ${SRCDIR}/*.erl
