all: util.import.scm json-query.import.scm jq

%.import.scm: %.scm
	csc -s -J $^

jq: jq.scm json-query.scm util.scm util.import.scm json-query.import.scm
	compile_jq.sh $^

deps:
	sudo chicken-install srfi-1 srfi-34 srfi-180 srfi-193 vector-lib
