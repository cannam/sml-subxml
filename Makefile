
subxmlparse:	subxmlparse.mlb subxml.sml subxmlparse.sml
	mlton $<
	./test.sh > test.log 2>&1
	tail -3 test.log

clean:
	rm -f subxmlparse test.log

