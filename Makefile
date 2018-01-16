
xmlparse:	xmlparse.mlb xml.sml xmlparse.sml
	../sml-buildscripts/polybuild $<   ###!!! switch to mlton
	./test.sh > test.log 2>&1
	tail -3 test.log

clean:
	rm -f xmlparse test.log

