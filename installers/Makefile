BUILD=../core/linux
SHIP=als-student-prolog


student:
	rm -fR $(SHIP)
	mkdir $(SHIP)
	cp -a $(BUILD)/alspro $(SHIP)
	cp -a $(BUILD)/alsdev $(SHIP)
	cp -a $(BUILD)/lib $(SHIP)
	mkdir $(SHIP)/alsdir
	cp -pR $(BUILD)/alsdir/library $(SHIP)/alsdir
	cp -pR $(BUILD)/alsdir/images $(SHIP)/alsdir
	cp -pR $(BUILD)/alsdir/shared $(SHIP)/alsdir
	cp -a ../manual/manual.pdf $(SHIP)
	cp -a ../examples $(SHIP)
	tar czf $(SHIP)-linux.tgz $(SHIP)

standard:
