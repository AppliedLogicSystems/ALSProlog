BUILD=../core/unix/linux
SHIP=als-student-prolog

all: student

student:
	rm -fr $(SHIP)
	mkdir $(SHIP)
	cp -pr $(BUILD)/alspro $(SHIP)
	cp -pr $(BUILD)/alsdev $(SHIP)
	cp -pr $(BUILD)/lib $(SHIP)
	mkdir $(SHIP)/alsdir
	cp -pr $(BUILD)/alsdir/library $(SHIP)/alsdir
	cp -pr $(BUILD)/alsdir/images $(SHIP)/alsdir
	cp -pr $(BUILD)/alsdir/shared $(SHIP)/alsdir
	cp -pr ../manual/manual.pdf $(SHIP)
	cp -pr ../examples $(SHIP)
	tar czf $(SHIP)-linux.tgz $(SHIP)

standard:
