ARCH = ${shell 	case `uname -rs` in \
	"SunOS 4"*) 	echo sunos	;; \
	"SunOS 5"*) 	echo solaris 	;; \
	Linux*) 	echo linux 	;; \
	"HP-UX"*)	echo hpux	;; \
	*) 		echo "Unknown machine type..."; exit 1 ;; \
	esac}

BUILD=../../core/unix/$(ARCH)
SHIP=$(ARCH)/als-student-prolog


all: student

student:
	rm -fr $(SHIP)
	mkdir -p $(SHIP)
	cp -pr $(BUILD)/alspro $(SHIP)
	cp -pr $(BUILD)/alsdev $(SHIP)
	cp -pr $(BUILD)/lib $(SHIP)
	mkdir $(SHIP)/alsdir
	cp -pr $(BUILD)/alsdir/library $(SHIP)/alsdir
	cp -pr $(BUILD)/alsdir/images $(SHIP)/alsdir
	cp -pr $(BUILD)/alsdir/shared $(SHIP)/alsdir
	cp -pr ../../manual/*.pdf $(SHIP)
	cp -pr ../../examples $(SHIP)
	tar -C $(ARCH)/ -czf als-student-prolog-$(ARCH).tgz  als-student-prolog

standard:
