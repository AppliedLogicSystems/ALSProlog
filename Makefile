all test clean distclean:
	$(MAKE) -C ./core/unix $@
	$(MAKE) -C ./installers/unix $@
