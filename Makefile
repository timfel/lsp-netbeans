VERSION:=$(shell git log -1 --format=%at)
PACKAGE_ID:=lsp-netbeans
PACKAGE_NAME:=$(PACKAGE_ID)-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR): $(PACKAGE_ID)-pkg.el
	mkdir -p $@
	cp -r ../$(PACKAGE_ID)/* $@/

$(PACKAGE_ID)-pkg.el: *.el
	@echo "(define-package"   > $@
	@echo '  "lsp-netbeans"' >> $@
	@echo '  "$(VERSION)"'   >> $@
	@echo '  "A package to use the Netbeans based LSP and DAP server with emacs-lsp."' >> $@

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
