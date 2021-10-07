VERSION:=0.0.1
PACKAGE_ID:=lsp-netbeans
PACKAGE_NAME:=$(PACKAGE_ID)-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r ../$(PACKAGE_ID)/* $@
	sed -i "s/VERSION/$(VERSION)/" $@/$(PACKAGE_ID)-pkg.el
	sed -i "s/;; Version: VERSION/;; Version: $(VERSION)/" $@/$(PACKAGE_ID).el

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)

release:
	@echo Release $(VERSION)
	sed -i "s/VERSION/$(VERSION)/" $(PACKAGE_ID)-pkg.el
	sed -i "s/;; Version: VERSION/;; Version: $(VERSION)/" $(PACKAGE_ID).el
	git add $(PACKAGE_ID)-pkg.el
	git add $(PACKAGE_ID).el
	git commit -m "Release $(VERSION)"
	git tag v$(VERSION)
	sed -i "s/$(VERSION)/VERSION/" $(PACKAGE_ID)-pkg.el
	sed -i "s/;; Version: $(VERSION)/;; Version: VERSION/" $(PACKAGE_ID).el
	git add $(PACKAGE_ID)-pkg.el
	git add $(PACKAGE_ID).el
	git commit -m "Next dev cycle"
