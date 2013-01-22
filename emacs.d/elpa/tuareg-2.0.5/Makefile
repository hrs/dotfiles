VERSION = $(shell grep ';; Version:' tuareg.el \
	| sed 's/;; Version: *\([0-9.]\+\).*/\1/')
DESCRIPTION = $(shell grep ';;; tuareg.el ---' tuareg.el \
	| sed 's/[^-]*--- *\(.*\)/\1/')
REQUIREMENTS = $(shell grep ';; Package-Requires:' tuareg.el \
	| sed 's/;; Package-Requires: *\(.\+\).*/\1/')
DIST_NAME = tuareg-$(VERSION)

ELS = tuareg.el ocamldebug.el
ELC = $(ELS:.el=.elc)

DIST_FILES += $(ELS) Makefile README

EMACS = emacs
NOINIT = -q --no-site-file

# when testing, we want all settings to be non-0
TEST_INIT = -eval '(setq tuareg-in-indent 2)'
BATCH = -batch $(NOINIT) --load tuareg.elc
RM = rm -rf
CP = cp -f
LN = ln
CMP = cmp
# CMP = diff -u
DIFF_W = diff -uw


INSTALL_RM_R = $(RM)
INSTALL_MKDIR = mkdir
INSTALL_CP = $(CP)

all : elc

elc : $(ELC)

%.elc : %.el
	$(EMACS) -batch $(NOINIT) -f batch-byte-compile $<

# ifneq ($(realpath .hg),)
# POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
# MAKE_VERSION_FILE = hg id -i | fgrep -v '+' >/dev/null || \
#         (echo 'uncommitted changes' >&2; exit 1); \
#         hg id -i --debug > $(VERSION_FILE)
# else
# ifneq ($(realpath .svn),)
# POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
# MAKE_VERSION_FILE = svn info | grep Revision: | sed 's/Revision: //' > $(VERSION_FILE)
# else
# ifneq ($(realpath .bzr),)
# POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
# MAKE_VERSION_FILE = bzr log -l -1 | grep revno: > $(VERSION_FILE)
# else
# ifneq ($(realpath $(VERSION_FILE)),)
# POST_INSTALL_HOOK =
# MAKE_VERSION_FILE = @echo "Using \"$(VERSION_FILE)\" in the distribution."
# else
# POST_INSTALL_HOOK =
# MAKE_VERSION_FILE = @(echo "missing \"$(VERSION_FILE)\" in the distribution?" >&2; exit 1)
# endif
# endif
# endif
# endif

# install : $(ELC) $(VERSION_FILE)
#         fgrep `cat $(VERSION_FILE)` tuareg.elc >/dev/null 2>&1 || \
#          ($(RM) tuareg.elc; $(MAKE) tuareg.elc)
#         $(INSTALL_RM_R) ${DEST}
#         $(INSTALL_MKDIR) ${DEST}
#         for f in $(ELS) $(ELC) $(VERSION_FILE); do $(INSTALL_CP) $$f $(DEST)/$$f; done
#         $(POST_INSTALL_HOOK)

# # have to indent twice because comments are indented to the _following_ code
# REINDENT = --file test.ml --eval '(with-current-buffer "test.ml" (tuareg-mode) (indent-region (point-min) (point-max)) (indent-region (point-min) (point-max)) (save-buffer))' --kill

# MANGLE = sed -e 's/^\(  *[a-z].*[^\"]\)$$/ \1/'

# EXTRA_CHECK_COMMANDS =

# check : $(ELC) sample.ml
#         @echo ====sample.ml====
#         $(MANGLE) sample.ml > test.ml
#         $(EMACS) $(BATCH) $(TEST_INIT) $(REINDENT)
#         $(CMP) sample.ml test.ml
#         $(EXTRA_CHECK_COMMANDS)
#         $(RM) test.ml test.ml~


.PHONY: dist tar
dist: $(DIST_NAME).tar.gz
tar: $(DIST_NAME).tar

$(DIST_NAME).tar.gz $(DIST_NAME).tar: $(DIST_FILES)
	mkdir -p $(DIST_NAME)
	for f in $(DIST_FILES); do $(LN) $$f $(DIST_NAME); done
	echo '(define-package "tuareg" "$(VERSION)" "$(DESCRIPTION)" ' "'"'$(REQUIREMENTS))' > $(DIST_NAME)/tuareg-pkg.el
	tar acvf $@ $(DIST_NAME)
	$(RM) -rf $(DIST_NAME)

clean :
	$(RM) $(ELC) "$(DIST_NAME).tar.gz" "$(DIST_NAME).tar"
#         $(POST_INSTALL_HOOK)

# .PHONY : all elc clean install force check distrib dist
