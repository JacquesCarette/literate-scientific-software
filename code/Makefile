# For documentation on how to add to this, see Issue #1371

SHELL=/bin/bash

# List of all known packages
PACKAGES = lang utils build gool code theory database printers gen data docLang example

# and associated package-level suffixes
BUILD_P_SUFFIX = _build
DOC_P_SUFFIX = _doc
GRAPH_P_SUFFIX = _graph

BUILD_PACKAGES = $(addsuffix $(BUILD_P_SUFFIX), $(PACKAGES))
DOC_PACKAGES = $(addsuffix $(DOC_P_SUFFIX), $(PACKAGES))
GRAPH_PACKAGES = $(addsuffix $(GRAPH_P_SUFFIX), $(PACKAGES))
ANALYSIS_PACKAGES = drasil $(PACKAGES) 

PACKAGE_GEN_TARGET = BUILD DOC GRAPH

#####

# Current list of examples
SRC_EXAMPLES = glassbr nopcm projectile
EXAMPLES = $(SRC_EXAMPLES) hghc swhs ssp gamephysics template dblpendulum pdController
GOOLTEST = gooltest

# where they live
HGHC_DIR  = HGHC
GLASSBR_DIR = GlassBR
NOPCM_DIR = NoPCM
SWHS_DIR  = SWHS
SSP_DIR   = SSP
GAMEPHYSICS_DIR  = GamePhysics
PROJECTILE_DIR = Projectile
TEMPLATE_DIR = Template
GOOLTEST_DIR = GOOLTest
DBLPENDULUM_DIR = DblPendulum
PDCONTROLLER_DIR = PDController

MULTI_SRC_DIRS = $(PROJECTILE_DIR) # Directories for examples with multiple src versions

EXAMPLE_DIRS = $(MULTI_SRC_DIRS) $(HGHC_DIR) $(GLASSBR_DIR) $(NOPCM_DIR) \
  $(SWHS_DIR) $(SSP_DIR) $(GAMEPHYSICS_DIR) $(DBLPENDULUM_DIR) \
  $(PDCONTROLLER_DIR) # All example directories

# and the name of their executables
HGHC_EXE  = hghc
GLASSBR_EXE = glassbr
NOPCM_EXE = nopcm
SWHS_EXE  = swhs
SSP_EXE   = ssp
GAMEPHYSICS_EXE  = gamephysics
PROJECTILE_EXE = projectile
TEMPLATE_EXE = template
GOOLTEST_EXE = codegenTest
DBLPENDULUM_EXE = dblpendulum
PDCONTROLLER_EXE = pdController

# and the associated per-example information generated
GEN_E_SUFFIX = _gen
TEST_E_SUFFIX = _diff
TEX_E_SUFFIX = _tex
CODE_E_SUFFIX = _code
DCP_E_SUFFIX = _deploy_code_path
STABILIZE_E_SUFFIX = _stabilize
BC_E_SUFFIX = _build_clean

GEN_EXAMPLES = $(addsuffix $(GEN_E_SUFFIX), $(EXAMPLES) $(GOOLTEST))
TEST_EXAMPLES =  $(addsuffix $(TEST_E_SUFFIX), $(EXAMPLES) $(GOOLTEST))
TEX_EXAMPLES = $(addsuffix $(TEX_E_SUFFIX), $(EXAMPLES))
CODE_EXAMPLES = $(addsuffix $(CODE_E_SUFFIX), $(SRC_EXAMPLES))
DCP_EXAMPLES = $(addsuffix $(DCP_E_SUFFIX), $(EXAMPLES))
STABILIZE_EXAMPLES = $(addsuffix $(STABILIZE_E_SUFFIX), $(EXAMPLES))
BC_EXAMPLES = $(addsuffix $(BC_E_SUFFIX), $(EXAMPLES))

EXAMPLE_GEN_TARGET = GEN TEST TEX CODE DCP

GENNED_FOLDERS = $(ANALYSIS_FOLDER_NAME) $(BUILD_FOLDER_NAME) $(DEPLOY_FOLDER_NAME) $(DOCS_FOLDER_NAME) $(GRAPH_FOLDER_NAME) $(LOG_FOLDER_NAME)
CLEAN_GF_PREFIX = clean_
CLEAN_FOLDERS = $(addprefix $(CLEAN_GF_PREFIX), $(GENNED_FOLDERS))

ALL_EXPANDED_TARGETS = $(foreach P, $(PACKAGE_GEN_TARGET), $($(P)_PACKAGES)) $(foreach E, $(EXAMPLE_GEN_TARGET), $($(E)_EXAMPLES)) $(CLEAN_FOLDERS)

#####

# make variables/configuration
DIFF = diff --strip-trailing-cr -r -X ../.gitignore -x '*.txt'
LOG_SUFFIX = _log.log
MIN_STACK_VER = 2.3.1  # Match stack.yaml see PR #2142 for more info.
CACHED_MSV_FILE = .drasil-min-stack-ver
DF_DIR = datafiles/
BUILD_FOLDER_NAME = build
BUILD_FOLDER = $(BUILD_FOLDER_NAME)/
STABLE_FOLDER_NAME = stable
STABLE_FOLDER = $(STABLE_FOLDER_NAME)/
EXAMPLE_CODE_SUBFOLDER = src/
LOG_FOLDER_NAME = logs
LOG_FOLDER = $(LOG_FOLDER_NAME)/
SCRIPT_FOLDER = scripts/
GRAPH_FOLDER_NAME = graphs
GRAPH_FOLDER = $(GRAPH_FOLDER_NAME)/
ANALYSIS_FOLDER_NAME = analysis
ANALYSIS_FOLDER = $(ANALYSIS_FOLDER_NAME)/
TYPEGRAPH_FOLDER_NAME = TypeDependencyGraphs
TYPEGRAPH_FOLDER = $(ANALYSIS_FOLDER)$(TYPEGRAPH_FOLDER_NAME)/
DEPLOY_FOLDER_NAME = deploy
DEPLOY_FOLDER = $(DEPLOY_FOLDER_NAME)/
DEPLOY_CODE_PATH_KV_SEP = :
DOCS_FOLDER_NAME = docs
DOCS_FOLDER = $(DOCS_FOLDER_NAME)/
TRACEY_GRAPH_FOLDER_NAME = TraceyGraph
TRACEY_GRAPH_FOLDER = $(TRACEY_GRAPH_FOLDER_NAME)/
TRACEY_GRAPHS_FOLDER_NAME = traceygraphs
TRACEY_GRAPHS_FOLDER = $(TRACEY_GRAPHS_FOLDER_NAME)/
TRACEY_GRAPH_NAMES = allvsall allvsr avsa avsall refvsref
# make command line options
#  GHC debug options
PROFALL = --executable-profiling --library-profiling
PROFEXEC = +RTS -xc -P

#  GHC build options
ifndef GHCTHREADS
override GHCTHREADS = 2
endif

override GHCFLAGS += -Wall -j$(GHCTHREADS)
override stackArgs += --ghc-options="$(GHCFLAGS)"

#  Output amount control
NOISY=no
SUMMARIZE_TEX=no

# actual targets
all: test

debug: stackArgs+=$(PROFALL) 
debug: EXECARGS+=$(PROFEXEC) 
debug: test

# sanity check -- are we using the right stack version?
check_stack:
	 @MIN_STACK_VER="$(MIN_STACK_VER)" CACHED_MSV_FILE="$(CACHED_MSV_FILE)" "$(SHELL)" "$(SCRIPT_FOLDER)"check_stack.sh

check_dot:
	@BIN=dot DOWNLOAD_LOCATION=https://www.graphviz.org/download/ "$(SHELL)" "$(SCRIPT_FOLDER)"check_binary.sh

packages: $(BUILD_PACKAGES)

# Actually build all the packages
$(filter %$(BUILD_P_SUFFIX), $(BUILD_PACKAGES)): %$(BUILD_P_SUFFIX): check_stack
	stack install $(stackArgs) "drasil-$*"

%$(GEN_E_SUFFIX) %$(TEST_E_SUFFIX) %$(STABILIZE_E_SUFFIX) %$(BC_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(GEN_E_SUFFIX) %$(TEST_E_SUFFIX) %$(STABILIZE_E_SUFFIX) %$(BC_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)

# individual example runs
%$(GEN_E_SUFFIX): EEXE=$($(EXAMPLE)_EXE)
$(filter %$(GEN_E_SUFFIX), $(GEN_EXAMPLES)): %$(GEN_E_SUFFIX): example$(BUILD_P_SUFFIX)
	@mkdir -p "$(BUILD_FOLDER)$(EDIR)"
	cd "$(BUILD_FOLDER)$(EDIR)" && stack exec -- "$(EEXE)" $(EXECARGS)

# individual example build cleans
$(filter %$(BC_E_SUFFIX), $(BC_EXAMPLES)): %$(BC_E_SUFFIX):
	- rm -rf "$(BUILD_FOLDER)$(EDIR)"

# individual example stabilization
$(filter %$(STABILIZE_E_SUFFIX), $(STABILIZE_EXAMPLES)): %$(STABILIZE_E_SUFFIX): %$(BC_E_SUFFIX) %$(GEN_E_SUFFIX)
	- rm -rf "$(STABLE_FOLDER)$*/"
	- cp -r "$(BUILD_FOLDER)$(EDIR)" "$(STABLE_FOLDER)$*/"

# individual example diff tests
$(filter %$(TEST_E_SUFFIX), $(TEST_EXAMPLES)): %$(TEST_E_SUFFIX): $(CLEAN_GF_PREFIX)$(LOG_FOLDER_NAME) %$(GEN_E_SUFFIX)
	@mkdir -p "$(LOG_FOLDER)"
	- $(DIFF) "$(STABLE_FOLDER)$*/" "$(BUILD_FOLDER)$(EDIR)/" > "$(LOG_FOLDER)$(EDIR)$(LOG_SUFFIX)" 2>&1

# actually run the tests
test: $(TEST_EXAMPLES)
	@echo ----------------------------
	@echo Make complete, checking logs
	@echo ----------------------------
	@LOG_FOLDER="$(LOG_FOLDER)" LOG_SUFFIX="$(LOG_SUFFIX)" NOISY=$(NOISY) "$(SHELL)" "$(SCRIPT_FOLDER)"log_check.sh

prog: $(GEN_EXAMPLES)

stabilize: $(STABILIZE_EXAMPLES)
	@echo ----------------------------
	@echo Stabilized all examples
	@echo ----------------------------

# generate pretty dot graphs too
graphmod: check_stack
	stack install dotgen graphmod

$(filter %$(GRAPH_P_SUFFIX), $(GRAPH_PACKAGES)): %$(GRAPH_P_SUFFIX): graphmod check_dot
	@mkdir -p "$(GRAPH_FOLDER)"
	find "drasil-$*" -name '*.hs' -print | grep -v stack | xargs stack exec -- graphmod -q -p --no-cluster | dot -Tpdf > "$(GRAPH_FOLDER)drasil-$*".pdf

graphs: $(GRAPH_PACKAGES)
###

traceygraph: graphmod $(TEST_EXAMPLES)
	@echo Creating traceability graphs
	@rm -rf "$(TRACEY_GRAPHS_FOLDER)"
	@mkdir -p "$(TRACEY_GRAPHS_FOLDER)"
	@for dir in $(EXAMPLE_DIRS); do \
		if [ -d "$(BUILD_FOLDER)$$dir/$(TRACEY_GRAPH_FOLDER)" ]; then \
			mkdir -p "$(TRACEY_GRAPHS_FOLDER)$$dir" ; \
			cd "$(BUILD_FOLDER)$$dir/$(TRACEY_GRAPH_FOLDER)" && for graph in $(TRACEY_GRAPH_NAMES); do \
				dot -Tpdf $$graph.dot > "../../../$(TRACEY_GRAPHS_FOLDER)$$dir/$$graph.pdf" ; \
			done ; \
			cd "../../.." ; \
		fi ; \
	done
	@echo Done

analysis: graphmod
	- rm -rf "$(ANALYSIS_FOLDER)"
	@mkdir -p "$(ANALYSIS_FOLDER)"
	cd $(SCRIPT_FOLDER) && stack exec -- runghc DataTableGen.hs
	cd $(SCRIPT_FOLDER) && stack exec -- runghc TypeGraphGen.hs
	@echo Making dependency graphs
	@cd $(TYPEGRAPH_FOLDER) && for pack in $(ANALYSIS_PACKAGES); do \
		dot -Tpdf $$pack.dot > "./$$pack.pdf" ; \
	done
	@echo Done dependency graphs

# Use a 'local' HLint installation, using HLint from the current stack repo
hlint: check_stack
	stack install hlint
	hlint .

# Use the latest HLint version, downloading the binary each time
hot_hlint:
	curl --max-time 60 -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .

docs: check_stack
	DOCS_FOLDER="$(DOCS_FOLDER)" GHC_FLAGS="$(GHCFLAGS)" sh scripts/make_docs.sh

%$(TEX_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(TEX_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(TEX_E_SUFFIX), $(TEX_EXAMPLES)): %$(TEX_E_SUFFIX): %$(GEN_E_SUFFIX)
	EDIR="$(EDIR)" BUILD_FOLDER="$(BUILD_FOLDER)" SUMMARIZE_TEX=$(SUMMARIZE_TEX) MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"tex_build.sh

tex: $(TEX_EXAMPLES)

%$(CODE_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(CODE_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(CODE_E_SUFFIX), $(CODE_EXAMPLES)): %$(CODE_E_SUFFIX): %$(GEN_E_SUFFIX)
	@DF_DIR="$(DF_DIR)" EDIR="$(EDIR)" BUILD_FOLDER="$(BUILD_FOLDER)" \
	EXAMPLE_CODE_SUBFOLDER="$(EXAMPLE_CODE_SUBFOLDER)" \
	MULTI_SRC_DIRS="$(MULTI_SRC_DIRS)" TARGET=$(TARGET) \
	MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"code_build.sh

$(GOOLTEST): $(GOOLTEST)$(GEN_E_SUFFIX)
	@EDIR="$(GOOLTEST_DIR)" BUILD_FOLDER="$(BUILD_FOLDER)" TARGET=$(TARGET) \
	MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"gooltest_build.sh

code: $(CODE_EXAMPLES) $(GOOLTEST)

doxygen: TARGET=doc
doxygen: $(CODE_EXAMPLES)

%$(DCP_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(DCP_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(DCP_E_SUFFIX), $(DCP_EXAMPLES)): %$(DCP_E_SUFFIX):
	@EDIR="$(EDIR)" BUILD_FOLDER="$(BUILD_FOLDER)" EXAMPLE_CODE_SUBFOLDER="$(EXAMPLE_CODE_SUBFOLDER)" EXAMPLE=$* \
	MULTI_SRC_DIRS="$(MULTI_SRC_DIRS)" DEPLOY_CODE_PATH_KV_SEP="$(DEPLOY_CODE_PATH_KV_SEP)" \
	"$(SHELL)" "$(SCRIPT_FOLDER)"code_path.sh

deploy_code_path: $(DCP_EXAMPLES)

deploy_lite:
	@mkdir -p "$(DEPLOY_FOLDER)"
	@BUILD_FOLDER="$(BUILD_FOLDER)" DEPLOY_FOLDER="$(DEPLOY_FOLDER)" GRAPH_FOLDER="$(GRAPH_FOLDER)" \
	EXAMPLE_DIRS="$(EXAMPLE_DIRS)" MULTI_SRC_DIRS="$(MULTI_SRC_DIRS)" ANALYSIS_FOLDER="$(ANALYSIS_FOLDER)" \
	DEPLOY_CODE_PATH_KV_SEP="$(DEPLOY_CODE_PATH_KV_SEP)" MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"deploy_stage.sh

website:
	stack install drasil-website
	cd "drasil-website" && \
	CUR_DIR="$(PWD)/" \
	DEPLOY_FOLDER="$(CUR_DIR)$(DEPLOY_FOLDER)"  \
	DOCS_FOLDER="$(DOCS_FOLDER)" \
	DOX_FOLDER="doxygen/" \
	EXAMPLES_FOLDER="examples/" \
	SRS_FOLDER_FRAG="srs/" \
	GRAPH_FOLDER="$(GRAPH_FOLDER)" \
	ANALYSIS_FOLDER="$(ANALYSIS_FOLDER)" \
	stack exec website
# This rule is for use with developing deployment layout locally. As part of it, it ensures all needed
# dependencies exist. One of the downsides is we ensure all files are re-generated which means the TeX
# files appear newer and thus PDF's are regenerated. If you want to "just generate the structure,
# everything exists," (or you've run `deploy` once already) then `deploy_lite` does just that and is
# what `deploy.bash` calls. 
deploy: analysis graphs docs tex doxygen
	"$(MAKE)" deploy_lite


# follow two are for cleaning things out
$(filter $(CLEAN_GF_PREFIX)%, $(CLEAN_FOLDERS)): $(CLEAN_GF_PREFIX)%:
	- rm -rf "./$*/"

clean_artifacts cleanArtifacts: $(CLEAN_FOLDERS)
	- rm -f "$(CACHED_MSV_FILE)"

clean: clean_artifacts
	- stack clean

.PHONY: clean clean_artifacts cleanArtifacts code hlint hot_hlint analysis tex doc debug prog test graphs graphmod check_stack graphs website deploy_code_path deploy deploy_lite traceygraph all $(GOOLTEST) $(ALL_EXPANDED_TARGETS)
