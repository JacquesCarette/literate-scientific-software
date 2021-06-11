LABEL_FILE=/tmp/ci_build_failures \
MANAGED_LABEL_FILE=/tmp/ci_managed_labels \
ALL_FUNCTIONS_FILE=$(mktemp) \
SHELL_OPTS_FILE=$(mktemp) \
GITHUB_REF="refs/heads/master" \
BOT_TOKEN=test \
GITHUB_RUN_ID=10001 \
GITHUB_RUN_NUMBER=10012 \
GITHUB_SHA="fake hash! :)" \
GITHUB_REPOSITORY=JacquesCarette/Drasil \
bash scripts/deploy_wrapper.bash
