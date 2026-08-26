#!/usr/bin/env bash
# Bootstrap everything this Emacs config expects on a fresh machine.
# Elisp packages are NOT handled here - straight.el installs those
# automatically the first time Emacs starts. This script covers the
# external binaries the config shells out to.
#
# Usage: ./install_packages.sh

set -u
cd "$(dirname "$0")"

echo "==> git submodules (chronoscope)"
git submodule update --init

echo "==> uv (installs python tools into isolated envs in ~/.local/bin)"
if ! command -v uv >/dev/null 2>&1; then
  curl -LsSf https://astral.sh/uv/install.sh | sh
  export PATH="$HOME/.local/bin:$PATH"
fi

echo "==> python tools"
# pylsp with all plugins: jedi completion/hover/goto, flake8 lint,
# autopep8, pylint, pydocstyle. This is what the IDE section runs.
uv tool install "python-lsp-server[all]"
uv tool install black      # formatter, used by apheleia format-on-save
uv tool install ipython    # nicer run-python shell when present

if [[ "$(uname)" == "Darwin" ]]; then
  echo "==> system deps (homebrew)"
  # cmake + GNU libtool (glibtool): vterm builds its C module with these
  # ripgrep: projectile search, dumb-jump, wgrep
  # clang-format: C/C++ formatting for apheleia
  brew install cmake libtool ripgrep clang-format
  # clangd comes with the Xcode command line tools
  if ! xcrun -f clangd >/dev/null 2>&1; then
    xcode-select --install
  fi
else
  echo "==> system deps (apt)"
  sudo apt-get install -y cmake libtool-bin build-essential ripgrep \
    clangd clang-format python3-venv
fi

echo
echo "Done. First Emacs launch will take a few minutes while straight.el"
echo "clones and builds every elisp package. After that, run once inside Emacs:"
echo "  M-x all-the-icons-install-fonts"
echo "  M-x nerd-icons-install-fonts"
echo "and restart. If vterm's module didn't build, M-x vterm-module-compile."
