#!/bin/sh
# Install/refresh the GNU Emacs daemon launchd agent.
#
# macOS launchd refuses symlinked or hard-linked plist files in
# ~/Library/LaunchAgents/, so this script copies the canonical
# version-tracked file into place and re-bootstraps the agent.

set -eu

REPO_PLIST="$(cd "$(dirname "$0")" && pwd)/gnu.emacs.daemon.plist"
TARGET="$HOME/Library/LaunchAgents/gnu.emacs.daemon.plist"
LABEL="gnu.emacs.daemon"
DOMAIN="gui/$(id -u)"

# Unload first if already loaded.
if launchctl print "$DOMAIN/$LABEL" >/dev/null 2>&1; then
    launchctl bootout "$DOMAIN/$LABEL"
fi

mkdir -p "$(dirname "$TARGET")"
cp "$REPO_PLIST" "$TARGET"
launchctl bootstrap "$DOMAIN" "$TARGET"

echo "Installed: $TARGET"
launchctl list | grep "$LABEL"
