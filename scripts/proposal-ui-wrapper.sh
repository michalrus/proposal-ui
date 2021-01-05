#!/usr/bin/env bash

set -u

INSTRUCTIONS=(
"The next screen will allow the selection of proposal UI credentials.\n\n"
"Some keybindings are:\n\n"
"   <tab>    Move between panels\n"
"<arrows>    Move between panels and text\n"
" <space>    Update bottom text w/ selected\n"
" <enter>    Choose bottom text target\n"
"<typing>    Type bottom text target\n"
"   <esc>    Abort"
)

CONFIRM=(
"Are you sure you want select the following proposal UI credentials file?\n\n"
)

[ -d ./static/ ] && DIR="./static/" || DIR="./"
while true; do
  dialog --title 'Proposal UI Credentials Selection' --msgbox "${INSTRUCTIONS[*]}" 15 48 || { echo "Instructions cancelled -- aborting"; exit 1; }
  CREDS=$(dialog --stdout --title "Choose a Proposal UI Credentials File" --fselect "$DIR" 14 68) || { echo "File selection cancelled -- aborting"; exit 1; }
  dialog --stdout --title "Confirm Credentials Selection" --yesno "${CONFIRM[*]}${CREDS}" 8 68
  STATUS=$?
  [ "$STATUS" -eq 0 ] && break
  [ "$STATUS" -eq 255 ] && { echo "Confirmation cancelled -- aborting"; exit 1; }
done

set -o allexport
# shellcheck disable=SC1090
source "$CREDS" || { echo "Failed to source file \"${CREDS}\" -- aborting"; exit 1; }
set +o allexport

proposal-ui || { echo "Proposal UI exited with an error"; exit 1; } && { echo "Proposal UI exited cleanly"; exit 0; }
