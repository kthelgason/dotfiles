#!/usr/bin/env zsh
# Copyright (c) 2017  Kári Tristan Helgason <kthelgason@gmail.com>
# Copyright (c) 2016  Sebastian Wiesner <swiesner@lunaryorn.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

function sudo {
  echo "sudo $@?"
  command sudo "$@"
}

# Change my shell
if [[ $SHELL != */zsh ]]; then
  echo "Will ask for your password to turn change your login shell..."
  chsh -s /usr/local/bin/zsh;
fi

# Set locale and units
defaults write NSGlobalDomain AppleLocale -string 'en_IS'
defaults write NSGlobalDomain AppleMeasurementUnits -string 'Centimeters'
defaults write NSGlobalDomain AppleMetricUnits -bool true

# Expand dialogs by default
panels=(
  'NSNavPanelExpandedStateForSaveMode'
  'PMPrintingExpandedStateForPrint'
  'NSNavPanelExpandedStateForSaveMode2'
  'PMPrintingExpandedStateForPrint2'
)
for panel in panels; do defaults write NSGlobalDomain "$panel" -bool true; done

# Don't save to iCloud by default, we've got icloud documents anyway
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Force font smoothing everywhere
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# Disable quarantine for downloaded apps—I know what I'm doing
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Ask for a password immediately after the screensaver starts
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -float 0.0

# Dock to bottom, always visible, with indicators for running applications and
# no stupid bouncing
defaults write com.apple.dock 'orientation' -string 'bottom'
defaults write com.apple.dock 'autohide' -bool true
defaults write com.apple.dock 'show-process-indicators' -bool true
defaults write com.apple.dock 'no-bouncing' -bool true

# Don't sort spaces by usage please
defaults write com.apple.dock 'mru-spaces' -bool false

# Disable the dashboard
defaults write com.apple.dashboard dashboard-enabled-state -int 1

# Show battery percentage in menu bar
defaults write com.apple.menuextra.battery ShowPercent -string 'YES'

# Save screenshots as PNG on desktop, without the distracting shadow
defaults write com.apple.screencapture location -string "${HOME}/Desktop"
defaults write com.apple.screencapture type -string PNG
defaults write com.apple.screencapture disable-shadow -bool true

# In Finder search in the current folder by default and don't warn about
# changing extensions
defaults write com.apple.finder FXDefaultSearchScope -string SCcf
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Don't put shit on my desktop
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false
defaults write com.apple.finder ShowMountedServersOnDesktop -bool false
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false

# Enable developers tools in Safari and webkit
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari \
         WebKitDeveloperExtrasEnabledPreferenceKey -bool true

# Check for AppStore updates daily, and download them automatically.  Also
# install critical updates automatically.
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1
# Automatically download purchased apps
defaults write com.apple.SoftwareUpdate ConfigDataInstall -int 1

# Don't offer to use new volumes for backup
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

# Launch main window in activity monitor, show CPU usage in dock icon, show all
# processes by default and sort by CPU usage.
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true
defaults write com.apple.ActivityMonitor IconType -int 5
defaults write com.apple.ActivityMonitor ShowCategory -int 0
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0

# Show ~/Library in finder
chflags nohidden "$HOME/Library"

# Don't open Photos automatically when devices are plugged in
defaults write com.apple.ImageCapture disableHotPlug -bool true

# Disable smart quotes and smart dashes as they're annoying when typing code
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Enabling full keyboard access for all controls (e.g. enable Tab in modal dialogs)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Setting a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 0

# Fix super annoying emacs bug
defaults write org.gnu.Emacs NSAppSleepDisabled -bool YES

# Restart affected apps
apps=(
  'Activity Monitor'
  'Calendar'
  'Contacts'
  'Dock'
  'Finder'
  'Mail'
  'Messages'
  'Safari'
  'SystemUIServer'
  'csprefsd'
)
read "reply?Kill affected apps? [yes|no] "
if [[ $reply == "yes" ]]; then
  for app in $apps; do killall "$app" &> /dev/null; done
fi

# Install Homebrew.
if ! command -v brew > /dev/null 2>&1; then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
  echo "Brew already installed."
fi

# Let Homebrew track me, they appreciate it; see
# http://docs.brew.sh/Analytics.html
brew analytics on

# Install some basic Python tooling, anything specific goes into a virtualenv
packages=(
  # virtualenv management
  virtualenvwrapper
  # A better python shell
  ipython
  # And some basic linting
  flake8
  pep8-naming
  flake8-quotes
  flake8_docstrings
)
pip3 install --user -U "${packages[@]}"
