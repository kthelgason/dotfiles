# -*- mode: ruby; -*-

# Install with:
#
# $ brew bundle

# Gather system information
require 'socket'

OSX_VERSION = IO.popen(['sw_vers', '-productVersion']) do |source|
  Gem::Version.new(source.read)
end
IS_SIERRA = OSX_VERSION >= Gem::Version.new('10.12')

# Required taps
tap 'homebrew/dupes'

# System tools
brew 'htop'

# Network tools
brew 'nmap'
brew 'privoxy'

# Core tools (have GNU at hand when needed)
brew 'coreutils'
brew 'findutils'
brew 'gnu-sed'
brew 'automake'

# Shell
brew 'zsh'                      
brew 'tmux'                     
brew 'reattach-to-user-namespace'
brew 'tree'                    
brew 'z'                 
cask 'go2shell'                 

# Tools
brew 'jq'                       

# Editing
brew 'macvim', args: ['override-system-vi']
brew 'emacs', args: ['with-cocoa']

# Images
brew 'imagemagick'              

ffmpeg_options = [
	'with-libvpx',
	'with-opus',
	'with-x265',
	'without-qtkit'
]

# Multimedia
brew 'ffmpeg', args: ffmpeg_options

# Dotfile management
brew 'stow'

# Crypto (mostly for git) & Security
brew 'gnupg@2.1'
brew 'pinentry-mac'

# Git
brew 'git'
brew 'git-standup'
brew 'git-cal'

# Development tools
cask 'p4merge'
brew 'the_silver_searcher'

# Databases
brew 'postgres', restart_service: true

# Javascript
brew 'node'
brew 'yarn'

# Various other languages
brew 'shellcheck'
brew 'ruby'
brew 'python3'
brew 'haskell-stack'
