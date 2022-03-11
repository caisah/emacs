#!/bin/bash
set -e

echo "Cleaning..."
cd ~/.emacs.d
echo "Removing old packages"
rm -rf .cask
echo "Crisp!"
echo "Upgrading cask itself"
cask upgrade-cask
echo "Upgrading/Installing packages..."
cask update
cask upgrade
cask
echo "Cask is out!"
