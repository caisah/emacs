echo "Cleaning..."
cd ~/.emacs.d;
echo "Crisp!"
rm -rf .cask;
cask update;
cask upgrade;
cask;
echo "Cask is out!";
