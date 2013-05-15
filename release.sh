## Release script for HackageDB.

echo Building Haskell package.
cd Haskell
release.sh
cd ..
echo Done.
echo

echo Building JavaScript library from Informl source.
informl -js Informl/Imparse.iml
mv Informl/Imparse.js JavaScript/Imparse.js
echo Done.
echo

echo Release script for imparse finished.
echo

##eof
