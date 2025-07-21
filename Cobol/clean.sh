#!/bin/bash
echo "Cleaning COBOL build files..."

# Supprime les fichiers objets et modules
rm -f *.o *.so

# Supprime l'exécutable du menu principal
rm -f PRGMENU
rm -f PRGV0001
rm -f PRGI0002
rm -f PRGU0003
rm -f PRGD0004
rm -f PRGQ0005
rm -f PRGQ0006
rm -f PRGQ0007
rm -f PRGR0008

# Supprime le fichier simulé
rm -f vsam-simulated.txt
rm -f report.txt
rm -f temp.txt

echo "✅ Clean complete. You can now run ./build.sh"
