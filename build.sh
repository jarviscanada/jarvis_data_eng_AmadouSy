#!/bin/bash

# Script to compile all COBOL programs and create the PRGMENU executable

echo "Compiling COBOL programs..."

cobc -x -o PRGMENU \
  PRGMENU.cob \
  PRGV0001.cob \
  PRGI0002.cob \
  PRGU0003.cob \
  PRGD0004.cob \
  PRGQ0005.cob \
  PRGQ0006.cob \
  PRGQ0007.cob \
  PRGR0008.cob

if [ $? -eq 0 ]; then
  echo "✅ Compilation successful! Run it with: ./PRGMENU"
else
  echo "❌ Compilation failed. Please check for errors."
fi
