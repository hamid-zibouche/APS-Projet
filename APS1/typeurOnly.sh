#!/bin/bash
echo "=================================="
echo "🧪 Lancement des tests du typeur"
echo "=================================="

make
# Parcours de tous les fichiers .aps dans le répertoire Test,
# qui se trouve à la racine (donc on remonte d'un niveau)
for file in Test/*.aps; do
    echo ""
    echo "🔹 Test sur le fichier : $file"
    ./prologTerm "$file" | swipl typeur.pl
    echo "----------------------------------"
done
make clean

echo "✅ Tests du typeur terminés."
