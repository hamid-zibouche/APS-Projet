#!/bin/bash
echo "=================================="
echo "🧪 Lancement des tests du evaluateur"
echo "=================================="
TIMEOUT_DURATION=3
make
# Parcours de tous les fichiers .aps dans le répertoire Test,
# qui se trouve à la racine (donc on remonte d'un niveau)
for file in Test/*.aps; do
   echo ""
    echo "🔹 Test sur le fichier : $file"
    timeout "${TIMEOUT_DURATION}s" ./evaluateur "$file"
    if [[ $? -eq 124 ]]; then
        echo "⏱️  Temps écoulé (${TIMEOUT_DURATION}s) — test interrompu."
    fi
    echo "----------------------------------"
done
make clean

echo "✅ Tests du evaluateur terminés."
