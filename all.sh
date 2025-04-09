#!/bin/bash

# Durée max pour un test en secondes
TIMEOUT_DURATION=3

clean_all() {
    echo ""
    echo "🧹 Nettoyage des répertoires APS..."
    for dir in APS0 APS1 APS1a APS2; do
        if [ -d "$dir" ]; then
            echo "🧼 $dir : make clean"
            (cd "$dir" && make clean)
        fi
    done
}

while true; do
    echo "================================="
    echo "🌐 Choisissez un langage :"
    select lang in APS0 APS1 APS1a APS2 "Quitter"; do
        if [[ "$lang" == "Quitter" ]]; then
            clean_all
            echo "👋 Fin du script. À bientôt !"
            exit 0
        elif [[ -n "$lang" ]]; then
            echo "Langage sélectionné : $lang"
            break
        else
            echo "❌ Sélection invalide."
        fi
    done

    # Compilation
    echo "🔧 Compilation avec make dans le dossier $lang..."
    cd "$lang" || { echo "❌ Erreur : dossier $lang introuvable."; exit 1; }
    make || { echo "❌ Erreur lors du make."; exit 1; }

    while true; do
        echo ""
        echo "================================="
        echo "⚙️  Choisissez une action :"
        select action in "Tester le typeur" "Tester l’évaluateur" "Retour" "Quitter"; do
            case $REPLY in
                1)
                    echo "=============================="
                    echo "🧪 Lancement des tests du typeur"
                    echo "=============================="
                    for file in Test/*.aps; do
                        echo ""
                        echo "🔹 Fichier : $file"
                        ./prologTerm "$file" | swipl typeur.pl
                        echo "------------------------------"
                    done
                    break
                    ;;
                2)
                    echo "=============================="
                    echo "🧪 Lancement des tests de l’évaluateur"
                    echo "=============================="
                    for file in Test/*.aps; do
                        echo ""
                        echo "🔹 Fichier : $file"
                        timeout "$TIMEOUT_DURATION"s ./evaluateur "$file"
                        if [[ $? -eq 124 ]]; then
                            echo "⏱️  Temps écoulé ($TIMEOUT_DURATION s) — test interrompu."
                        fi
                        echo "------------------------------"
                    done
                    break
                    ;;
                3)
                    echo "↩️  Retour au choix du langage..."
                    cd ..
                    break 2  # sort des deux boucles pour revenir au menu langage
                    ;;
                4)
                    cd ..
                    clean_all
                    echo "👋 Fin du script. À bientôt !"
                    exit 0
                    ;;
                *)
                    echo "❌ Option invalide."
                    ;;
            esac
        done
    done
done
