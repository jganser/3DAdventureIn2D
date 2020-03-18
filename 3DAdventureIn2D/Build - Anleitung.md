# Build - Anleitung

Das Spiel lässt sich mit Hilfe der Software ```stack``` installieren.

## Stack installieren

Eine Installationsanleitung findet sich unter der offiziellen Dokumentation auf der Seite [docs.haskellstack.org](https://docs.haskellstack.org/en/stable/README/).

## Stack updaten

Im zweiten Schritt nach der Installation muss ```stack``` upgedated werden.

Dies wird  mit dem Kommando ```stack update``` erreicht.

## Build starten

Nachdem ```stack``` sein Update erfahren hat, kann das Projekt gebuilded werden.

Dies wird mit dem Kommando ```stack build``` erreicht.

## Ausführen

Nach dem build kann mit dem Kommando ```stack exec 3DAdventureIn2D-exe``` das Spiel gestartet werden.



## Kommandos im Überblick

```stack update``` $\longrightarrow$ updated stack und seine Repository Liste

```stack build``` $\longrightarrow$ startet den build process. Lädt alle nötigen Haskell Module herunter.

```stack exec 3DAdventureIn2D-exe``` $\longrightarrow$ führt die gebuildete ausführbare Datei des Projektes aus



