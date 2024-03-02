# Cantara Song Score Format (cssf) documentation and parser

In this repository, the documentation of the Cantara song score format (cssf) and the implementation of the parser can be found. **Work is currently in progress.**

## About

The Cantara song score format (cssf) aims to provide an easy to read and write text format for the purpose of describing songs in all of their complexity. This includes–but is not limited to:

* Handling of different song parts (stanzas, refrains, bridges) as well as their repetions (e.g. a refrain follows after every stanza, a bridge comes before the last refrain)
* Handling of different information concerning the song content, such as lyrics, scores (different voices+instruments) and chords which allow both the creation of score sheets via Lilypond and the song presentations via Cantara.
* Handling of multible languages and translations of the song lyrics.

The format syntax follows a markdown-like approach. The description of scores and chords follows the Lilypond syntax.

The cantara song score format does not describe any formation such as score size, paper size etc. but is supposed to be format independent.

## examples

Please have a look at the folder [testfiles](testfiles/).
