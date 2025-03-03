R codes for the web application serving the dataset from
“Retro-digitised Enggano-German dictionary derived from Kähler’s (1987)
“Enggano-Deutsches Wörterbuch””
================
[Gede Primahadi Wijaya
Rajeg](https://www.ling-phil.ox.ac.uk/people/gede-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-oxweb-logo.gif"
width="84" alt="The University of Oxford" />](https://www.ox.ac.uk/)
[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-lingphil.png"
width="83"
alt="Faculty of Linguistics, Philology and Phonetics, the University of Oxford" />](https://www.ling-phil.ox.ac.uk/)
[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-ahrc.png"
width="325" alt="Arts and Humanities Research Council (AHRC)" />](https://www.ukri.org/councils/ahrc/)
</br>*This work is part of the [AHRC-funded
project](https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1) on the
lexical resources for Enggano, led by the Faculty of Linguistics,
Philology and Phonetics at the University of Oxford, UK. Visit the
[central webpage of the Enggano
project](https://enggano.ling-phil.ox.ac.uk/)*.

<p xmlns:cc="http://creativecommons.org/ns#">
This work is licensed under
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC
BY-NC-SA 4.0
<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a>
</p>
<!-- badges: end -->

## How to cite

Please cite the original source ([Kähler 1987](#ref-kähler1987)) (if in
APA7<sup>th</sup>) and the particular version of the digitised
dictionary ([Rajeg et al. 2024](#ref-rajeg_kahler)) (in
[DataCite](https://support.datacite.org/docs/data-citation)) as follows:

> Kähler, H. (with Schmidt, H.). (1987). *Enggano-Deutsches Wörterbuch*.
> Dietrich Reimer Verlag. <https://search.worldcat.org/title/18191699>

> Rajeg, Gede Primahadi Wijaya; Pramartha, Cokorda Rai Adi;
> Sarasvananda, Ida Bagus Gede; Widiatmika, Putu Wahyu; Segara, Ida
> Bagus Made Ari; Pita, Yul Fulgensia Rusman; et al. (2024).
> Retro-digitised Enggano-German dictionary derived from Kähler’s (1987)
> “Enggano-Deutsches Wörterbuch”. University of Oxford. Dataset.
> <https://doi.org/10.25446/oxford.28057742.v1>

For future updates and version of records, please check the
[Releases](https://github.com/engganolang/kahler-1987-shiny/releases)
page on this GitHub repository.

## Overview

This repository hosts the source R codes for building the Shiny web
application serving the retro-digitised[^1] Enggano-German Dictionary
([Rajeg et al. 2024](#ref-rajeg_kahler)) by Hans Kähler
([1987](#ref-kähler1987)).

The work on the manual checking of the Indonesian translation from the
English translation is available
[here](https://doi.org/10.25446/oxford.28089452). Some of the words in
the dictionary are included in the Shiny web application for the
[*EnoLEX*](https://doi.org/10.25446/oxford.28282169) database ([Krauße
et al. 2024](#ref-krausse_enolex_2024); [Rajeg, Krauße & Pramartha
2024](#ref-rajeg_enolex_2024); [Rajeg et al. 2025](#ref-rajeg_r_2025)).

### References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-kähler1987" class="csl-entry">

Kähler, Hans. 1987. *Enggano-Deutsches wörterbuch* (Veröffentlichungen
Des Seminars Für Indonesische Und Südseesprachen Der Universität Hamburg
14). Berlin; Hamburg: Dietrich Reimer Verlag.

</div>

<div id="ref-krausse_enolex_2024" class="csl-entry">

Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Rai Adi Pramartha,
Erik Zobel, Bernd Nothofer, Charlotte Hemmings, Sarah Ogilvie, I Wayan
Arka & Mary Dalrymple. 2024. EnoLEX: A diachronic lexical database for
the Enggano language. Online database. University of Oxford.
<https://doi.org/10.25446/oxford.28282169.v1>.

</div>

<div id="ref-rajeg_enolex_2024" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Daniel Krauße & Cokorda Pramartha. 2024.
EnoLEX: A diachronic lexical database for the Enggano language. In Ai
Inoue, Naho Kawamoto & Makoto Sumiyoshi (eds.), *AsiaLex 2024
proceedings: Asian Lexicography - Merging cutting-edge and established
approaches*, 123–132. Toyo University, Tokyo, Japan.
<https://doi.org/10.25446/oxford.27013864>.

</div>

<div id="ref-rajeg_r_2025" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Daniel Krauße, Cokorda Rai Adi Pramartha,
Erik Zobel, Bernd Nothofer, Charlotte Hemmings, Sarah Ogilvie, I Wayan
Arka & Mary Dalrymple. 2025. R codes and curated dataset for “EnoLEX: A
Diachronic Lexical Database for the Enggano Language.” Computer
Software. Oxford, UK: University of Oxford.
<https://doi.org/10.25446/oxford.28282946.v1>.

</div>

<div id="ref-rajeg_kahler" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Cokorda Rai Adi Pramartha, Ida Bagus Gede
Sarasvananda, Putu Wahyu Widiatmika, Ida Bagus Made Ari Segara, Yul
Fulgensia Rusman Pita, Fitriani Putri Koemba, et al. 2024.
Retro-digitised Enggano-German dictionary derived from Kähler’s (1987)
“Enggano-Deutsches Wörterbuch.” Dataset. University of Oxford.
https://doi.org/<https://doi.org/10.25446/oxford.28057742.v1>.

</div>

</div>

[^1]: News related to the transcription of the Enggano-German dictionary
    are published
    [here](https://www.ling-phil.ox.ac.uk/news/2023/05/28/retro-digitisation-work-enggano-german-dictionary-udayana-university-indonesia)
    and
    [here](https://sasing.unud.ac.id/posts/boel-students-involved-in-research-project-led-by-researchers-from-the-university-of-oxford-uk)
