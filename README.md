R codes for the Enggano-German Dictionary web application serving the
dataset from “Retro-digitised Enggano-German dictionary derived from
Kähler’s (1987) “Enggano-Deutsches Wörterbuch””
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

[![DOI](https://img.shields.io/badge/doi-10.25446/oxford.28540331-blue.svg?style=flat&labelColor=whitesmoke&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAAB8AAAAfCAYAAAAfrhY5AAAJsklEQVR42qWXd1DTaRrHf%2BiB2Hdt5zhrAUKz4IKEYu9IGiGFFJJQ0gkJCAKiWFDWBRdFhCQUF3UVdeVcRQEBxUI3yY9iEnQHb3bdW1fPubnyz%2F11M7lvEHfOQee2ZOYzPyDv%2B3yf9%2Fk95YX4fx%2BltfUt08GcFEuPR4U9hDDZ%2FVngIlhb%2FSiI6InkTgLzgDcgfvtnovhH4BzoVlrbwr55QnhCtBW4QHXnFrZbPBaQoBh4%2FSYH2EnpBEtqcDMVzB93wA%2F8AFwa23XFGcc8CkT3mxz%2BfXWtq9T9IQlLIXYEuHojudb%2BCM7Hgdq8ydi%2FAHiBXyY%2BLjwFlAEnS6Jnar%2FvnQVhvdzasad0eKvWZKe8hvDB2ofLZ%2FZEcWsh%2BhyIuyO5Bxs2iZIE4nRv7NWAb0EO8AC%2FWPxjYAWuOEX2MSXZVgPxzmRL3xKz3ScGpx6p6QnOx4mDIFqO0w6Q4fEhO5IzwxlSwyD2FYHzwAW%2BAZ4fEsf74gCumykwNHskLM7taQxLYjjIyy8MUtraGhTWdkfhkFJqtvuVl%2F9l2ZquDfEyrH8B0W06nnpH3JtIyRGpH1iJ6SfxDIHjRXHJmdQjLpfHeN54gnfFx4W9QRnovx%2FN20aXZeTD2J84hn3%2BqoF2Tqr14VqTPUCIcP%2B5%2Fly4qC%2BUL3sYxSvNj1NwsVYPsWdMUfomsdkYm3Tj0nbV0N1wRKwFe1MgKACDIBdMAhPE%2FwicwNWxll8Ag40w%2BFfhibJkGHmutjYeQ8gVlaN%2BjO51nDysa9TwNUFMqaGbKdRJZFfOJSp6mkRKsv0rRIpEVWjAvyFkxNOEpwvcAVPfEe%2Bl8ojeNTx3nXLBcWRrYGxSRjDEk0VlpxYrbe1ZmaQ5xuT0u3r%2B2qe5j0J5uytiZPGsRL2Jm32AldpxPUNJ3jmmsN4x62z1cXrbedXBQf2yvIFCeZrtyicZZG2U2nrrBJzYorI2EXLrvTfCSB43s41PKEvbZDEfQby6L4JTj%2FfIwam%2B4%2BwucBu%2BDgNK05Nle1rSt9HvR%2FKPC4U6LTfvUIaip1mjIa8fPzykii23h2eanT57zQ7fsyYH5QjywwlooAUcAdOh5QumgTHx6aAO7%2FL52eaQNEShrxfhL6albEDmfhGflrsT4tps8gTHNOJbeDeBlt0WJWDHSgxs6cW6lQqyg1FpD5ZVDfhn1HYFF1y4Eiaqa18pQf3zzYMBhcanlBjYfgWNayAf%2FASOgklu8bmgD7hADrk4cRlOL7NSOewEcbqSmaivT33QuFdHXj5sdvjlN5yMDrAECmdgDWG2L8P%2BAKLs9ZLZ7dJda%2BB4Xl84t7QvnKfvpXJv9obz2KgK8dXyqISyV0sXGZ0U47hOA%2FAiigbEMECJxC9aoKp86re5O5prxOlHkcksutSQJzxZRlPZmrOKhsQBF5zEZKybUC0vVjG8PqOnhOq46qyDTDnj5gZBriWCk4DvXrudQnXQmnXblebhAC2cCB6zIbM4PYgGl0elPSgIf3iFEA21aLdHYLHUQuVkpgi02SxFdrG862Y8ymYGMvXDzUmiX8DS5vKZyZlGmsSgQqfLub5RyLNS4zfDiZc9Edzh%2FtCE%2BX8j9k%2FqWB071rcZyMImne1SLkL4GRw4UPHMV3jjwEYpPG5uW5fAEot0aTSJnsGAwHJi2nvF1Y5OIqWziVCQd5NT7t6Q8guOSpgS%2Fa1dSRn8JGGaCD3BPXDyQRG4Bqhu8XrgAp0yy8DMSvvyVXDgJcJTcr1wQ2BvFKf65jqhvmxXUuDpGBlRvV36XvGjQzLi8KAKT2lYOnmxQPGorURSV0NhyTIuIyqOmKTMhQ%2BieEsgOgpc4KBbfDM4B3SIgFljvfHF6cef7qpyLBXAiQcXvg5l3Iunp%2FWv4dH6qFziO%2BL9PbrimQ9RY6MQphEfGUpOmma7KkGzuS8sPUFnCtIYcKCaI9EXo4HlQLgGrBjbiK5EqMj2AKWt9QWcIFMtnVvQVDQV9lXJJqdPVtUQpbh6gCI2Ov1nvZts7yYdsnvRgxiWFOtNJcOMVLn1vgptVi6qrNiFOfEjHCDB3J%2BHDLqUB77YgQGwX%2Fb1eYna3hGKdlqJKIyiE4nSbV8VFgxmxR4b5mVkkeUhMgs5YTi4ja2XZ009xJRHdkfwMi%2BfocaancuO7h%2FMlcLOa0V%2FSw6Dq47CumRQAKhgbOP8t%2BMTjuxjJGhXCY6XpmDDFqWlVYbQ1aDJ5Cptdw4oLbf3Ck%2BdWkVP0LpH7s9XLPXI%2FQX8ws%2Bj2In63IcRvOOo%2BTTjiN%2BlssfRsanW%2B3REVKoavBOAPTXABW4AL7e4NygHdpAKBscmlDh9Jysp4wxbnUNna3L3xBvyE1jyrGIkUHaqQMuxhHElV6oj1picvgL1QEuS5PyZTEaivqh5vUCKJqOuIgPFGESns8kyFk7%2FDxyima3cYxi%2FYOQCj%2F%2B9Ms2Ll%2Bhn4FmKnl7JkGXQGDKDAz9rUGL1TIlBpuJr9Be2JjK6qPzyDg495UxXYF7JY1qKimw9jWjF0iV6DRIqE%2B%2FeWG0J2ofmZTk0mLYVd4GLiFCOoKR0Cg727tWq981InYynvCuKW43aXgEjofVbxIqrm0VL76zlH3gQzWP3R3Bv9oXxclrlO7VVtgBRpSP4hMFWJ8BrUSBCJXC07l40X4jWuvtc42ofNCxtlX2JH6bdeojXgTh5TxOBKEyY5wvBE%2BACh8BtOPNPkApjoxi5h%2B%2FFMQQNpWvZaMH7MKFu5Ax8HoCQdmGkJrtnOiLHwD3uS5y8%2F2xTSDrE%2F4PT1yqtt6vGe8ldMBVMEPd6KwqiYECHDlfbvzphcWP%2BJiZuL5swoWQYlS%2Br7Yu5mNUiGD2retxBi9fl6RDGn4Ti9B1oyYy%2BMP5G87D%2FCpRlvdnuy0PY6RC8BzTA40NXqckQ9TaOUDywkYsudxJzPgyDoAWn%2BB6nEFbaVxxC6UXjJiuDkW9TWq7uRBOJocky9iMfUhGpv%2FdQuVVIuGjYqACbXf8aa%2BPeYNIHZsM7l4s5gAQuUAzRUoT51hnH3EWofXf2vkD5HJJ33vwE%2FaEWp36GHr6GpMaH4AAPuqM5eabH%2FhfG9zcCz4nN6cPinuAw6IHwtvyB%2FdO1toZciBaPh25U0ducR2PI3Zl7mokyLWKkSnEDOg1x5fCsJE9EKhH7HwFNhWMGMS7%2BqxyYsbHHRUDUH4I%2FAheQY7wujJNnFUH4KdCju83riuQeHU9WEqNzjsJFuF%2FdTDAZ%2FK7%2F1WaAU%2BAWymT59pVMT4g2AxcwNa0XEBDdBDpAPvgDIH73R25teeuAF5ime2Ul0OUIiG4GpSAEJeYW9wDTf43wfwHgHLKJoPznkwAAAABJRU5ErkJggg%3D%3D)](http://dx.doi.org/10.25446/oxford.28540331)
<a href="https://doi.org/10.5281/zenodo.14969432" target="_blank"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.14969432.svg" alt="DOI"></a>

<!-- badges: end -->

## How to cite

Please cite the original source ([Kähler 1987](#ref-kähler1987)) (if in
APA7<sup>th</sup>), the particular version of the digitised dictionary
dataset ([Rajeg et al. 2024](#ref-rajeg_kahler)), this repository of the
R code ([Rajeg 2025](#ref-rajeg_kahler_shiny_code_2025)), and the web
application ([Rajeg, Pramartha, et al. 2025](#ref-rajeg_kahler_2025))
(in [DataCite](https://support.datacite.org/docs/data-citation)) as
follows:

> Kähler, H. (with Schmidt, H.). (1987). *Enggano-Deutsches Wörterbuch*.
> Dietrich Reimer Verlag. <https://search.worldcat.org/title/18191699>

> Rajeg, Gede Primahadi Wijaya; Pramartha, Cokorda Rai Adi;
> Sarasvananda, Ida Bagus Gede; Widiatmika, Putu Wahyu; Segara, Ida
> Bagus Made Ari; Pita, Yul Fulgensia Rusman; et al. (2024).
> Retro-digitised Enggano-German dictionary derived from Kähler’s (1987)
> “Enggano-Deutsches Wörterbuch”. University of Oxford. Dataset.
> <https://doi.org/10.25446/oxford.28057742.v1>

> Rajeg, Gede Primahadi Wijaya (2025). R codes for the Enggano-German
> Dictionary web application serving the dataset from “Retro-digitised
> Enggano-German dictionary derived from Kähler’s (1987)
> *Enggano-Deutsches Wörterbuch*”. University of Oxford. Software.
> <https://doi.org/10.25446/oxford.28540331>

> Rajeg, Gede Primahadi Wijaya; Pramartha, Cokorda Rai Adi;
> Sarasvananda, Ida Bagus Gede; Widiatmika, Putu Wahyu; Segara, Ida
> Bagus Made Ari; Pita, Yul Fulgensia Rusman; et al. (2025). The
> Enggano-German Dictionary online derived from Kähler’s (1987)
> “Enggano-Deutsches Wörterbuch”. University of Oxford. Online Resource.
> <https://doi.org/10.25446/oxford.28532666>
> <https://enggano.shinyapps.io/enggano-german-dictionary/>

For future updates, please check the
[Releases](https://github.com/engganolang/kahler-1987-shiny/releases)
page on this GitHub repository.

## Overview

This repository hosts the source R codes ([Rajeg
2025](#ref-rajeg_kahler_shiny_code_2025)) for building [the
Enggano-German Dictionary online
web-application](https://enggano.shinyapps.io/enggano-german-dictionary/)
([Rajeg, Pramartha, et al. 2025](#ref-rajeg_kahler_2025)) serving the
retro-digitised[^1] Enggano-German Dictionary dataset ([Rajeg et al.
2024](#ref-rajeg_kahler)) by Hans Kähler ([1987](#ref-kähler1987)).

The work on the manual checking of the Indonesian translation from the
English translation is available
[here](https://doi.org/10.25446/oxford.28089452). Some of the words in
the dictionary are included in the Shiny web application for the
[*EnoLEX*](https://doi.org/10.25446/oxford.28282169) database ([Krauße
et al. 2024](#ref-krausse_enolex_2024); [Rajeg, Krauße & Pramartha
2024](#ref-rajeg_enolex_2024); [Rajeg, Krauße, et al.
2025](#ref-rajeg_r_2025)).

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

<div id="ref-rajeg_kahler_shiny_code_2025" class="csl-entry">

Rajeg, Gede Primahadi Wijaya. 2025. R codes for the Enggano-German
Dictionary web application serving the dataset from
"<span class="nocase">Retro-digitised Enggano-German dictionary derived
from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”</span>". Computer
Software. Oxford, UK: University of Oxford.
<https://doi.org/10.25446/oxford.28540331>.

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
<https://doi.org/10.25446/oxford.28057742.v1>.

</div>

<div id="ref-rajeg_kahler_2025" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Cokorda Rai Adi Pramartha, Ida Bagus Gede
Sarasvananda, Putu Wahyu Widiatmika, Ida Bagus Made Ari Segara, Yul
Fulgensia Rusman Pita, Fitriani Putri Koemba, et al. 2025. The
Enggano-German Dictionary online derived from Kähler’s (1987)
“Enggano-Deutsches Wörterbuch.” Online resource. University of Oxford.
<https://doi.org/10.25446/oxford.28532666>.

</div>

</div>

[^1]: News related to the transcription of the Enggano-German dictionary
    are published
    [here](https://www.ling-phil.ox.ac.uk/news/2023/05/28/retro-digitisation-work-enggano-german-dictionary-udayana-university-indonesia)
    and
    [here](https://sasing.unud.ac.id/posts/boel-students-involved-in-research-project-led-by-researchers-from-the-university-of-oxford-uk)
