import { fibonacci } from "./fibonacci.js";
'use strict';
let opoznienie = document.getElementById('opoznione');
let wejsiceStart = document.getElementById("start");
let wejscieKoniec = document.getElementById("destination");
let wejscieImie = document.getElementById("name");
let wejscieNazwisko = document.getElementById("surname");
let wejscieData = document.getElementById("date");
let potwierdzenieRejestracji = document.getElementById('rezerwacja-popup');
let form = document.getElementById('rezerwacja-form');
let przycisk = document.getElementById("guzik-wyslij");
opoznienie.addEventListener('click', pokoloruj);
form.addEventListener('input', rozwiklajWejscie);
form.addEventListener('submit', rozwiklajPotwierdzenie);
form.addEventListener('reset', rozwiklajReset);
ukryjPrzyciskJesliBrakujePol();
function rozwiklajReset() {
    wejscieData.value = "";
    ukryjPrzyciskJesliBrakujePol();
}
function rozwiklajWejscie() {
    ukryjPrzyciskJesliBrakujePol();
}
function ukryjPrzyciskJesliBrakujePol() {
    if (czyWszystkiePolaSaWypelnionePoprawnie()) {
        przycisk.removeAttribute("disabled");
    }
    else {
        przycisk.setAttribute("disabled", "true");
    }
}
function czyWszystkiePolaSaWypelnionePoprawnie() {
    return czyPoleZwyboremJestPoprawne(wejsiceStart)
        && czyPoleZwyboremJestPoprawne(wejscieKoniec)
        && czyPoleTeskstoweJestNiepuste(wejscieImie)
        && czyPoleTeskstoweJestNiepuste(wejscieNazwisko)
        && czyPoleZDataJestPoprawne(wejscieData);
}
function czyPoleTeskstoweJestNiepuste(pole) {
    return pole.value.trim().length > 0;
}
function czyPoleZwyboremJestPoprawne(pole) {
    return pole.value != "empty";
}
function czyPoleZDataJestPoprawne(pole) {
    let wpisanaData = new Date(Date.parse(pole.value));
    let dzisiejszaData = new Date(Date.now() - 86400000);
    return wpisanaData >= dzisiejszaData;
}
function rozwiklajPotwierdzenie(event) {
    potwierdzenieRejestracji.textContent = `
    Skąd: ${wejsiceStart.value}
    Dokąd: ${wejscieKoniec.value}
    Imie: ${wejscieImie.value}
    Nazwisko: ${wejscieNazwisko.value}
    Data: ${wejscieData.value}`;
    potwierdzenieRejestracji.style.visibility = "visible";
    event.preventDefault();
}
let licznikKlikniec = 0;
function pokoloruj(mouseEvent) {
    licznikKlikniec++;
    console.log(fibonacci(10 * licznikKlikniec));
    let cel = mouseEvent.target;
    // @ts-ignore
    let element = this;
    let aktualnyKolor = window
        .getComputedStyle(element)
        .getPropertyValue('background-color');
    // @ts-ignore
    let [_, ...koloryJakoTekst] = /rgb[a]?\((\d+),[^0-9]*(\d+),[^0-9]*(\d+)[,]?[^0-9]*(\d*)\)/
        .exec(aktualnyKolor);
    let colors = [];
    for (let i = 0; i < 3; i++) {
        colors[i] = (parseInt(koloryJakoTekst[i]) + 0x20) % 256;
    }
    element.style.backgroundColor = `rgb(${colors[0]},${colors[1]},${colors[2]})`;
}
