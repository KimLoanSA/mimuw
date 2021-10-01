import {fibonacci} from "./fibonacci.js";

const opoznienie = document.getElementById('opoznione') as HTMLElement;

const wejsiceStart = document.getElementById("start") as HTMLInputElement;
const wejscieKoniec = document.getElementById("destination") as HTMLInputElement;
const wejscieImie = document.getElementById("name") as HTMLInputElement;
const wejscieNazwisko = document.getElementById("surname") as HTMLInputElement;
const wejscieData = document.getElementById("date") as HTMLInputElement;

const potwierdzenieRejestracji =  document.getElementById('rezerwacja-popup') as HTMLElement;

const form = document.getElementById('rezerwacja-form') as HTMLElement;
const przycisk = document.getElementById("guzik-wyslij") as HTMLElement;

opoznienie.addEventListener('click', pokoloruj);

form.addEventListener('input', rozwiklajWejscie);
form.addEventListener('submit', rozwiklajPotwierdzenie);
form.addEventListener('reset', rozwiklajReset);

ukryjPrzyciskJesliBrakujePol();

function rozwiklajReset(): void {
  wejscieData.value="";
  ukryjPrzyciskJesliBrakujePol();
}

function rozwiklajWejscie(): void {
  ukryjPrzyciskJesliBrakujePol();
}

function ukryjPrzyciskJesliBrakujePol() {
  if (czyWszystkiePolaSaWypelnionePoprawnie()) {
    przycisk.removeAttribute("disabled");
  } else {
    przycisk.setAttribute("disabled", "true");
  }
}

function czyWszystkiePolaSaWypelnionePoprawnie(): boolean {
  return czyPoleZwyboremJestPoprawne(wejsiceStart)
    && czyPoleZwyboremJestPoprawne(wejscieKoniec)
    && czyPoleTeskstoweJestNiepuste(wejscieImie)
    && czyPoleTeskstoweJestNiepuste(wejscieNazwisko)
    && czyPoleZDataJestPoprawne(wejscieData);
}

function czyPoleTeskstoweJestNiepuste(pole: HTMLInputElement): boolean {
  return pole.value.trim().length > 0
}

function czyPoleZwyboremJestPoprawne(pole: HTMLInputElement): boolean {
  return pole.value !== "empty"
}

function czyPoleZDataJestPoprawne(pole: HTMLInputElement): boolean {
  const wpisanaData = new Date(Date.parse(pole.value));
  const dzisiejszaData = new Date (Date.now() - 86400000);

  return wpisanaData > dzisiejszaData
}

function rozwiklajPotwierdzenie(event: any): void {
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
function pokoloruj(mouseEvent: MouseEvent) {
  licznikKlikniec++;
  console.log(fibonacci(10 * licznikKlikniec));
  const cel = mouseEvent.target;
  // @ts-ignore
  const element = this as HTMLElement;


  const aktualnyKolor = window
  .getComputedStyle(element)
  .getPropertyValue('background-color');

  // @ts-ignore
  const [_,...koloryJakoTekst] =
      /rgb[a]?\((\d+),[^0-9]*(\d+),[^0-9]*(\d+)[,]?[^0-9]*(\d*)\)/
      .exec(aktualnyKolor);

  const colors: number[] = [];
  for(let i = 0; i < 3; i++) {
    colors[i] = (parseInt(koloryJakoTekst[i], 10) + 0x20) % 256;
  }

  element.style.backgroundColor = `rgb(${colors[0]},${colors[1]},${colors[2]})`;
}
