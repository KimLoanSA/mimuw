import { expect } from "chai";
import "mocha";
import { driver } from 'mocha-webdriver';

const guzikZatwierdz = "input[type=submit]";
const sciezka = `file://${process.cwd()}/listaLotow.html`;
const poprawneImie = "Marcin";
const poprawneNazwisko = "Abramowicz";
const poprawnyStart = "warszawa";
const poprawnyKoniec = "lodz";
const pustySelect = "empty";
const przeszlaData = "2000-01-01";
const przyszlaData = "2222-01-01";

const dzisiaj = new Date();
const dd = String(dzisiaj.getDate()).padStart(2, '0');
const mm = String(dzisiaj.getMonth() + 1).padStart(2, '0');
const yyyy = dzisiaj.getFullYear();

const dzisiejszaData = `${yyyy}-${mm}-${dd}`;

describe('Form', function () {
  it("should block button if name is empty", async function() {
    await uruchomTestDlaZablokowanegoGuzika(this, "", poprawneNazwisko, poprawnyStart, poprawnyKoniec, przeszlaData);
  });

  it("should block button if surname is empty", async function() {
    await uruchomTestDlaZablokowanegoGuzika(this, poprawneImie, "", poprawnyStart, poprawnyKoniec, przyszlaData);
  });

  it("should block button if from is empty", async function() {
    await uruchomTestDlaZablokowanegoGuzika(this, poprawneImie, poprawneNazwisko, pustySelect, poprawnyKoniec, przyszlaData);
  });

  it("should block button if destination is empty", async function() {
    await uruchomTestDlaZablokowanegoGuzika(this, poprawneImie, poprawneNazwisko, poprawnyStart, pustySelect, przyszlaData);
  });

  it("should block button if date is invalid", async function() {
    await uruchomTestDlaZablokowanegoGuzika(this, poprawneImie, poprawneNazwisko, poprawnyStart, poprawnyKoniec, przeszlaData);
  });

  it("should unblock button if date is today", async function() {
    await uruchomTestDlaOdblokowanegoGuzika(this, poprawneImie, poprawneNazwisko, poprawnyStart, poprawnyKoniec, dzisiejszaData);
  });

  it("should unblock button if date is today", async function() {
    await uruchomTestDlaDomyslnychDanych(this, poprawnyStart, poprawnyKoniec, dzisiejszaData);
  });

  it("should unblock button if input correct", async function() {
    await uruchomTestDlaOdblokowanegoGuzika(this, poprawneImie, poprawneNazwisko, poprawnyStart, poprawnyKoniec, przyszlaData);
  });
});

async function uruchomTestDlaDomyslnychDanych(kontekst: any, start: string, koniec: string, data: string) {
  kontekst.timeout(4000);

  await driver.get(sciezka);
  await wypelnijPolaBezDanych(start, koniec, data);

  expect(await driver.find(guzikZatwierdz).isEnabled()).to.equal(true);
}

async function uruchomTestDlaZablokowanegoGuzika(kontekst: any, imie: string, nazwisko: string, start: string, koniec: string, data: string) {
  await uruchomTest(kontekst, imie, nazwisko, start, koniec, data, false);
}

async function uruchomTestDlaOdblokowanegoGuzika(kontekst: any, imie: string, nazwisko: string, start: string, koniec: string, data: string) {
  await uruchomTest(kontekst, imie, nazwisko, start, koniec, data, true);
}

async function uruchomTest(kontekst: any, imie: string, nazwisko: string, start: string, koniec: string, data: string, oczekiwana: boolean) {
  kontekst.timeout(4000);

  await driver.get(sciezka);
  await wypelnijPola(imie, nazwisko, start, koniec, data);

  expect(await driver.find(guzikZatwierdz).isEnabled()).to.equal(oczekiwana);
}

async function wypelnijPola(imie: string, nazwisko: string, start: string, koniec: string, data: string) {
  await wypelnijPole("input", "name", imie);
  await wypelnijPole("input", "surname", nazwisko);
  await wypelnijPolaBezDanych(start, koniec, data);
}

async function wypelnijPolaBezDanych(start: string, koniec: string, data: string) {
  await wypelnijPole("select", "start", start);
  await wypelnijPole("select", "destination", koniec);
  await wypelnijPole("input", "date", data);
}

async function wypelnijPole(typPola: string, nazwaPola: string, wartosc: string) {
  const wejscie = await driver.find(`${typPola}[name=${nazwaPola}]`);
  if (typPola ==="input") {
    await wejscie.clear();
  }
  await wejscie.sendKeys(wartosc);
}
