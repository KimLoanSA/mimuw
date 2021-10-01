let pasazerZNajwiekszymId = znajdzPasazeraZNajwiekszymId();
console.log(dajImieINazwiskoPasazera(pasazerZNajwiekszymId));

setTimeout(() => {
  console.log('No już wreszcie.');
}, 2000);

// teczoweKolory(document.querySelector('ol'));
// teczoweKolory2(document.querySelector('ol'));
teczoweKolory3(document.querySelector('ol'));

utworzNowyElementNaKoncuZTeskstem("zdjęcie autora najnowszego commitu z repozytorium TypeScript na githubie:");
wyswietlZdjecieAutoraOstatniegoCommitu();

function znajdzPasazeraZNajwiekszymId(): Element {
  const ol = document.querySelector('ol');
  const pasazerowie = ol.getElementsByClassName('pasazer');
  const maxIndeksPasazera = znajdzIndeksPasazeraZNajwiekszymId(pasazerowie);

  return pasazerowie[maxIndeksPasazera];
}

function znajdzIndeksPasazeraZNajwiekszymId(pasazerowie) {
  let maxIdPasazera: string = "";
  let maxIndeksPasazera = 0;

  for (let i = 0; i < pasazerowie.length; i++) {
    const idPasazera = pasazerowie[i].getAttribute('data-identyfikator-pasazera');

    if (idPasazera > maxIdPasazera) {
      maxIdPasazera = idPasazera;
      maxIndeksPasazera = i;
    }
  }

  return maxIndeksPasazera;
}

function dajImieINazwiskoPasazera(pasazerZNajwiekszymId): string {
  const imieINazwiskoRegex = "(.*) (.*)";

  return pasazerZNajwiekszymId
    .textContent
    .match(imieINazwiskoRegex)
    .shift();
}


function teczoweKolory(el: HTMLElement) {
  poczekaj(1000)
  .then(() => console.log('red'))
  .then(() => el.style.backgroundColor = 'red')
  .then(() =>
      poczekaj(1000)
      .then(() => el.style.backgroundColor = 'orange'))
  .then(() =>
      poczekaj(1000)
      .then(() => el.style.backgroundColor = 'yellow'))
  .then(() =>
      poczekaj(1000)
      .then(() => el.style.backgroundColor = 'green'))
  .then(() =>
      poczekaj(1000)
      .then(() => el.style.backgroundColor = 'blue'))
  .then(() =>
      poczekaj(1000)
      .then(() => el.style.backgroundColor = 'indigo'))
  .then(() =>
      poczekaj(1000)
      .then(() => el.style.backgroundColor = 'purple'));
}

function teczoweKolory2(el: HTMLElement) {
  let promise = poczekaj(1000);
  const kolory = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'purple'];

  for (const kolor of kolory) {
    promise = promise
    .then(() => el.style.backgroundColor = kolor)
    .then(() => poczekaj(1000));
  }
}

async function teczoweKolory3(el: HTMLElement) {
  const kolory = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'purple'];

  for (const kolor of kolory) {
    await poczekaj(1000);
    el.style.backgroundColor = kolor;
  }

}

function poczekaj(czasCzekaniaWMilisekundach: number) {
  return new Promise(lambda => setTimeout(lambda, czasCzekaniaWMilisekundach));
}

function wyswietlZdjecieAutoraOstatniegoCommitu() {
  const url = "https://api.github.com/repos/Microsoft/TypeScript/commits";

  fetch(url)
  .then(o => o.json())
  .then(o => o[0].author.avatar_url)
  .then(o => utworzNowyElementNaKoncuZObrazkiem(o))
  .catch(o => console.log("nie udalo sie: ", o));
}

function utworzNowyElementNaKoncuZTeskstem(tekst: string) {
  const nowyElement = document.createElement('div');
  nowyElement.textContent = tekst;
  document.body.appendChild(nowyElement);
}

function utworzNowyElementNaKoncuZObrazkiem(url: string) {
  console.log(url);

  const nowyElement = document.createElement('img');
  nowyElement.src = url;
  document.body.appendChild(nowyElement);
}
