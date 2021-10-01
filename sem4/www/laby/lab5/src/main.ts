// niestety uzywam polskiego w kodze :/

function zaloguj(...komunikaty: string[]) {
  console.log("Ależ skomplikowany program!", ...komunikaty);
}

zaloguj("Ja", "cię", "nie", "mogę");

let jsonString: string = `{
    "piloci": [
        "Pirx",
        "Exupery",
        "Idzikowski",
        "Główczewski"
    ],
    "lotniska": {
        "WAW": ["Warszawa", [3690, 2800]],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

type Pilot = string;

interface Lotnisko {
  [name: string]: [string, number[]]
}

interface ILiniaLotnicza {
  piloci: [Pilot];
  lotniska: Lotnisko;
}


function sprawdzDaneLiniiLotniczej(dane: any): dane is ILiniaLotnicza {
  const pola: string[] = ["piloci", "lotniska"];

  let sparsowaneDane = JSON.parse(dane);

  return sprawdzCzyDaneMajaPola(sparsowaneDane, pola)
      && sprawdzPoprawnoscPilotow(sparsowaneDane)
      && sprawdzPoprawnoscLotnisk(sparsowaneDane);
}

function sprawdzCzyDaneMajaPola(dane: any, pola: string[]) {
  for (let pole of pola) {
    if (pole in dane == false) {
      return false;
    }
  }

  return true;
}

function sprawdzPoprawnoscPilotow(dane: any): boolean {
  return sprawdzCzyPoleJestTablicaDanegoTypu(dane.piloci, "string");
}

function sprawdzPoprawnoscLotnisk(dane: any): boolean {
  for (let lotnisko in dane.lotniska) {
    if (sprawdzPoprwnoscKrotekWLotnisku(dane.lotniska[lotnisko]) == false) {
      return false;
    }
  }

  return true;
}

function sprawdzPoprwnoscKrotekWLotnisku(dane: any) {
  return sprawdzCzyPoleJestTablica(dane)
      && dane.length == 2
      && sprawdzCzyPoleJestDanegoTypu(dane[0], "string")
      && sprawdzCzyPoleJestTablicaDanegoTypu(dane[1], "number");
}

function sprawdzCzyPoleJestTablicaDanegoTypu(dane: any, typ: string): boolean {
  return sprawdzCzyPoleJestTablica(dane)
      && sprawdzCzyKazdePoleJestDanegoTypu(dane, typ);
}

function sprawdzCzyKazdePoleJestDanegoTypu(dane: any[], typ: string): boolean {
  for (let pole of dane) {
    if (sprawdzCzyPoleJestDanegoTypu(pole, typ) == false) {
      return false;
    }
  }

  return true;
}

function sprawdzCzyPoleJestDanegoTypu(pole: any, typ: string): boolean {
  return typeof pole == typ
}

function sprawdzCzyPoleJestTablica(pole: any): boolean {
  return pole instanceof Array;
}


// =================================================================================
// testy

// dane do testow

let jsonStringPilociPolylkaWNazwie: string = `{
    "piloc": [
        "Pirx",
        "Exupery",
        "Idzikowski",
        "Główczewski"
    ],
    "lotniska": {
        "WAW": ["Warszawa", [3690, 2800]],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

let jsonStringLotniskaPolylkaWNazwie: string = `{
    "piloci": [
        "Pirx",
        "Exupery",
        "Idzikowski",
        "Główczewski"
    ],
    "lotnisk": {
        "WAW": ["Warszawa", [3690, 2800]],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

let jsonStringPilociPolylkaWPolu: string = `{
    "piloci": [
        "Pirx",
        1,
        "Idzikowski",
        "Główczewski"
    ],
    "lotniska": {
        "WAW": ["Warszawa", [3690, 2800]],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

let jsonStringLotniskaPolylkaWKrotce1: string = `{
    "piloci": [
        "Pirx",
        "Exupery",
        "Idzikowski",
        "Główczewski"
    ],
    "lotniska": {
        "WAW": [37, [3690, 2800]],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

let jsonStringLotniskaPolylkaWKrotce2: string = `{
    "piloci": [
        "Pirx",
        "Exupery",
        "Idzikowski",
        "Główczewski"
    ],
    "lotniska": {
        "WAW": ["Warszawa", ["hey", 2800]],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

let jsonStringLotniskaPolylkaWKrotce3: string = `{
    "piloci": [
        "Pirx",
        "Exupery",
        "Idzikowski",
        "Główczewski"
    ],
    "lotniska": {
        "WAW": ["Warszawa", ["hey", 2800], 2],
        "NRT": ["Narita", [4000, 2500]],
        "BQH": ["Biggin Hill", [1802, 792]],
        "LBG": ["Paris-Le Bourget", [2665, 3000, 1845]]
    }
}`;

// "odpalanie" testow

function sprawdzDaneTest(dane: any, oczekiwanaOdpowiedz: boolean, wiadomoscDlaBledu: string,) {
  if (sprawdzDaneLiniiLotniczej(dane) != oczekiwanaOdpowiedz) {
    console.error(wiadomoscDlaBledu);
  }
}

sprawdzDaneTest(jsonStringPilociPolylkaWNazwie, false, "nazwa `pilot`");
sprawdzDaneTest(jsonStringLotniskaPolylkaWNazwie, false, "nazwa `lotniska`");
sprawdzDaneTest(jsonStringPilociPolylkaWPolu, false, "pole `pilot`");
sprawdzDaneTest(jsonStringLotniskaPolylkaWKrotce1, false, "krotka 1 `lotniska`");
sprawdzDaneTest(jsonStringLotniskaPolylkaWKrotce2, false, "krotka 2 `lotniska`");
sprawdzDaneTest(jsonStringLotniskaPolylkaWKrotce3, false, "krotka 3 `lotniska`");
