import * as fs from 'fs';
import {promisify} from 'util';

let open = promisify(fs.open);
let write = promisify(fs.write);
let close = promisify(fs.close);


fs.open('plik.txt', 'a', (err, fd) => {
  if (err) {
    console.log('Nie udało się otworzyć pliku :(', err);
    return;
  }
  fs.write(fd, 'Kolejny wpis do pliku!\n', (err, written,str) => {
    if (err) {
      console.log('Nie udało się zapisać', err);
    }
    fs.close(fd, () => {});
  });
});

let fd;
open('plik2.txt', 'a').then((_fd) => {
  fd = _fd;
  write(fd, 'A z promisami też się może zapisze?\n');
}).then(() => close(fd)).catch((reason) => {
  console.log('Błąd był straszliwy!', reason);
});

zapiszCos();

async function zapiszCos() {
  let fd = -1;
  try {
    fd = await open('plik3.txt', 'a');
    await write(fd, 'To jeszcze z async/await');
    await close(fd);
  } catch (e) {
    console.log('Jakiś błąd w trakcie zapisywania', e);
    if (fd != -1) {
      await close(fd);
    }
  }
}
