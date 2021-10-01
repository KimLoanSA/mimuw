import {createServer, IncomingMessage, ServerResponse} from 'http';
import * as sqlite3 from "sqlite3";

let statisticsUrl: string = '/statystyki';
let databaseName: string = 'stats.db';
let errorMessage: string = 'pewnie zly url : (';

interface FilesStatistics {
  fileName: string,
  counter: number
}

createDatabase();
let server = createServer(resolveResponse);
server.listen(8080);

function resolveResponse(request: IncomingMessage, response: ServerResponse) {
  let responseStatusCode = getResponseStatusCode(request.url);

  response.statusCode = responseStatusCode;
  response.write(getResponse(request.url, responseStatusCode));
  response.end();
}

function getResponseStatusCode(requestUrl: string): number {
  if (isUrlValid(requestUrl)) {
    return 200;
  } else {
    return 500;
  }
}

function isUrlValid(url: string) {
  return isOnyOneSlashInUrl(url)
    && isUrlNonEmpty(url);
}

function isOnyOneSlashInUrl(url: string) {
  let slash_separator = '/';

  return url.split(slash_separator).length == 2;
}

function isUrlNonEmpty(url: string) {
  return url.length > 1;
}

function getResponse(requestUrl: string, responseStatusCode: number) {
  if (responseStatusCode != 200) {
    return errorMessage;
  } else if (requestUrl == statisticsUrl) {
    return giveStatistics();
  } else {
    return getFileContentAndUpadateFileCounter(requestUrl);
  }
}

function giveStatistics() {
  console.log(getStatisticsFromDatabase());

  return 'STATY';

}

function getFileContentAndUpadateFileCounter(fileName: string) {
  updateCounterForFileInDatabase('xd');
  return 'FILE' + fileName;
}


function createDatabase() {
  sqlite3.verbose();
  let db = new sqlite3.Database(databaseName);
  db.run('CREATE TABLE views (file VARCHAR(255), counter INT);');
  db.close();
}

function updateCounterForFileInDatabase(fileName: string) {
  sqlite3.verbose();
  let db = new sqlite3.Database(databaseName);
  db.run('INSERT INTO views (file, counter) VALUES ("a", 1), ("b",2);');
  db.close();
}

async function getStatisticsFromDatabase(): FilesStatistics[] {
  sqlite3.verbose();
  let db = new sqlite3.Database(databaseName);

  let statistics: FilesStatistics[] = [];
  db.all('SELECT file, counter FROM views;', [], (err, rows) => {
    if (err) {
      throw(err);
    }
    db.close();

    for (let {file, counter} of rows) {
      statistics.push(<FilesStatistics>{
        fileName: file,
        counter: counter
      })
    }

    console.log(statistics);
    // return statistics
  });

  return statistics
}
