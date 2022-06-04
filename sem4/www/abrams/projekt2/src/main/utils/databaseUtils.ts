import sqlite from 'sqlite3'

export function asyncDbGet(database: sqlite.Database, query: string, params?: any[]): Promise<any> {
  return new Promise((resolve, reject) => {
    database.get(query, params, (err, row)  => {
      if(err) {
        reject(new Error('Database error'));
      } else {
        resolve(row)
      }
    })
  });
}


export function asyncDbRun(database: sqlite.Database, query: string, params?: any[]): Promise<void> {
  return new Promise((resolve, reject) => {
    database.run(query, params, (err) => {
      if (err) {
        reject(new Error('Database error.'));
      } else {
        resolve();
      }
    })
  });
}


export function asyncDbAll(database: sqlite.Database, query: string, params?: any[]): Promise<any[]> {
  return new Promise((resolve, reject) => {
    database.all(query, params, (err, list) => {
      if (err) {
        reject(new Error('Database error.'));
      } else {
        resolve(list);
      }
    })
  });
}
