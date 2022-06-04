import {createServer} from 'http';

let server = createServer(
    (req, res) => {
      res.write('Ale super!');
      res.end();
    }
);

server.listen(8080);
