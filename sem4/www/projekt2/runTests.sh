#!/usr/bin/env bash

rm -r persistence
rm -r dist

npm run createdb
npm run build

node dist/src/app.js &
APP_PID=$!

npx mocha -r ts-node/register src/test/*.ts

kill ${APP_PID}


rm -r persistence
rm -r dist
